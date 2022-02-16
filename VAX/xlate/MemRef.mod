implementation module MemRef;

(*****************************************************************************
 *									     *
 *             Copyright 1984-1990 Digital Equipment Corporation             *
 *                         All Rights Reserved				     *
 *								             *
 * Permission to use, copy, and modify this software and its documentation   *
 * is hereby granted only under the following terms and conditions.  Both    *
 * the above copyright notice and this permission notice must appear in all  *
 * copies of the software, derivative works or modified versions, and any    *
 * portions thereof, and both notices must appear in supporting              *
 * documentation.							     *
 *									     *
 * Users of this software agree to the terms and conditions set forth        *
 * herein, and hereby grant back to Digital a non-exclusive, unrestricted,   *
 * royalty-free right and license under any changes, enhancements or         *
 * extensions made to the core functions of the software, including but not  *
 * limited to those affording compatibility with other hardware or software  *
 * environments, but excluding applications which incorporate this software. *
 * Users further agree to use their best efforts to return to Digital any    *
 * such changes, enhancements or extensions that they make and inform        *
 * Digital of noteworthy uses of this software.  Correspondence should be    *
 * provided to Digital at:						     *
 * 									     *
 *                       Director of Licensing				     *
 *                       Western Research Laboratory			     *
 *                       Digital Equipment Corporation			     *
 *                       100 Hamilton Avenue				     *
 *                       Palo Alto, California  94301  			     *
 * 									     *
 * This software may be distributed (but not offered for sale or transferred *
 * for compensation) to third parties, provided such third parties agree to  *
 * abide by the terms and conditions of this notice.  			     *
 * 									     *
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS    *
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED        *
 * WARRANTIES OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL    *
 * EQUIPMENT CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR     *
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF    *
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR     *
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR    *
 * PERFORMANCE OF THIS SOFTWARE.				    	     *
 *									     *
 *****************************************************************************)


from SYSTEM import MININT;

from CodeSubs import MultiWordBinOp;

from Consts import WORDSIZE, BYTESIZE, HALFSIZE;

from EES import 
    Reg, RegSet, REGTEMP, REGEES, EESKind, EESKindSet, EESElement, ees, 
    top, maxtoffset, Push, Pop, Registerize, DeRegisterize, AllocReg, 
    FreeReg, NeedDisp, DispReg, ClearAddress, SwapEES, MemTReg,
    TREGSET;

from Error import Error;

from Output import Op, S, R, C, X, L, I, Lit, LitI, Indirect, WriteName;

from Types import pcodetype, sizerange;

from Util import power, PowerOfTwo;

from Vars import curlev, curblockid;


type
    StackMemNode = pointer to StackMemNodeRec;
    StackMemNodeRec = record
	next, prev : StackMemNode;
	offset : integer;
	size : integer;
    end;
var
    stackMem : StackMemNode;


procedure AddrIsTReg(e : EESElement) : boolean;
begin
    return (ees[e].addrBlock = curblockid) and
	    (ees[e].addrOffset < maxtoffset) and (ees[e].addrMemType = 't');
end AddrIsTReg;

procedure IsOnlyBaseReg(e:EESElement) : boolean;
begin
    return (ees[e].breg <> NULLREG) and (ees[e].sreg = NULLREG)
	    and (ees[e].addrOffset = 0) and (ees[e].addrMemType = ' ')
	    and not ees[e].indirect;
end IsOnlyBaseReg;

(* DispRef:  write out display register or global data reference *)
(*  level := relative display level (0 := current)               *)
(*  if global level, then write name of global area 'xxxxxxxx'   *)
(*  if not, then write name of display register '(rN)'           *)
procedure DispRef(level:integer; mt : char);
begin
    if (level < 0) or (level > curlev) then
	Error('DispRef: bad level');
    elsif level = curlev then
	(* relative level curlev means local *)
	if mt = 'p' then
	    S('(ap)');
	else
	    S('(fp)');
	end;
    else
	(* write register name as '(rN)' where N is the  *)
	(*  display register for that level *)
	C('('); R(DispReg(level,mt)); C(')');
    end;
end DispRef;

procedure TRegOpnd(var r : Reg; e : EESElement);
var
    newr : Reg;
begin
    assert(r in TREGSET, 'r should be in rt0..rt9');
    newr := AllocReg(REGEES,e,ees[e].ptype);
    R(r); X; R(newr);
    r := newr;
end TRegOpnd;

procedure CheckTReg(var r : Reg; e : EESElement);
var
    newr : Reg;
begin
    if r in RegSet{rt0..rt9} then
	newr := AllocReg(REGEES,e,ees[e].ptype);
	Op('movl'); R(r); X; R(newr); L;
	r := newr;
    end;
end CheckTReg;

procedure GenAddress(e : EESElement);
begin
    if ees[e].addrOffset mod BYTESIZE <> 0 then
	Error('GenAddress: not on a byte boundary');
    end;
    if AddrIsTReg(e) then
	R(MemTReg(ees[e].addrOffset));
    elsif (ees[e].addrMemType = ' ') and (ees[e].addrOffset = 0) then
	(* avoid writing 0 if base reg, but write 0 if there isn't one *)
	if ees[e].breg = NULLREG then
	    C('0');
	end;
    else
	case ees[e].addrMemType of
	| 'p': C('p'); I(ees[e].addrBlock); C('+'); 
	| 't': C('t'); I(ees[e].addrBlock); C('+'); 
	| 'm': C('m'); I(ees[e].addrBlock); C('+'); 
	| 's': C('s'); I(ees[e].addrBlock); C('+'); 
	| 'k': C('k'); I(ees[e].addrBlock); C('+'); 
	| ' ': 
	| 'c':
$if vms then
	    if ((ees[e].breg <> NULLREG) or (ees[e].sreg <> NULLREG)) then
		S('L^');
	    end;
$end
	    WriteName(ees[e].addrBlock); C('+');
	end (* case *);
	I(ees[e].addrOffset div BYTESIZE);
	if ees[e].addrLevel <> 0 then
	    DispRef(ees[e].addrLevel,ees[e].addrMemType);
	end;
    end;
    if ees[e].breg <> NULLREG then
	C('('); R(ees[e].breg); C(')');
    end;
    if ees[e].sreg <> NULLREG then
	if (ees[e].breg = NULLREG) and (ees[e].addrLevel = 0) and
		(ees[e].sunits <= BYTESIZE)
	then
	    C('('); R(ees[e].sreg); C(')');
	else
	    C('['); R(ees[e].sreg); C(']');
	end;
    end;
end GenAddress;

procedure Opnd(e : EESElement);
begin
    case ees[e].kind of
    | EESDATA :
	if ees[e].indirect then
	    Error('Indirect on DATA');
	end;
	if ees[e].dreg <> NULLREG then
	    R(ees[e].dreg);
	else
	    LitI(ees[e].constInt);
	end;
    
    | EESADDR :
	if ees[e].indirect then
	    Error('Indirect on ADDR');
	end;
	GenAddress(e);
    
    | EESVAR :
	if ees[e].indirect then
	    if AddrIsTReg(e) then
		C('('); GenAddress(e); C(')'); 
	    else
		Indirect; GenAddress(e);
	    end;
	else
	    GenAddress(e);
	end;
    
    end;
end Opnd;

procedure CheckSub(e:EESElement; size:sizerange);
var
    shift : integer;
    nosub : boolean;
begin
    if ees[e].sreg <> NULLREG then
	if ees[e].sunits = size then
	    (* already fixed up *)
	elsif ees[e].sunits > size then
	    if (ees[e].sunits mod size <> 0) then
		(* incompatible subscript, make it a base register *)
		size := BYTESIZE;
		nosub := true;
	    else
		nosub := false;
	    end;
	    shift := PowerOfTwo(ees[e].sunits div size);
	    if ees[e].sreg in TREGSET then
		(* Need to use 3-address instruction via TRegOpnd *)
		if shift = 1 then
		    Op('addl3'); R(ees[e].sreg); X; TRegOpnd(ees[e].sreg,e); L;
		elsif shift > 0 then
		    Op('ashl'); LitI(shift); X; TRegOpnd(ees[e].sreg,e); L;
		else
		    Op('mull3'); LitI(ees[e].sunits div size); X;
			    TRegOpnd(ees[e].sreg,e); L;
		end;
	    else
		(* 2-address instruction okay *)
		if shift = 1 then
		    Op('addl2'); R(ees[e].sreg); X; R(ees[e].sreg); L;
		elsif shift > 0 then
		    Op('ashl'); LitI(shift); X; R(ees[e].sreg); X;
			R(ees[e].sreg); L;
		else
		    Op('mull2'); LitI(ees[e].sunits div size); X;
			R(ees[e].sreg); L;
		end;
	    end;
	    ees[e].sunits := size;
	    if nosub then
		(* incorporate into base register *)
		if ees[e].breg = NULLREG then
		    ees[e].breg := ees[e].sreg;
		    ees[e].sreg := NULLREG;
		else
		    if ees[e].breg in TREGSET then
			Op('addl3'); R(ees[e].sreg); X; TRegOpnd(ees[e].breg,e); L;
		    else
			Op('addl2'); R(ees[e].sreg); X; R(ees[e].breg); L;
		    end;
		    FreeReg(ees[e].sreg);
		end;
		ees[e].sunits := 0;
	    end;
	else
	    Error('CheckSub: Array element size too small');
	    Op('divl2'); LitI(size div ees[e].sunits); X;
		    R(ees[e].sreg); L;
	    ees[e].sunits := size;
	end;
    end;
end CheckSub;

procedure CheckRegs(e:EESElement; size:sizerange);
var
    r : Reg;
begin
    if (ees[e].addrLevel <> 0) and (ees[e].addrLevel <> curlev) then
	NeedDisp(ees[e].addrLevel,ees[e].addrMemType);
    end;
    if ees[e].sreg <> NULLREG then
	CheckSub(e,size);
	Registerize(ees[e].sreg);
    end;
    r := ees[e].sreg;
    if ees[e].breg <> NULLREG then
	if ees[e].addrLevel <> 0 then
	    (* need base reg and a display reg *)
	    if ees[e].breg in TREGSET then
		Op('addl3'); R(DispReg(ees[e].addrLevel,ees[e].addrMemType)); X;
		    TRegOpnd(ees[e].breg,e); L;
	    else
		Op('addl2'); R(DispReg(ees[e].addrLevel,ees[e].addrMemType)); X;
		    R(ees[e].breg); L;
	    end;
	    ees[e].addrLevel := 0;
	end;
	Registerize(ees[e].breg);
	if ees[e].breg = r then
	    Error('CheckRegs: stole its own reg');
	end;
    end;
end CheckRegs;

procedure AddConst(e : EESElement; r : Reg);
begin
    if ees[e].constInt = 1 then
	Op('incl'); R(r); L;
    elsif ees[e].constInt = -1 then
	Op('decl'); R(r); L;
    elsif ees[e].constInt > 0 then
	Op('addl2'); LitI(ees[e].constInt); X; R(r); L;
    elsif ees[e].constInt < 0 then
	Op('subl2'); LitI(-ees[e].constInt); X; R(r); L;
    end;
    ees[e].constInt := 0;
end AddConst;

procedure BaseReg(e : EESElement) : Reg;
begin
    if ees[e].breg <> NULLREG then
	return ees[e].breg;
    else
	return DispReg(ees[e].addrLevel,ees[e].addrMemType);
    end;
end BaseReg;

procedure MoveAddress(e : EESElement);
var
    basereg, offset : boolean;
begin
    if ees[e].indirect then
	CheckRegs(e,WORDSIZE);
	ees[e].indirect := false;
	Op('movl'); Opnd(e); X;
    else
	basereg := (ees[e].breg <> NULLREG) or (ees[e].addrLevel <> 0);
	if not basereg and (ees[e].sreg <> NULLREG) and
		(ees[e].sunits = BYTESIZE)
	then
	    ees[e].breg := ees[e].sreg;
	    ees[e].sreg := NULLREG;
	    basereg := true;
	end;
	offset := (ees[e].addrMemType <> ' ') or (ees[e].addrOffset <> 0);
	if ees[e].sreg = NULLREG then
	    CheckRegs(e,BYTESIZE);
	    if basereg then
		if offset then
		    Op('movab'); GenAddress(e); X;
		else
		    Op('movl'); R(BaseReg(e)); X;
		end;
	    else
		if offset then
		    Op('movl'); Lit; GenAddress(e); X;
		else
		    Op('clrl');
		end;
	    end;
	elsif (ees[e].sunits mod WORDSIZE) <> 0 then
	    CheckRegs(e,BYTESIZE);
	    (* byte subscripting involved *)
	    if basereg then
		if offset then
		    Op('movab'); GenAddress(e); X;
		else
		    Op('addl3'); R(BaseReg(e)); X; R(ees[e].sreg); X;
		end;
	    else
		ees[e].breg := ees[e].sreg;
		ees[e].sreg := NULLREG;
		if offset then
		    Op('movab'); GenAddress(e); X;
		else
		    Op('movl'); R(ees[e].breg); X;
		end;
	    end;
	elsif ees[e].sunits = WORDSIZE then
	    CheckRegs(e,WORDSIZE);
	    (* word subscripting involved *)
	    Op('moval'); GenAddress(e); X;
	elsif ees[e].sunits = 2*WORDSIZE then
	    CheckRegs(e,2*WORDSIZE);
	    (* double word subscripting involved *)
	    Op('movad'); GenAddress(e); X;
	else
	    (* multi-word subscripting involved *)
	    CheckRegs(e,BYTESIZE);
	    if basereg then
		if offset then
		    Op('movab'); GenAddress(e); X;
		else
		    Op('addl3'); R(BaseReg(e)); X; R(ees[e].sreg); X;
		end;
	    else
		ees[e].breg := ees[e].sreg;
		ees[e].sreg := NULLREG;
		if offset then
		    Op('movab'); GenAddress(e); X;
		else
		    Op('movl'); R(ees[e].breg); X;
		end;
	    end;
	end;
    end;
end MoveAddress;

(*
    The way this works:

    A stack element may be one of three things:
	DATA :	loaded or computed value
	ADDR :	address constant or expression
	VAR :	address constant or expression for a variable

    The common routines to process these values are:
	Eval :	convert element to single register with no constant part
		if size <= WORDSIZE, make it DATA dreg
		if size > WORDSIZE, make it MultiWordTemp
	Check :	convert element to something that can appear as an operand
		if DATA, make it constant only or dreg only
		if ADDR, make it DATA dreg
		if VAR,
		    if size isn't right, do Eval
		    if size OK, fix subscript and be sure only one base reg
	Point :	like Eval, but assumes element is address and makes a var
		do Eval, if DATA dreg, make it breg
	OffsetPoint :
		like Point, but okay to have constant offset from a register
	Store :	store one element into another
		destination must be a VAR
		if size > WORDSIZE, copy from source to destination
		if sizes compatible for single instruction, do it
		if not, Eval source, then store
		
*)

(* get value of e *)
procedure Eval(e:EESElement);
var
    r : Reg;
    bitoff, mask : integer;
begin
    case ees[e].kind of
    | EESDATA :
	CheckRegs(e,WORDSIZE);
	(* if no reg, allocate one *)
	if ees[e].dreg = NULLREG then
	    r := AllocReg(REGEES,e,ees[e].ptype);
	    Op('movl'); LitI(ees[e].constInt); X; R(r); L;
	    ees[e].dreg := r;
	else
	    AddConst(e,ees[e].dreg);
	end;
	ees[e].constInt := 0;
(* |||	ees[e].size := WORDSIZE; *)

    | EESADDR :
	if IsOnlyBaseReg(e) then
	    (* already just a base reg, just make it data reg *)
	    CheckTReg(ees[e].breg,e);
	    ees[e].dreg := ees[e].breg;
	    ees[e].breg := NULLREG;
	else
	    (* compute address, put it in data reg *)
	    r := AllocReg(REGTEMP,0,taddress);
	    MoveAddress(e); R(r); L;
	    ClearAddress(e);
	    FreeReg(ees[e].sreg);
	    ees[e].sunits := 0;
	    FreeReg(ees[e].breg);
	    ees[e].dreg := r;
	    DeRegisterize(r,e);
	end;
	ees[e].kind := EESDATA;
	ees[e].size := WORDSIZE;
	ees[e].constInt := 0;

    | EESVAR :
	(* load variable *)
	if (ees[e].size > WORDSIZE) and (ees[e].ptype <> tlongreal) then
	    MakeMultiWordTemp(e);
	else
	    r := AllocReg(REGTEMP,0,ees[e].ptype);
	    (* get value in reg if less than or equal to a word *)
	    bitoff := ees[e].addrOffset mod BYTESIZE;
	    if ees[e].ptype = tlongreal then
		CheckRegs(e,2*WORDSIZE);
		Op('movd'); Opnd(e); X; R(r); L;
	    elsif (ees[e].size = WORDSIZE) and (bitoff = 0) then
		CheckRegs(e,WORDSIZE);
		if ees[e].constInt > 0 then
		    Op('addl3'); LitI(ees[e].constInt); X;
		    ees[e].constInt := 0;
		elsif ees[e].constInt < 0 then
		    Op('subl3'); LitI(-ees[e].constInt); X;
		    ees[e].constInt := 0;
		else
		    Op('movl');
		end;
		(* ... *)	Opnd(e); X; R(r); L;
	    elsif (ees[e].size = BYTESIZE) and (bitoff = 0) then
		CheckRegs(e,BYTESIZE);
		if ees[e].ptype = tinteger then
		    Op('cvtbl');
		else
		    Op('movzbl');
		end;
		    Opnd(e); X; R(r); L;
	    elsif (ees[e].size = HALFSIZE) and (bitoff = 0) then
		CheckRegs(e,HALFSIZE);
		if ees[e].ptype = tinteger then
		    Op('cvtwl');
		else
		    Op('movzwl');
		end;
		    Opnd(e); X; R(r); L;
	    elsif (bitoff = 0) and (ees[e].sreg = NULLREG)
		and (ees[e].ptype <> tinteger)
	    then
		(* no subscripting, may be doing word operation on byte *)
		CheckRegs(e,WORDSIZE);
		mask := power(2,ees[e].size);
		if mask <> MININT then
		    mask := -mask;
		end;
		Op('bicl3'); LitI(mask); X; Opnd(e); X; R(r); L;
	    else
		ees[e].addrOffset := ees[e].addrOffset div BYTESIZE * BYTESIZE;
		CheckRegs(e,BYTESIZE); (* instruction requires byte index *)
		if ees[e].ptype = tinteger then
		    Op('extv');
		else
		    Op('extzv');
		end;
		    LitI(bitoff ); X; LitI(ees[e].size); X;
			Opnd(e); X; R(r); L;
	    end;
	    AddConst(e,r);
	    (* now a data register *)
	    ees[e].kind := EESDATA;
(* |||	    ees[e].size := WORDSIZE; *)
	    ees[e].constInt := 0;
	    ees[e].dreg := r;
	    DeRegisterize(r,e);
	    if ees[e].ptype = tlongreal then
		ees[e].size := 2*WORDSIZE;
	    else
		ees[e].size := WORDSIZE;
	    end;
	    FreeReg(ees[e].sreg);
	    ees[e].sunits := 0;
	    FreeReg(ees[e].breg);
	    ClearAddress(e);
	end;
	
    end;
end Eval;

(* ensure that e can be used as an operand *)
procedure Check(e:EESElement; size:sizerange);
begin
    case ees[e].kind of
    | EESDATA :
	if ees[e].dreg <> NULLREG then
	    if ees[e].size <> size then
		if (ees[e].size = BYTESIZE) and (size = WORDSIZE) then
		    if ees[e].ptype = tinteger then
			Op('cvtbl');
		    else
			Op('movzbl');
		    end;
		    R(ees[e].dreg); X; R(ees[e].dreg); L;
		end;
	    end;
	    AddConst(e,ees[e].dreg);
	end;
    
    | EESADDR :
	if (ees[e].breg = NULLREG) and (ees[e].sreg = NULLREG) and
	    (ees[e].addrMemType = ' ') and (ees[e].addrLevel = 0)
	    and not ees[e].indirect
	then
	    (* only constant address, convert to integer constant *)
	    ees[e].constInt := ees[e].addrOffset div BYTESIZE;
	    ees[e].kind := EESDATA;
	else
	    Eval(e);
	end;
    
    | EESVAR :
	if (ees[e].size <> size) or (ees[e].constInt <> 0) or
	    (ees[e].addrOffset mod BYTESIZE <> 0)
	then
	    Eval(e);
	else
	    CheckRegs(e,size);
	end;
	
    end;
end Check;

(* ensure that e can be used as a base address for a variable *)
procedure Point(e:EESElement);
var
    r : Reg;
begin
    if (ees[e].kind = EESVAR) and (ees[e].size > WORDSIZE) then
	if IsOnlyBaseReg(e) then
	    (* already just a base address *)
	else
	    r := AllocReg(REGTEMP,0,taddress);
	    MoveAddress(e); R(r); L;
	    (* now a base register *)
	    ees[e].kind := EESVAR;
	    ClearAddress(e);
	    FreeReg(ees[e].sreg);
	    ees[e].sunits := 0;
	    FreeReg(ees[e].breg);
	    ees[e].breg := r;
	    DeRegisterize(r,e);
	end;
    elsif (ees[e].kind = EESADDR) and AddrIsTReg(e) then
	(* address of t register is just the t reg *)
	ees[e].kind := EESVAR;
	Error('Point: check address of t reg');
    else
	Eval(e);
	if ees[e].kind = EESDATA then
	    ees[e].kind := EESVAR;
	    ees[e].breg := ees[e].dreg;
	    ees[e].dreg := NULLREG;
	    ClearAddress(e);
	end;
    end;
    Registerize(ees[e].breg);
end Point;


procedure IsBaseOffset(e : EESElement) : boolean;
begin
    with ees[e] do
	return (kind = EESVAR) and (dreg = NULLREG) and (sreg = NULLREG) and
	   ((breg = NULLREG) or (addrMemType = ' ')) and
	   not indirect and
	   ((addrLevel = 0) or (addrLevel = curlev));
	   (* Operand is offset(register) right now *)
    end;
end IsBaseOffset;

(* ensure that e can be used as a base address plus offset for a variable. *)
procedure OffsetPoint(e : EESElement);
begin
    if not IsBaseOffset(e) then
	Point(e);
    end;
end OffsetPoint;


(* ensure that e can be used as a base address for a variable.  Want address
   regardless of size of object. *)
procedure PointAt(e:EESElement);
var
    r : Reg;
begin
    if (ees[e].kind = EESVAR) then
	if IsOnlyBaseReg(e) then
	    (* already just a base address *)
	else
	    r := AllocReg(REGTEMP,0,taddress);
	    MoveAddress(e); R(r); L;
	    (* now a base register *)
	    ees[e].kind := EESVAR;
	    ClearAddress(e);
	    FreeReg(ees[e].sreg);
	    ees[e].sunits := 0;
	    FreeReg(ees[e].breg);
	    ees[e].breg := r;
	    DeRegisterize(r,e);
	end;
    else
	ees[e].kind := EESVAR;
	Error('PointAt: value not EESVAR');
    end;
    Registerize(ees[e].breg);
end PointAt;

procedure StoreWithConstWord(v,a:EESElement);
var
    k : integer;
begin
    (* important special cases *)
    k := ees[v].constInt;
    ees[v].constInt := 0;
    if (k = 0) then
	if (ees[v].kind = EESDATA) and (ees[v].dreg = NULLREG) then
	    (* clear word *)
	    Op('clrl'); Opnd(a); L;
	else
	    (* word move *)
	    Op('movl'); Opnd(v); X; Opnd(a); L;
	end;
    elsif (ees[v].kind = EESDATA) and (ees[v].dreg = NULLREG) then
	(* constant move *)
	Op('movl'); LitI(k); X; Opnd(a); L;
    elsif (ees[v].kind = EESVAR) and (ees[a].kind = EESVAR)
	    and (ees[v].indirect=ees[a].indirect)
	    and (ees[v].addrOffset=ees[a].addrOffset)
	    and (ees[v].addrBlock=ees[a].addrBlock)
	    and (ees[v].addrMemType=ees[a].addrMemType)
	    and (ees[v].addrLevel=ees[a].addrLevel)
    	    and (ees[v].sreg = NULLREG) and (ees[a].sreg = NULLREG)
    	    and (ees[v].breg = NULLREG) and (ees[a].breg = NULLREG)
    then
	(* same variable! *)
	if k = 1 then
	    Op('incl'); Opnd(v); L;
	elsif k = -1 then
	    Op('decl'); Opnd(v); L;
	elsif k > 0 then
	    Op('addl2'); LitI(k); X; Opnd(v); L;
	elsif k < 0 then
	    Op('subl2'); LitI(-k); X; Opnd(v); L;
	end;
    elsif k > 0 then
	Op('addl3'); LitI(k); X; Opnd(v); X; Opnd(a); L;
    elsif k < 0 then
	Op('subl3'); LitI(-k); X; Opnd(v); X; Opnd(a); L;
    else
	Error('StoreWithConstWord: k=0?');
    end;
end StoreWithConstWord;

procedure StoreWithConstByte(v,a:EESElement);
var
    k : integer;
begin
    (* important special cases *)
    k := ees[v].constInt;
    ees[v].constInt := 0;
    if (k = 0) then
	if (ees[v].kind = EESDATA) and (ees[v].dreg = NULLREG) then
	    (* clear byte *)
	    Op('clrb'); Opnd(a); L;
	else
	    (* byte move *)
	    Op('movb'); Opnd(v); X; Opnd(a); L;
	end;
    elsif (ees[v].kind = EESDATA) and (ees[v].dreg = NULLREG) then
	(* constant move *)
	Op('movb'); LitI(k); X; Opnd(a); L;
    elsif (ees[v].kind = EESVAR) and (ees[a].kind = EESVAR)
	    and (ees[v].indirect=ees[a].indirect)
	    and (ees[v].addrOffset=ees[a].addrOffset)
	    and (ees[v].addrBlock=ees[a].addrBlock)
	    and (ees[v].addrMemType=ees[a].addrMemType)
	    and (ees[v].addrLevel=ees[a].addrLevel)
    	    and (ees[v].sreg = NULLREG) and (ees[a].sreg = NULLREG)
    	    and (ees[v].breg = NULLREG) and (ees[a].breg = NULLREG)
    then
	(* same variable! *)
	if k = 1 then
	    Op('incb'); Opnd(v); L;
	elsif k = -1 then
	    Op('decb'); Opnd(v); L;
	elsif k > 0 then
	    Op('addb2'); LitI(k); X; Opnd(v); L;
	elsif k < 0 then
	    Op('subb2'); LitI(-k); X; Opnd(v); L;
	end;
    elsif k > 0 then
	Op('addb3'); LitI(k); X; Opnd(v); X; Opnd(a); L;
    elsif k < 0 then
	Op('subb3'); LitI(-k); X; Opnd(v); X; Opnd(a); L;
    else
	Error('StoreWithConstByte: k=0?');
    end;
end StoreWithConstByte;

procedure StoreWithConstShortWord(v,a:EESElement);
var
    k : integer;
begin
    (* important special cases *)
    k := ees[v].constInt;
    ees[v].constInt := 0;
    if (k = 0) then
	if (ees[v].kind = EESDATA) and (ees[v].dreg = NULLREG) then
	    (* clear short word *)
	    Op('clrw'); Opnd(a); L;
	else
	    (* word move *)
	    Op('movw'); Opnd(v); X; Opnd(a); L;
	end;
    elsif (ees[v].kind = EESDATA) and (ees[v].dreg = NULLREG) then
	(* constant move *)
	Op('movw'); LitI(k); X; Opnd(a); L;
    elsif (ees[v].kind = EESVAR) and (ees[a].kind = EESVAR)
	    and (ees[v].indirect=ees[a].indirect)
	    and (ees[v].addrOffset=ees[a].addrOffset)
	    and (ees[v].addrBlock=ees[a].addrBlock)
	    and (ees[v].addrMemType=ees[a].addrMemType)
	    and (ees[v].addrLevel=ees[a].addrLevel)
    	    and (ees[v].sreg = NULLREG) and (ees[a].sreg = NULLREG)
    	    and (ees[v].breg = NULLREG) and (ees[a].breg = NULLREG)
    then
	(* same variable! *)
	if k = 1 then
	    Op('incw'); Opnd(v); L;
	elsif k = -1 then
	    Op('decw'); Opnd(v); L;
	elsif k > 0 then
	    Op('addw2'); LitI(k); X; Opnd(v); L;
	elsif k < 0 then
	    Op('subw2'); LitI(-k); X; Opnd(v); L;
	end;
    elsif k > 0 then
	Op('addw3'); LitI(k); X; Opnd(v); X; Opnd(a); L;
    elsif k < 0 then
	Op('subw3'); LitI(-k); X; Opnd(v); X; Opnd(a); L;
    else
	Error('StoreWithConstShortWord: k=0?');
    end;
end StoreWithConstShortWord;

procedure Store(v,a:EESElement);
var
    bitoffa, bitoffv, bitsize : integer;
    done : boolean;
begin
    done := false;
    if ees[a].kind <> EESVAR then
	Error('Store: address not a var');
    end;
    if ees[v].size <= WORDSIZE then
	(* make sure regs are OK *)
	(* if destination is a variable, may be able to do it neatly *)
	if ees[v].kind in EESKindSet{EESVAR,EESDATA} then
	    bitoffa := ees[a].addrOffset mod BYTESIZE;
	    if ees[v].kind = EESDATA then
		bitoffv := 0;
		if ees[v].dreg = NULLREG then   (* just a constant *)
		    ees[v].size := ees[a].size;
		end;
	    else
		bitoffv := ees[v].addrOffset mod BYTESIZE;
	    end;
	    if (bitoffa <> 0) or (bitoffv <> 0) then
		(* cannot handle as special case *)
	    elsif (ees[a].size = WORDSIZE) and (ees[v].size = WORDSIZE) then
		(* handle important special cases *)
		CheckRegs(a,WORDSIZE);
		CheckRegs(v,WORDSIZE);
		StoreWithConstWord(v,a);
		done := true;
	    elsif (ees[a].size = BYTESIZE) and (ees[v].size = BYTESIZE) then
		(* handle important special cases *)
		CheckRegs(a,BYTESIZE);
		CheckRegs(v,BYTESIZE);
		StoreWithConstByte(v,a);
		done := true;
	    elsif (ees[a].size = HALFSIZE) and (ees[v].size = HALFSIZE) then
		(* another important special case *)
		CheckRegs(a,HALFSIZE);
		CheckRegs(v,HALFSIZE);
		StoreWithConstShortWord(v,a);
		done := true;
	    elsif ees[v].constInt <> 0 then
		(* cannot handle constant and different sizes, too *)
	    elsif (ees[v].size = BYTESIZE) and (ees[a].size = WORDSIZE) then
		(* byte to long, convert it *)
		CheckRegs(v,BYTESIZE);
		CheckRegs(a,WORDSIZE);
		if ees[v].ptype = tinteger then
		    Op('cvtbl');
		else
		    Op('movzbl');
		end;
		Opnd(v); X; Opnd(a); L;
		done := true;
	    elsif (ees[v].size = BYTESIZE) and (ees[a].size = HALFSIZE) then
		(* byte to short word, convert it *)
		CheckRegs(v,BYTESIZE);
		CheckRegs(a,HALFSIZE);
		if ees[v].ptype = tinteger then
		    Op('cvtbw');
		else
		    Op('movzbw');
		end;
		Opnd(v); X; Opnd(a); L;
		done := true;
	    elsif (ees[v].size = HALFSIZE) and (ees[a].size = WORDSIZE) then
		(* short word to long, convert it *)
		CheckRegs(v,HALFSIZE);
		CheckRegs(a,WORDSIZE);
		if ees[v].ptype = tinteger then
		    Op('cvtwl');
		else
		    Op('movzwl');
		end;
		Opnd(v); X; Opnd(a); L;
		done := true;
	    elsif (ees[a].size = BYTESIZE) and (ees[v].size = WORDSIZE) then
		(* long to byte, convert it *)
		CheckRegs(v,WORDSIZE);
		CheckRegs(a,BYTESIZE);
		Op('cvtlb'); Opnd(v); X; Opnd(a); L;
		done := true;
	    elsif (ees[a].size = BYTESIZE) and (ees[v].size = HALFSIZE) then
		(* short word to byte, convert it *)
		CheckRegs(v,HALFSIZE);
		CheckRegs(a,BYTESIZE);
		Op('cvtwb'); Opnd(v); X; Opnd(a); L;
		done := true;
	    elsif (ees[a].size = HALFSIZE) and (ees[v].size = WORDSIZE) then
		(* long to short word, convert it *)
		CheckRegs(v,WORDSIZE);
		CheckRegs(a,HALFSIZE);
		Op('cvtlw'); Opnd(v); X; Opnd(a); L;
		done := true;
	    end;
	elsif ees[v].kind = EESADDR then
	    (* address is always a word and word aligned *)
	    CheckRegs(a,WORDSIZE);
	    MoveAddress(v); Opnd(a); L;
	    done := true;
	end;
	if not done then
	    (* couldn't do it the easy way, evaluate source *)
	    Eval(v);
	    bitoffa := ees[a].addrOffset mod BYTESIZE;
	    if (ees[a].size = WORDSIZE) and (bitoffa = 0) then
		(* store it in a word *)
		CheckRegs(a,WORDSIZE);
		Op('movl'); Opnd(v); X; Opnd(a); L;
	    elsif (ees[a].size = BYTESIZE) and (bitoffa = 0) then
		(* store it in a byte *)
		CheckRegs(a,BYTESIZE);
		Op('cvtlb'); Opnd(v); X; Opnd(a); L;
	    elsif (ees[a].size = HALFSIZE) and (bitoffa = 0) then
		(* store it in a short word *)
		CheckRegs(a,HALFSIZE);
		Op('cvtlw'); Opnd(v); X; Opnd(a); L;
	    else
		ees[a].addrOffset := ees[a].addrOffset div BYTESIZE * BYTESIZE;
		CheckRegs(a,BYTESIZE); (* instruction requires byte index *)
		Op('insv'); Opnd(v); X; LitI(bitoffa); X; LitI(ees[a].size); X;
			Opnd(a); L;
	    end;
	end;
    elsif (ees[v].size = 2*WORDSIZE) and (ees[v].size = ees[a].size)
	    and (ees[v].kind in EESKindSet{EESVAR,EESDATA})
	    and (ees[v].sunits mod (2*WORDSIZE) = 0)
	    and (ees[a].sunits mod (2*WORDSIZE) = 0)
    then
	(* two word variable, do it neatly *)
	(* make sure regs are OK *)
	CheckRegs(v,2*WORDSIZE);
	CheckRegs(a,2*WORDSIZE);
	Op('movq'); Opnd(v); X; Opnd(a); L;
    else
	(* multi-word, do a copy *)
	Push(EESDATA);
	ees[top].size := WORDSIZE;
	ees[top].ptype := tinteger;
	ees[top].constInt := ees[v].size div WORDSIZE;
	MultiWordBinOp('movl', 'movq', v, a);
	bitsize := ees[v].size mod WORDSIZE;
	if bitsize <> 0 then
	    (* some leftovers at the end *)
	    (* make vars of right size and recurse to move it *)
	    inc(ees[v].addrOffset, ees[v].size - bitsize);
	    inc(ees[a].addrOffset, ees[v].size - bitsize);
	    ees[v].size := bitsize;
	    ees[a].size := bitsize;
	    ees[v].kind := EESVAR;
	    ees[v].kind := EESVAR;
	    Store(v,a);
	end;
    end;
end Store;

procedure ClearStack;
begin
    new(stackMem);
    stackMem^.offset := -1;
    stackMem^.size := 0;
    stackMem^.next := stackMem;
    stackMem^.prev := stackMem;
    stackMemSize := 0;
end ClearStack;

procedure RemoveFromList(sm : StackMemNode);
begin
    sm^.next^.prev := sm^.prev;
    sm^.prev^.next := sm^.next;
    dispose(sm);
end RemoveFromList;

procedure AllocStack(size : sizerange) : sizerange;
var
    sm : StackMemNode;
    found : boolean;
    offset : sizerange;
begin
    size := (size + WORDSIZE-1) div WORDSIZE;
    sm := stackMem^.next;
    found := false;
    while (sm <> stackMem) and not found do
	if size <= sm^.size then
	    found := true;
	else
	    sm := sm^.next;
	end;
    end;
    if found then
	if size = sm^.size then
	    (* take whole area *)
	    offset := sm^.offset;
	    RemoveFromList(sm);
	else
	    (* take beginning, leave rest *)
	    offset := sm^.offset;
	    sm^.offset := sm^.offset + size;
	    sm^.size := sm^.size - size;
	end;
    else
	(* need to take more space *)
	offset := stackMemSize;
	stackMemSize := stackMemSize + size;
    end;
    return offset * WORDSIZE;
end AllocStack;

procedure FreeStack(offset, size : sizerange);
var
    sm, nextsm : StackMemNode;
    found : boolean;
begin
    size := (size + WORDSIZE-1) div WORDSIZE;
    sm := stackMem^.next;
    found := false;
    while not found and (sm <> stackMem) do
	if (offset > sm^.offset) and (offset < sm^.offset+sm^.size) then
	    Error('FreeStack: stack screwed up');
	end;
	if (offset+size > sm^.offset) and (offset+size < sm^.offset+sm^.size)
	then
	    Error('FreeStack: stack screwed up');
	end;
	if (offset = sm^.offset+sm^.size) or (offset+size = sm^.offset)
		or (offset < sm^.offset)
	then
	    found := true;
	else
	    sm := sm^.next;
	end;
	if offset+size = sm^.offset then
	    (* add to beginning of area *)
	    sm^.offset := sm^.offset - size;
	    sm^.size := sm^.size + size;
	elsif offset = sm^.offset+sm^.size then
	    (* add to end of area *)
	    sm^.size := sm^.size + size;
	    (* check for plugging a hole *)
	    if sm^.offset+sm^.size = sm^.next^.offset then
		sm^.size := sm^.size + sm^.next^.size;
		RemoveFromList(sm^.next);
	    end;
	else
	    (* no neighbors, add a new node *)
	    nextsm := sm;
	    new(sm);
	    sm^.offset := offset;
	    sm^.size := size;
	    sm^.next := nextsm;
	    sm^.prev := nextsm^.prev;
	    sm^.prev^.next := sm;
	    nextsm^.prev := sm;
	end;
    end;
end FreeStack;

procedure PushMultiWordTemp(ptype:pcodetype;size : sizerange);
var
    offset : sizerange;
begin
    Push(EESVAR);
    (* allocate temp *)
    offset := AllocStack(size);
    ees[top].smemoffset := offset;
    ees[top].smemsize := size;
    ees[top].size := size;
    ees[top].ptype := ptype;

    (* make element point to temp *)
    ees[top].addrLevel := curlev;
    ees[top].addrMemType := 's';
    ees[top].addrOffset := offset;
    ees[top].addrBlock := curblockid;
end PushMultiWordTemp;

procedure MakeMultiWordTemp(e:EESElement);
begin
    if ees[e].smemsize <> 0 then
	(* already a temp *)
    else
	(* get temp *)
	PushMultiWordTemp(ees[e].ptype,ees[e].size);

	(* swap top and element *)
	SwapEES(e,top);

	(* store old element into temp *)
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := (ees[e].size + WORDSIZE-1) div WORDSIZE;
	MultiWordBinOp('movl', 'movq', top-1, e);

	(* pop element *)
	Pop(1);
    end;
end MakeMultiWordTemp;
end MemRef.
