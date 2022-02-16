implementation module CodeSubs;

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


$IF vms THEN
(* This was added by CED on 3/23/87 for long symbol names on VMS. *)
from Symbol import HashSymbol;
$END

from Consts import
    WORDSIZE, BYTESIZE, BYTESPERWORD, HALFSIZE;

from EES import 
    Reg, REGRETURN, REGEES, EESKind, EESElement, ees, top, callNest,
    Push, Pop, AllocReg, ClearDisp, ClearAddress, RestoreRegs,
    SwapEES, MemTReg, ActiveReg;

from Error import Error;

from Input import Advance, PreRead;

from MemRef import Opnd, Eval, Check, Point, PointAt, OffsetPoint,
    MakeMultiWordTemp, AddrIsTReg;

from Output import C, L, X, I, S, Op, R, Lab, LabelC, GN, LitI, TextSeg, 
    DataSeg, JumpToLabel;

from PCodeOps import
    PCodeOp;

from Types import 
    operandstring, pcodetype, OpcodesSet, LabelNumber,
    sizerange;

from Util import
    Int;

from Vars import 
    opcode, opd, opd11, nopcode, nopd, curlev;

const
    MAXSETSIZE = 1024;		(* maximum set size (for constant generation)*)
    MAXSETWORDS = 64;		(* number of 16-bit words in the largest set *)

const
    NOLOOP = 10;     (* # words must be bigger than NOLOOP for loop rather
		       than straight-line code on multiword operations *)

(* counter to generate instruction labels *)
var
    currentLabel :  LabelNumber;

procedure NewLabel(): LabelNumber;
begin
    currentLabel := currentLabel + 1;
    return currentLabel;
end NewLabel;

(* MultiWordBinOp:  top=count *)
procedure MultiWordBinOp(const opl, opq    : array of char;
			       left, right : EESElement); 
var
    lab    : LabelNumber;
    nwords : cardinal;
    quad   : boolean;
    i      : [0 .. NOLOOP-1];
    leftoff, rightoff : integer;
begin
    (* Perform a non-destructive move.  source and dest register must remain
       unchanged. *)
    if ees[top].kind <> EESDATA then
	Error('MultiWordBinOp: expected data for count');
    end;
    nwords := ees[top].constInt;

    quad := opq[0] # 0C;
    if (ees[top].dreg = NULLREG) then
	(* Number of words is a known constant *)
	
	if (nwords <= NOLOOP) or (quad and (nwords <= 2 * NOLOOP)) then
	    (* Small object: generate straight-line code *)
	    OffsetPoint(left);
	    OffsetPoint(right);
    
	    (* Save current offsets: (1) must restore offsets to original values
	       before returning; (2) left and right may be the same EESElement,
	       so must assign ees[left/right].addrOffset each iteration. *)
	    leftoff := ees[left].addrOffset;
	    rightoff := ees[right].addrOffset;
    
	    if quad then
		(* Use quad opcode *)
		for i := 0 to (nwords div 2) - 1 do
		    ees[left].addrOffset := leftoff + i * 2 * WORDSIZE;
		    ees[right].addrOffset := rightoff + i * 2 * WORDSIZE;
		    Op(opq); Opnd(left); X; Opnd(right); L;
		end;
		if odd(nwords) then
		    ees[left].addrOffset := leftoff + (nwords-1) * WORDSIZE;
		    ees[right].addrOffset := rightoff + (nwords-1) * WORDSIZE;
		    Op(opl); Opnd(left); X; Opnd(right); L;
		end;
	    else
		for i := 0 to nwords-1 do
		    ees[left].addrOffset := leftoff + i * WORDSIZE;
		    ees[right].addrOffset := rightoff + i * WORDSIZE;
		    Op(opl); Opnd(left); X; Opnd(right); L;
		end;
	    end;
	    ees[left].addrOffset := leftoff;
	    ees[right].addrOffset := rightoff;

	else
	    (* Large object of known size, generate nice fast loop *)
	    Point(left);
	    Point(right);
	    lab := NewLabel();
	    if quad then
		if odd(nwords) then
		    Op(opl); I((nwords-1)*BYTESPERWORD); Opnd(left); X;
			I((nwords-1)*BYTESPERWORD); Opnd(right); L;
		end;
		nwords := nwords div 2;
	    end;
	    ees[top].constInt := nwords - 1;
	    Eval(top);
	    LabelC(lab); if quad then Op(opq) else Op(opl) end;
		Opnd(left); C('['); Opnd(top); C(']'); X;
		Opnd(right); C('['); Opnd(top); C(']'); L;
	    Op('decl'); Opnd(top); L;
	    if quad then Op(opq) else Op(opl) end;
		Opnd(left); C('['); Opnd(top); C(']'); X;
		Opnd(right); C('['); Opnd(top); C(']'); L;
		Op('sobgtr'); Opnd(top); X; Lab(lab); L;
	    if odd(nwords) then
		if quad then Op(opq) else Op(opl) end;
		    Opnd(left); X; Opnd(right); L;
	    end;
	end;

    else
	(* Object of unknown size *)
	Point(left);
	Point(right);
	ees[top].constInt := ees[top].constInt - 1;
	Eval(top);
	lab := NewLabel();
	LabelC(lab); Op(opl); Opnd(left); C('['); Opnd(top); C(']'); X;
	    Opnd(right); C('['); Opnd(top); C(']'); L;
	Op('sobgeq'); Opnd(top); X; Lab(lab); L;
    end;
    Pop(1);
end MultiWordBinOp;

procedure Compare(ptype : pcodetype; size : sizerange);
var
    loopLabel, exitLabel, recompareLabel : LabelNumber;
    i : [0 .. NOLOOP-1];
    wordSize : sizerange;
begin
    if ptype = tlongreal then
	Check(top,2*WORDSIZE);
	Check(top-1,2*WORDSIZE);
	Op('cmpd'); Opnd(top-1); X; Opnd(top); L;
	Pop(2);
    elsif ptype = tstring then
	PushConst(tinteger,WORDSIZE,size div BYTESIZE);
	Eval(top);
	PointAt(top-1);
	PointAt(top-2);
	loopLabel := NewLabel();
	recompareLabel := NewLabel();
	exitLabel := NewLabel();
	LabelC(loopLabel);
	Op('cmpb'); Opnd(top-2); C('+'); X; Opnd(top-1); L;
	JumpToLabel('jneq', exitLabel);
	Op('tstb'); Opnd(top-1); C('+'); L;
	JumpToLabel('jeql', recompareLabel);
	Op('sobgtr'); Opnd(top); X; Lab(loopLabel); L;
	JumpToLabel('jmp', exitLabel);
	LabelC(recompareLabel);
	Op('tstb'); C('-'); Opnd(top-1); L;
	Pop(3);
	LabelC(exitLabel); L;
    elsif size > WORDSIZE then
	wordSize := size div WORDSIZE;
	Point(top);
	Point(top-1);
	exitLabel := NewLabel();
	if wordSize <= NOLOOP then
	    (* Generate straight-line code *)
	    for i := 1 to wordSize do
		Op('cmpl'); Opnd(top-1); C('+'); X; Opnd(top); C('+'); L;
		JumpToLabel('jneq', exitLabel);
	    end;
	else
	    (* Generate loop *)
	    PushConst(tinteger,WORDSIZE,wordSize);
	    Eval(top);
	    loopLabel := NewLabel();
	    LabelC(loopLabel);
	    Op('cmpl'); Opnd(top-2); C('+'); X; Opnd(top-1); C('+'); L;
	    JumpToLabel('jneq', exitLabel);
	    Op('sobgtr'); Opnd(top); X; Lab(loopLabel); L;
	    Pop(1);
	end;
	(* tricky: if size not a multiple, we are now pointing at remainder. *)
	(* make pointer into variables and fix sizes, then recurse for compare *)
	size := size mod WORDSIZE;
	if size > 0 then
	    ees[top].kind := EESVAR;
	    ees[top].size := size;
	    ees[top-1].kind := EESVAR;
	    ees[top-1].size := size;
	    Compare(ptype,size);
	else
	    Pop(2);
	end;
	LabelC(exitLabel); L;
    elsif size = BYTESIZE then
	Check(top,BYTESIZE);
	Check(top-1,BYTESIZE);
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) and
		(ees[top].constInt = 0) then
	    Op('tstb'); Opnd(top-1); L;
	else
	    Op('cmpb'); Opnd(top-1); X; Opnd(top); L;
	end;
	Pop(2);
    elsif size = HALFSIZE then
	Check(top, HALFSIZE);
	Check(top-1, HALFSIZE);
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) and
		(ees[top].constInt = 0) then
	    Op('tstw'); Opnd(top-1); L;
	else
	    Op('cmpw'); Opnd(top-1); X; Opnd(top); L;
	end;
	Pop(2);
    else
	Check(top,WORDSIZE);
	Check(top-1,WORDSIZE);
	if ptype = treal then
	    Op('cmpf'); Opnd(top-1); X; Opnd(top); L;
	elsif (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) and
	    (ees[top].constInt = 0)
	then
	    Op('tstl'); Opnd(top-1); L;
	else
	    Op('cmpl'); Opnd(top-1); X; Opnd(top); L;
	end;
	Pop(2);
    end;
end Compare;

$if unix then
procedure CallProc(op : PCodeOp; ctype : char; size : sizerange;
	numParamWords : integer; const procName : operandstring;
	procNameSize : integer);
(* procNameSize unused on Unix, and procName isn't changed either.
   We just wanted a consistent interface for both *)
$else
procedure CallProc(op : PCodeOp; ctype : char; size : sizerange;
	numParamWords : integer; const longProcName : operandstring;
	procNameSize : integer);
var procName : operandstring;
$end

var
    i : integer;
    ptype : pcodetype;
    ch : char;
begin
$if vms then
    i := 1;
    loop
	ch := longProcName[i];
	procName[i] := ch;
	if ch = 0C then exit end;
	inc(i);
    end (* loop *);
    procNameSize := i-1;
    HashSymbol(procName, procNameSize);
$end
    ptype := associatedType[ctype];
    if op = PCCIP then
	Point(top);
	Op('calls'); LitI(numParamWords); X; Opnd(top); L;
	Pop(1);
    elsif op = PCCUP then
	Op('jsb'); GN; S(procName); L;
	if numParamWords <> 0 then
	    Op('addl2'); LitI(numParamWords*4); X; R(sp); L;
	end;
    elsif op = PCXFC then
	Op('xfc'); L;
    else
	Op('calls'); LitI(numParamWords); X; GN; S(procName); L;
    end;
    ClearDisp;
    dec(callNest);
    if ctype <> 'P' then
	PushReg(ptype,size,AllocReg(REGRETURN,top+1,ptype));
	if (size > WORDSIZE) and (ptype <> tlongreal) then
	    (* reg is address of return value, make it base address *)
	    ees[top].kind := EESVAR;
	    ees[top].breg := ees[top].dreg;
	    ees[top].dreg := NULLREG;
	    if nopcode in OpcodesSet{PCSTR, PCSRO} then
		(* if next op is store, don't bother putting into a temp *)
	    else (* have to store to a temporary, yuch *)
		MakeMultiWordTemp(top);
	    end;
	end;
    end;
    RestoreRegs(top);
end CallProc;

procedure MakeBaseAddress(e:EESElement);
var
    saveoffset : integer;
begin
    case ees[e].kind of
    | EESVAR, EESDATA :
	saveoffset := ees[e].constInt * BYTESIZE;   
	ees[e].constInt := 0;
	if ees[e].kind = EESVAR then
	    if not ees[e].indirect and AddrIsTReg(e) then
		(* make it dreg: will become breg below *)
		ees[e].dreg := MemTReg(ees[e].addrOffset);
		ClearAddress(e);
	    else
		Eval(e);
	    end;
	end;
	ees[e].breg := ees[e].dreg;
	ees[e].dreg := NULLREG;
	ees[e].kind := EESADDR;
	ees[e].addrOffset := saveoffset;
    
    | EESADDR :
	
    end;
end MakeBaseAddress;

procedure MakeVariable(e:EESElement);
var
    constOffset : integer;
begin
    case ees[e].kind of
    | EESADDR :
	(* make address into variable *)
	ees[e].kind := EESVAR;
    
    | EESVAR :
	if (ees[e].sreg <> NULLREG) or (ees[e].constInt <> 0) or
		ees[e].indirect
	then
	    (* get value, make base address of variable *)
	    constOffset := ees[e].constInt * BYTESIZE;
	    ees[e].constInt := 0;
	    if not ees[e].indirect and AddrIsTReg(e) then
		(* make it dreg: will become breg below *)
		ees[e].dreg := MemTReg(ees[e].addrOffset);
		ClearAddress(e);
	    else
		Eval(e);
	    end;
	    ees[e].breg := ees[e].dreg;
	    ees[e].dreg := NULLREG;
	    ClearAddress(e);
	    ees[e].addrOffset := constOffset;
	    ees[e].kind := EESVAR;
	else
	    (* use indirect addressing (no subscripting allowed) *)
	    ees[e].indirect := true;
	end;
    
    | EESDATA :
	(* get value, make base address of variable *)
	ees[e].breg := ees[e].dreg;
	ees[e].dreg := NULLREG;
	ClearAddress(e);
	ees[e].addrOffset := ees[e].constInt * BYTESIZE;
	ees[e].kind := EESVAR;
	ees[e].constInt := 0;
	
    end;
end MakeVariable;

procedure Increment(value : integer);
begin
    (* if need to store bit offset, make into address first *)
    if (opd11 = 'a') and (value mod BYTESIZE <> 0) and
	    (ees[top].kind <> EESADDR)
    then
	MakeBaseAddress(top);
    end;
    case ees[top].kind of
    | EESDATA :
	if opd11 = 'a' then
	    (* value is in bits *)
	    ees[top].constInt := ees[top].constInt + value div BYTESIZE;
	else
	    ees[top].constInt := ees[top].constInt + value;
	end;
    
    | EESVAR :
	if ees[top].indirect then
	    Eval(top);
	end;
	if opd11 = 'a' then
	    (* value is in bits *)
	    ees[top].constInt := ees[top].constInt + value div BYTESIZE;
	else
	    ees[top].constInt := ees[top].constInt + value;
	end;
    
    | EESADDR :
	if ees[top].indirect then
	    Eval(top);
	end;
	ees[top].addrOffset := ees[top].addrOffset + value;
    
    end;
end Increment;

procedure SetConst(size : sizerange; e : EESElement);
const
    BITSPERSETWORD = 16;
var
    value: array [1..MAXSETWORDS] of integer;
    word, bit, index: integer;
    numbits : integer;
    constlab : integer;
begin
    for word := 1 to MAXSETWORDS do value[word] := 0;end;
    numbits := Int(opd^[3]);
    if numbits > MAXSETSIZE then
	Error('Too many bits in set');
    end;
    for index:=0 to numbits-1 do
	if index mod BITSPERSETWORD = 0 then bit := 1 else bit := 2 * bit;end;
	if (opd^[4][index+1]='1') then
	    value[1+index div BITSPERSETWORD] :=
			value[1+index div BITSPERSETWORD] + bit;
	end;
    end;
    constlab := currentConstant;
    currentConstant := currentConstant + 1;
    DataSeg;
    C('k'); I(constlab); C(':'); L;
    for word:=1 to (size+BITSPERSETWORD-1) div BITSPERSETWORD do
	Op('.word'); I(value[word]); L;
    end;
    TextSeg;
    ees[e].addrMemType := 'k';
    ees[e].addrBlock := constlab;
end SetConst;

(* TwoOrThree:
    if dest is data and not constant, use 2 operand form
	op2  source,dest
    if not, allocate reg, use 3 operand form, and make dest data
	op3  source,dest,r
*)
procedure TwoOrThree(op : array of char; source, dest : EESElement;
	ptype : pcodetype; size : sizerange; dostore : boolean);
var
    r : Reg;
    i : integer;
begin
    (* find N in instruction *)
    i := 1;
    while op[i] <> 'N' do
	i := i + 1;
    end;
    Check(source,size);
    Check(dest,size);
    (* look ahead to see if next thing is a store *)
    if dostore and (nopcode in OpcodesSet{PCSTR, PCSRO, PCSTN}) and
	    (* size must match, to avoid complications *)
	    (size = Int(nopd^[2])) then
	(* make store be current instruction *)
	Advance;
	PreRead;
	Push(EESVAR);
	ees[top].size := size;
	ees[top].ptype := ptype;
	(* get address *)
	if opcode = PCSRO then
	    ees[top].addrLevel := 0;
	    ees[top].addrMemType := opd^[3][1];
	    ees[top].addrOffset := Int(opd^[4]);
	    ees[top].addrBlock := Int(opd^[5]);
	else (* opcode in OpcodesSet{PCSTR,PCSTN} *)
	    ees[top].addrLevel := curlev - Int(opd^[3]);
	    ees[top].addrMemType := opd^[4][1];
	    ees[top].addrOffset := Int(opd^[5]);
	    ees[top].addrBlock := Int(opd^[6]);
	end;
	Check(top,size);
	(* Are we really storing back into dest? *)
	if      (ees[dest].kind = EESVAR) and (not ees[dest].indirect) and
		(ees[dest].sreg = NULLREG) and (ees[dest].breg = NULLREG) and
		(ees[dest].addrLevel   = ees[top].addrLevel) and
		(ees[dest].addrMemType = ees[top].addrMemType) and
		(ees[dest].addrOffset  = ees[top].addrOffset) and
		(ees[dest].addrBlock   = ees[top].addrBlock) then
	    op[i] := '2';
	    Op(op); Opnd(source); X; Opnd(top); L;
	else

	    op[i] := '3';
	    Op(op); Opnd(source); X; Opnd(dest); X; Opnd(top); L;
	end;
	if opcode = PCSTN then
	    (* as in stn: pop original and leave T on stack *)
	    SwapEES(dest,top);
	    Pop(1);
	else
	    Pop(2);	(* as in store: one for the result, one for the address *)
	end;
    elsif (ees[dest].kind = EESDATA) and ActiveReg(ees[dest].dreg) then
	(* dest is usable register, do 2 operand operation *)
	op[i] := '2';
	Op(op); Opnd(source); X; Opnd(dest); L;
    else
	r := AllocReg(REGEES,dest,ptype);
	op[i] := '3';
	Op(op); Opnd(source); X; Opnd(dest); X; R(r); L;
	(* set up value as dest *)
	Push(EESDATA);
	ees[top].size := size;
	ees[top].ptype := ptype;
	ees[top].dreg := r;
	SwapEES(dest,top);
	Pop(1);
    end;
end TwoOrThree;

procedure PushConst(ptype:pcodetype; size:sizerange;value:integer);
begin
    Push(EESDATA);
    ees[top].ptype := ptype;
    ees[top].size := size;
    ees[top].constInt := value;
end PushConst;

procedure PushReg(ptype:pcodetype; size:sizerange;dreg:Reg);
begin
    Push(EESDATA);
    ees[top].ptype := ptype;
    ees[top].size := size;
    ees[top].dreg := dreg;
end PushReg;

var tempchar : char;

begin
    currentLabel:=50001;
    currentConstant := 1;

    for tempchar:= chr(0) to chr(127) do
        associatedType[tempchar]:=tundefined;end;
    associatedType['a']:=taddress;
    associatedType['b']:=tboolean;
    associatedType['c']:=tchar;
    associatedType['i']:=tinteger;
    associatedType['j']:=tcardinal;
    associatedType['k']:=tcardinal;
    associatedType['p']:=tproc;
    associatedType['q']:=trecord;
    associatedType['r']:=treal;
    associatedType['R']:=tlongreal;
    associatedType['S']:=tset;
    associatedType['s']:=tstring;
end CodeSubs.
