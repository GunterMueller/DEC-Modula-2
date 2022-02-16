implementation module OpSubs;

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
(* This was added by CED on 3/23/87 for long symbol names on VMS.*)
from Symbol import HashSymbol;

from bitoperations import BitAnd,BitShiftRight;

from Types import operandstring;
$END

from SYSTEM import MAXINT;

from ASCII import CharSet;

from CodeSubs import
    currentConstant, NewLabel, MultiWordBinOp, Compare, CallProc, 
    Increment, SetConst, TwoOrThree, PushConst, PushReg, MakeBaseAddress, 
    associatedType, MakeVariable;

from Consts import 
    WORDSIZE, BYTESPERWORD, BYTESIZE, ADDRSIZE, BOOLSIZE, CHARSIZE, 
    HALFSIZE, LONGREALSIZE;

from EES import 
    MAXDISPLEVEL, Level, APOFF, DISPOFF, Reg, RegSet, RETURNREG, LASTREG,
    FIRSTVREG, LASTVREG, REGREG, REGSP, REGTEMP, REGEES, EESKind, EESKindSet,
    NUMTEMPS, EESElement, ees, top, maxtoffset, firsttreg, TREGSET,
    Push, Pop,
    CheckFreeReg, DumpReg, DumpEES, ClearEES, ClearReg, Registerize,
    DeRegisterize, AllocReg, FreeReg, FreeDisp, ClearDisp,
    ClearAddress, SwapEES, MoveReg, TRegReg, MemTReg, ActiveReg, SaveRegs;

from Error import Error;

from Machine import atof;

from MemRef import
    stackMemSize, Opnd, CheckRegs, CheckSub, Eval, Check, Point,
    Store, ClearStack, TRegOpnd, AddrIsTReg, MoveAddress, IsBaseOffset;

from Output import
    Op, S, R, C, X, L, I, Lab, LabelC, LabelCE, GN, Lit, LitI,
    TextSeg, DataSeg, Bssz, SOEscape, error, writelabel, JumpToLabel;

$if vms then
from Output import Comment;
$end

from PCodeOps import
    PCodeOp;

from Types import
    ShortString, pcodetype, pcodetypeset, LabelNumber, sizerange;

from Util import 
    Int, datasize, power, PowerOfTwo;

from Vars import 
    pclabel, pclbsize, opcode, opd, opdsizes, opdcount, opd11, 
    curlev, curblockid, regmask, rmemoff, dmemoff, (* ECHOPCODE, *)
    terminate, NUMCOMBLOCKS, numcomblocks, comtable, PRINTNAMES;

    (* stuff for delayed jump code *)
type
    jumpcond	    = (jcnone, jceq, jcne, jcgt, jcle, jclt, jcge,
				jcgtu, jcleu, jcltu, jcgeu);
var
    jumpopposite    : array jumpcond of jumpcond;
    jumpnames       : array jumpcond of ShortString;
    nextjump	    : jumpcond;    (* next jump to be done *)



const
(* Names of built-in routines *)
    RTDISPOSE     = 'runtime__deallocate';  (* dispose external procedure *)
    RTNEW         = 'runtime__allocate';    (* new external procedure *)
    RTMAKESET     = 'runtime__makeset';
    RTSMALLEST    = 'runtime__smallest';
    RTLONGDIV     = 'runtime__udiv';
    RTLONGMOD     = 'runtime__umod';
    RTERRORASSERT = 'runtime__errorassert';

(* Size of environment buffer used by setjmp/longjmp *)
$IF vms THEN
    SETJMPBUF       = 60;	(* typedef int jmp_buf[15] *)
$ELSE
    SETJMPBUF       = 40;	(* typedef int jmp_buf[10] *)
$END


const
    NUMCOUNTBLOCKS = 100;
    COUNTSPERBLOCK = 1000;
type
    CountBlockArray = array [0..COUNTSPERBLOCK-1] of integer;
    CountBlock = pointer to CountBlockArray;
var
    countList : array [0..NUMCOUNTBLOCKS] of CountBlock;
    countActive : boolean;
    gprof : boolean;

var
    nodisplay : boolean;
    internal : boolean;
    pmemoff, tmemoff, mmemoff, smemoff, gmemoff, arsize : integer;
    mainprogblockid : integer;

const
    NUMDATABLOCKS = 100;
    WORDSPERBLOCK = 1024;
    BYTESPERBLOCK = 4 * WORDSPERBLOCK;
    SHORTWORDSPERBLOCK = 2 * WORDSPERBLOCK;
    LONGREALSPERBLOCK = WORDSPERBLOCK div 2;
type
    InitDataBlock = pointer to InitDataBlockRec;
    InitDataBlockRec = record
	used : boolean;
	case integer of
	| 8: lrdata: @align 32 array [0..LONGREALSPERBLOCK-1] of longreal;
	| 4: wdata : @align 32 array [0..WORDSPERBLOCK-1] of integer;
	| 2: swdata: @align 32 array [0..SHORTWORDSPERBLOCK-1] of 
			@size 16 [-32768..32767];
	| 1: bdata : @align 32 array [0..BYTESPERBLOCK-1] of 
			@size 8 [-128..127];
	end;
    end;
var
    initData : array [0..NUMDATABLOCKS] of InitDataBlock;
    initInProgress : boolean;
    initSize : integer;

procedure MarkStack(numParamWords : integer);
begin
(* ||| Special case 1?  maybe 2? *)
    if numParamWords > 0 then
	Op('subl2'); Lit; I(numParamWords*BYTESPERWORD); X; R(sp); L;
    end;
end MarkStack;

procedure PushArg(e : EESElement; offset : integer);
begin
    offset := offset * BYTESPERWORD;
    if ees[e].kind = EESADDR then
	if (ees[e].sreg # NULLREG) and ((ees[e].sunits mod WORDSIZE) = 0) then
	    CheckRegs(e,WORDSIZE);
	    Op('moval'); Opnd(e); X; I(offset); S('(sp)'); L;
	else
	    CheckRegs(e,BYTESIZE);
	    Op('movab'); Opnd(e); X; I(offset); S('(sp)'); L;
	end;
    elsif ees[e].ptype = tlongreal then
	(* two-word real *)
	Check(e,2*WORDSIZE);
	Op('movq'); Opnd(e); X; I(offset); S('(sp)'); L;
    elsif (ees[e].kind = EESDATA) or (ees[e].size <= WORDSIZE) then
	(* one word *)
	Check(e,WORDSIZE);
	Op('movl'); Opnd(e); X; I(offset); S('(sp)'); L;
    else
	(* must be a multi-word quantity.  Push address *)
	if ees[e].indirect then
	    (* indirect, make it a variable address *)
	    ees[e].kind := EESVAR;
	    ees[e].ptype := taddress;
	    ees[e].size := ADDRSIZE;
	    ees[e].indirect := false;
	end;
	(* push parameter on stack *)
	Point(e);
	Op('movl'); R(ees[e].breg); X; I(offset); S('(sp)'); L;
    end;
end PushArg;

procedure doabs;
var lab : LabelNumber;
begin
    Eval(top);
    lab := NewLabel();
    if opd11 = 'r' then
	Op('tstf'); Opnd(top); L;
	JumpToLabel('jgeq', lab);
	Op('mnegf'); Opnd(top); X; Opnd(top); L;
    elsif opd11 = 'R' then
	Op('tstd'); Opnd(top); L;
	JumpToLabel('jgeq', lab);
	Op('mnegd'); Opnd(top); X; Opnd(top); L;
    else
	Op('tstl'); Opnd(top); L;
	JumpToLabel('jgeq', lab);
	Op('mnegl'); Opnd(top); X; Opnd(top); L;
    end;
    LabelC(lab); L;
end doabs;

    (* ||| Should be in doadd *)
    procedure MakeBaseRegs(const e : EESElement);
    begin
	CheckSub(e,BYTESIZE);
	if ees[e].breg = NULLREG then
	    ees[e].breg := ees[e].sreg;
	    ees[e].sreg := NULLREG;
	else
	    if ees[e].breg in TREGSET then
		Op('addl3'); R(ees[e].sreg); X; TRegOpnd(ees[e].breg,e); L;
	    else
		Op('addl2'); R(ees[e].sreg); X; R(ees[e].breg); L;
	    end;
	end;
    end MakeBaseRegs;

procedure @inline doadd;
var
    r : Reg;

begin (* doadd *)
    if opd11 = 'r' then
	TwoOrThree('addfN',top,top-1,treal,WORDSIZE,true);
    elsif opd11 = 'R' then
	TwoOrThree('adddN',top,top-1,tlongreal,2*WORDSIZE,true);
    elsif opd11 = 'a' then
	MakeBaseAddress(top);
	MakeBaseAddress(top-1);
	(* make sure subscripts are compatible *)
	if (ees[top].sreg # NULLREG) and (ees[top-1].sreg # NULLREG) then
	    if (ees[top].sunits mod ees[top-1].sunits) = 0 then
		CheckSub(top,ees[top-1].sunits);
	    elsif (ees[top-1].sunits mod ees[top].sunits) = 0 then
		CheckSub(top-1,ees[top].sunits);
	    else
		(* subscript units are incompatible, make them base regs *)
		MakeBaseRegs(top);
		MakeBaseRegs(top-1);
	    end;
	end;
	ees[top-1].addrOffset := ees[top-1].addrOffset + ees[top].addrOffset;
	if ees[top].addrMemType # ' ' then
	    if ees[top-1].addrMemType # ' ' then
		Error('doadd: add two base addresses');
	    end;
	    ees[top-1].addrMemType := ees[top].addrMemType;
	    ees[top-1].addrLevel := ees[top].addrLevel;
	    ees[top-1].addrBlock := ees[top].addrBlock;
	end;
	(* check for special case: no sreg, no constant *)
	if (ees[top].sreg = NULLREG) and (ees[top-1].sreg = NULLREG) and
	    (ees[top-1].addrMemType = ' ') and (ees[top-1].addrOffset = 0)
	    and (ees[top].breg # NULLREG) and (ees[top-1].breg # NULLREG)
	then
	    (* to use TwoOrThree, rearrange as dreg *)
	    ees[top].kind := EESDATA;
	    ees[top].dreg := ees[top].breg;
	    ees[top].breg := NULLREG;
	    ees[top-1].kind := EESDATA;
	    ees[top-1].dreg := ees[top-1].breg;
	    ees[top-1].breg := NULLREG;
	    TwoOrThree('addlN',top,top-1,taddress,WORDSIZE,true);
	else
	    if ees[top].breg # NULLREG then
		if ees[top-1].breg # NULLREG then
		    if not ActiveReg(ees[top-1].breg) and ActiveReg(ees[top].breg)
		    then
			(* switch them *)
			MoveReg(top-1,ees[top].breg);
			MoveReg(top,ees[top-1].breg);
			r := ees[top].breg;
			ees[top].breg := ees[top-1].breg;
			ees[top-1].breg := r;
		    end;
		    if ees[top-1].breg in TREGSET then
			Op('addl3'); R(ees[top].breg); X;
			    TRegOpnd(ees[top-1].breg,top-1); L;
		    else
			Op('addl2'); R(ees[top].breg); X; R(ees[top-1].breg); L;
		    end;
		else
		    ees[top-1].breg := ees[top].breg;
		    MoveReg(top-1,ees[top].breg);
		    ees[top].breg := NULLREG;
		end;
	    end;
	    if ees[top].sreg # NULLREG then
		if ees[top-1].sreg # NULLREG then
		    if not ActiveReg(ees[top-1].sreg) and ActiveReg(ees[top].sreg)
		    then
			(* switch them *)
			MoveReg(top-1,ees[top].sreg);
			MoveReg(top,ees[top-1].sreg);
			r := ees[top].sreg;
			ees[top].sreg := ees[top-1].sreg;
			ees[top-1].sreg := r;
		    end;
		    if ees[top-1].sreg in TREGSET then
			Op('addl3'); R(ees[top].sreg); X;
			    TRegOpnd(ees[top-1].sreg,top-1); L;
		    else
			Op('addl2'); R(ees[top].sreg); X; R(ees[top-1].sreg);L;
		    end;
		else
		    ees[top-1].sreg := ees[top].sreg;
		    MoveReg(top-1,ees[top].sreg);
		    ees[top].sreg := NULLREG;
		    ees[top-1].sunits := ees[top].sunits;
		end;
	    end;
	    ees[top-1].ptype := taddress;
	    ees[top-1].size := WORDSIZE;
	end;
    elsif (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG)
	and (ees[top-1].kind in EESKindSet{EESDATA,EESVAR})
    then
	(* top is constant *)
	ees[top-1].constInt := ees[top-1].constInt + ees[top].constInt;
    elsif (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG)
	and (ees[top].kind in EESKindSet{EESDATA,EESVAR})
    then
	(* top-1 is constant *)
	SwapEES(top,top-1);	(* make top constant *)
	ees[top-1].constInt := ees[top-1].constInt + ees[top].constInt;
    else
	TwoOrThree('addlN',top,top-1,tinteger,WORDSIZE,true);
    end;
    Pop(1);
end doadd;

procedure doad2;
var
    size : integer;
begin
    size := Int(opd^[2]);
    MakeVariable(top-1);
    ees[top-1].size := size;
    Check(top-1,size);
    Check(top,size);
    if opd11 = 'r' then
	if size # WORDSIZE then
	    Error('opad2: bad size');
	end;
	ees[top-1].ptype := treal;
	Op('addf2'); Opnd(top); X; Opnd(top-1); L;
    elsif opd11 = 'R' then
	if size # 2*WORDSIZE then
	    Error('opad2: bad size');
	end;
	ees[top-1].ptype := tlongreal;
	Op('addd2'); Opnd(top); X; Opnd(top-1); L;
    else
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) and
		(ees[top].constInt = 1)
	then
	    if size = WORDSIZE then
		Op('incl'); Opnd(top-1); L;
	    elsif size = HALFSIZE then
		Op('incw'); Opnd(top-1); L;
	    elsif size = BYTESIZE then
		Op('incb'); Opnd(top-1); L;
	    else
		Error('opad2: bad size');
	    end;
	else
	    if size = WORDSIZE then
		Op('addl2');
	    elsif size = HALFSIZE then
		Op('addw2');
	    elsif size = BYTESIZE then
		Op('addb2');
	    else
		Error('opad2: bad size');
	    end;
	    Opnd(top); X; Opnd(top-1); L;
	end;
    end;
    Pop(2);
end doad2;

procedure doadr;
(* Called to allow . and [] on structured function results *)
begin
    ees[top].ptype := taddress;
    ees[top].size := ADDRSIZE;
    ees[top].kind := EESADDR;
end doadr;

procedure doand;
var r : Reg;
begin
    Eval(top-1);
    Check(top,BOOLSIZE);
    Op('mcomb'); Opnd(top-1); X; Opnd(top-1); L;
    Op('bicb3'); Opnd(top-1); X; Opnd(top); X; Opnd(top-1); L;
    Pop(1);
end doand;

procedure dobgn;
var
    i : integer;
    r : Reg;
begin
    if (pclbsize=0) then Error('Missing program name');end;

    mainprogblockid := Int(opd^[2]);
    countActive := false;
    initInProgress := false;
    gprof := false;
    if opdcount >= 3 then
	countActive := opd^[3][1] = '1';
	gprof := opd^[3][2] = '1';
    end;
    Op('.globl'); GN; S('runtime__display'); L;
    for r := FIRSTVREG to LASTVREG do
	Op('.globl'); R(r); L;
    end;
$IF vms THEN
    Op('.psect'); S('$code,long,pic,nowrt,shr'); L;
    Op('.psect'); S('data,long,noexe'); L;
    (* To use libraries on VMS - you need the module name in the object file *)
    Op('.title'); S(pclabel); L;
$END
    TextSeg;
end dobgn;

procedure dobit;
var
    i, m, n : integer;
    r : Reg;
begin
    case opd11 of
    | 'a' : (* BITAND *)
	(* VAX has no direct AND operation, just
	    bicl2 : NOT opnd1 AND opnd2 => opnd2;
	    bicl3 : NOT opnd1 AND opnd2 => opnd3 *)
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    SwapEES(top, top-1);    (* Put constant on top, dest at top-1  *)
	    ees[top].constInt := - ees[top].constInt - 1;
	elsif (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	    ees[top].constInt := - ees[top].constInt - 1;
	else (* neither is constant, so runtime complement top *);
	    Eval(top);
	    Op('mcoml'); Opnd(top); X; Opnd(top); L;
	end;
	TwoOrThree('biclN', top, top-1, tinteger, WORDSIZE, true);
	Pop(1);
    
    | 'o' : (* BITOR *)
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    SwapEES(top, top-1);    (* Put constant on top *)
	end;
	TwoOrThree('bislN', top, top-1, tinteger, WORDSIZE, true);
	Pop(1);
    
    | 'x' : (* BITXOR *)
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    SwapEES(top, top-1);    (* Put constant on top *)
	end;
	TwoOrThree('xorlN', top, top-1, tinteger, WORDSIZE, true);
	Pop(1);
    
    | 'l' : (* BITSHIFTLEFT *)
	Check(top,BYTESIZE);
	Eval(top-1);
	Op('ashl'); Opnd(top); X; Opnd(top-1); X; Opnd(top-1); L;
	Pop(1);
    
    | 'r' : (* BITSHIFTRIGHT *)
	Check(top,WORDSIZE);
	Eval(top-1);
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	    Op('extzv'); LitI(ees[top].constInt); X;
		LitI(WORDSIZE-ees[top].constInt); X;
		Opnd(top-1); X; Opnd(top-1); L;
	else
	    r := AllocReg(REGTEMP,0,tinteger);
	    Op('subl3'); Opnd(top); X; LitI(WORDSIZE); X; R(r); L;
	    Op('extzv'); Opnd(top); X; R(r); X; Opnd(top-1);
		    X; Opnd(top-1); L;
	    FreeReg(r);
	end;
	Pop(1);

    | 'n' : (* BITNOT *)
	Eval(top);
	Op('mcoml'); Opnd(top); X; Opnd(top); L;
    
    | 'e' : (* BITEXTRACT *)
	(* word, first, size *)
	Eval(top-2);
	Check(top-1,WORDSIZE);
	Check(top,WORDSIZE);
	Op('extzv'); Opnd(top-1); X; Opnd(top); X; Opnd(top-2);
			    X; Opnd(top-2); L;
	Pop(2);
    
    | 'i' : (* BITINSERT *)
	(* field first size word *)
	SwapEES(top,top-3);
	(* word first size field *)
	Eval(top-3);
	Check(top-2,WORDSIZE);
	Check(top-1,WORDSIZE);
	Check(top,WORDSIZE);
	Op('insv'); Opnd(top); X; Opnd(top-2); X; Opnd(top-1);
			    X; Opnd(top-3); L;
	Pop(3);
    
    | else
	Error('dobit: unknown bit operation');
    end;
end dobit;

procedure docap;
var
    lab : LabelNumber;
begin
    Eval(top);
    lab := NewLabel();
    Op('cmpb'); Opnd(top); X; LitI(ord('a')); L;
    JumpToLabel('jlss', lab);
    Op('cmpb'); Opnd(top); X; LitI(ord('z')); L;
    JumpToLabel('jgtr', lab);
    Op('addb2'); LitI(ord('A')-ord('a')); X; Opnd(top); L;
    LabelC(lab); L;
end docap;

procedure @inline docep;
begin
    CallProc(PCCEP, opd11, Int(opd^[2]), Int(opd^[3]), opd^[4], opdsizes^[4]);
end docep;

(* ||| Should be in dochk *)
    procedure GetValue(var i : integer) : integer;
    var
	result : integer;
    begin
	result := 0;
	while opd^[4][i] in CharSet{'0'..'9'} do
	    result := result * 10 + ord(opd^[4][i]) - ord('0');
	    i := i + 1;
	end;
	return result;
    end GetValue;

procedure @inline dochk;
var
    labok, laberr, labmatch, labnext : LabelNumber;
    min, max, value, offset, size, i : integer;
    elsevariant, continue, gentest : boolean;
    op : ShortString;
    r : Reg;
begin
    case opd11 of
    | 'a', 'd' :
	labok := NewLabel();
	if opd11 = 'd' then
	    Eval(top);
	    Registerize(ees[top].dreg);
	    (* if dynarray, we have address of value. *)
	    r := AllocReg(REGREG,0,taddress);
	    Op('movl'); C('('); R(ees[top].dreg); C(')'); X; R(r); L;
	    DeRegisterize(ees[top].dreg,top);
	else
	    Eval(top);
	    Registerize(ees[top].dreg);
	    r := ees[top].dreg;
	end;
	case opd^[2][1] of
	| 'm' :
	    Op('cmpl'); R(r); X; I(-4); C('('); R(r); C(')'); L;
	    JumpToLabel('jeql', labok);
	
	| 'n' :
	    Op('tstl'); R(r); L;
	    JumpToLabel('jneq', labok);
	
	| 'p' :
	    laberr := NewLabel();
	    Op('cmpl'); R(r); X; GN; S('_maxptr'); L;
	    JumpToLabel('jgtru', laberr);
	    Op('cmpl'); R(r); X; GN; S('_minptr'); L;
	    JumpToLabel('jgequ', labok);
	    LabelCE(laberr); L;
	
	end;
	Op('pushl'); R(r); L;
	Op('calls'); LitI(1); X; GN; S('runtime__erroraddr'); L;
	LabelC(labok); L;
	if opd11 = 'd' then
	    FreeReg(r);
	else
	    DeRegisterize(r,top);
	end;
    
    | 'r','s' :
	min := Int(opd^[3]);
	max := Int(opd^[4]);
	labok := NewLabel();
	laberr := NewLabel();
	if (min = 0) and (max = MAXINT) then
	    (* special case for cardinal *)
	    gentest := true;
	    if ees[top].size # WORDSIZE then
		Eval(top);
		gentest := false;
	    elsif ees[top].kind = EESDATA then
		if (ees[top].dreg = NULLREG) or (ees[top].constInt # 0)
		then
		    Eval(top);	(* will generate condition codes *)
		    gentest := false;
		end;
	    else
		Eval(top);	(* will generate condition codes *)
		gentest := false;
	    end;
	    if gentest then
		Check(top,WORDSIZE);
		Op('tstl'); Opnd(top); L;
	    end;
	    JumpToLabel('jgeq', labok);
	else
	    Eval(top);
	    if min = 0 then
		(* treat as logical value *)
	    else
		Op('cmpl'); Opnd(top); X; LitI(min); L;
		(* if (signed!) max < min, then it's a cardinal range *)
		(*  that spans maxint.  Only case where signed fails. *)
		if max < min then
		    JumpToLabel('jlssu', laberr);
		else
		    JumpToLabel('jlss', laberr);
		end;
	    end;
	    Op('cmpl'); Opnd(top); X; LitI(max); L;
	    if min >= 0 then
		JumpToLabel('jlequ', labok);
	    else
		JumpToLabel('jleq', labok);
	    end;
	end;
	LabelCE(laberr);
	Op('pushl'); LitI(max); L;
	Op('pushl'); LitI(min); L;
	Op('pushl'); Opnd(top); L;
	if opd11 = 's' then
	    Op('calls'); LitI(3); X; GN; S('runtime__errorsubscript'); L;
	else
	    Op('calls'); LitI(3); X; GN; S('runtime__errorrange'); L;
	end;
	LabelC(labok); L;
    
    | 'A' :
	SaveRegs(top-1);
	MarkStack(1);
	PushArg(top,0);
	Pop(1);
	CallProc(PCCEP,'P',0,1,RTERRORASSERT,0);
    
    | 'p' :
	Op('calls'); LitI(0); X; GN; S('runtime__errornoreturn'); L;
	(* insert a few no-ops so dbx will print the right address *)
	Op('.byte'); I(1); X; I(1); L;
    
    | 'c' :
	Op('calls'); LitI(0); X; GN; S('runtime__errorcase'); L;
    
    | 'o' :
	(* open array subscript *)
	labok := NewLabel();
	Check(top-1,WORDSIZE);
	Check(top,WORDSIZE);
	ees[top].ptype := tcardinal;
	ees[top].size := WORDSIZE;
	Check(top,WORDSIZE);
	Op('cmpl'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jlssu', labok);
	Op('pushl'); Opnd(top); L;
	Op('pushl'); Opnd(top-1); L;
	Op('calls'); LitI(2); X; GN; S('runtime__errorsubscriptopen'); L;
	LabelC(labok); L;
	Pop(1);
    
    | 'x' :
	(* subarray *)
	labok := NewLabel();
	laberr := NewLabel();
	Eval(top-2);		(* number of values *)
	Op('tstl'); Opnd(top-2); L;
	JumpToLabel('jlss', laberr);
	Eval(top-1);		(* first subscript *)
	Op('addl2'); Opnd(top-2); X; Opnd(top-1); L;
	Check(top,WORDSIZE);	(* upper bound + 1 *)
	Op('cmpl'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jlequ', labok);
	LabelCE(laberr); L;
	Op('pushl'); Opnd(top); L;
	Op('pushl'); Opnd(top-2); L;
	Op('pushl'); Opnd(top-1); L;
	Op('calls'); LitI(3); X; GN; S('runtime__errorsubarray'); L;
	LabelC(labok); L;
	Pop(2);
    
    | 'v' :
	Point(top);
	offset := Int(opd^[2]);
	size := Int(opd^[3]);
	Push(EESDATA);
	r := AllocReg(REGEES,top,tinteger);
	ees[top].dreg := r;
	if size = WORDSIZE then
	    Op('movl'); I(offset div BYTESIZE); Opnd(top-1); X; R(r); L;
	    op := 'cmpl';
	elsif (size = BYTESIZE) and (offset mod BYTESIZE = 0) then
	    Op('movb'); I(offset div BYTESIZE); Opnd(top-1); X; R(r); L;
	    op := 'cmpb';
	elsif (size = HALFSIZE) and (offset mod BYTESIZE = 0) then
	    Op('movw'); I(offset div BYTESIZE); Opnd(top-1); X; R(r); L;
	    op := 'cmpw';
	else
	    Op('extzv'); LitI(offset); X; LitI(size); X; Opnd(top-1); X; R(r);L;
	    op := 'cmpl';
	end;
	if opd^[4][1] = '~' then
	    i := 2;
	    elsevariant := true;
	else
	    i := 1;
	    elsevariant := false;
	end;
	labmatch := NewLabel();
	repeat
	    min := GetValue(i);
	    if opd^[4][i] # ':' then
		(* Single element *)
		Op(op); Opnd(top); X; LitI(min); L;
		JumpToLabel('jeql', labmatch);
	    else
		(* Range *)
		i := i + 1;
		labnext := NewLabel();
		max := GetValue(i);
		Op(op); Opnd(top); X; LitI(min); L;
		JumpToLabel('jlss', labnext);
		Op(op); Opnd(top); X; LitI(max); L;
		JumpToLabel('jleq', labmatch);
		LabelCE(labnext); L;
		labnext := NewLabel();
	    end;
	    if opd^[4][i] = ';' then
		i := i + 1;
		continue := i <= opdsizes^[4];
	    else
		continue := false;
	    end;
	until not continue;
	if elsevariant then
	    (* for else variant, if we made it here the tag is in not in one of
	       the specified variant tag lists, so everything is okay *)
	    labnext := NewLabel();
	    JumpToLabel('jbr', labnext);
	    LabelCE(labmatch); L;
	    Op('pushl'); Opnd(top); L;
	    Op('calls'); LitI(1); X; GN; S('runtime__errorvariant'); L;
	    LabelC(labnext); L;
	else
	    (* Tag not in specified set, so error *)
	    Op('pushl'); Opnd(top); L;
	    Op('calls'); LitI(1); X; GN; S('runtime__errorvariant'); L;
	    LabelC(labmatch); L;
	end;
	Pop(1);
	(* Point converted from address to variable; switch back *)
	ees[top].kind := EESADDR;
	if ees[top].breg # NULLREG then
	    DeRegisterize(ees[top].breg,top);
	end;
    
    end;
end dochk;

procedure dochr;
begin
    ees[top].ptype := tchar;
    ees[top].size := CHARSIZE;
end dochr;

procedure docip;
begin
    CallProc(PCCIP, opd11, Int(opd^[2]), Int(opd^[3]), (*dummy parm*)opd^[1],0);
end docip;

procedure docjp;
begin
    Op('.long'); writelabel(1); L;
end docjp;

procedure docom;
var
    i, b : integer;

begin
    if opdcount # 4 then
	error(1);
    end;
    if initInProgress then
	Error('Initialization error: still in progress');
    end;
    comtable[numcomblocks].block := Int(opd^[1]);
$if vms then
    HashSymbol(opd^[2], opdsizes^[2]);
$end
    for i := 1 to opdsizes^[2] do
	comtable[numcomblocks].name[i] := opd^[2][i];
    end;
    comtable[numcomblocks].name[opdsizes^[2]+1] := 0C;
    initInProgress := false;
    if (opd^[4][1] = 'i') or (opd^[4][1] = 'I') then
	if opd^[4][1] = 'i' then
	    Op('.globl'); GN; S(comtable[numcomblocks].name); L;
	else
	    (* private data, don't generate global linker symbol *)
	end;
	DataSeg;
	GN; S(comtable[numcomblocks].name); C(':'); L;
	initInProgress := true;
	for b := 0 to NUMDATABLOCKS do
	    if initData[b] = nil then
	    elsif initData[b]^.used then
		for i := 0 to WORDSPERBLOCK-1 do
		    initData[b]^.wdata[i] := 0;
		end;
		initData[b]^.used := false;
	    end;
	end;
	initSize := Int(opd^[3]);
    elsif opd^[4][1] = 'd' then
$if vms then
	Op('.globl'); GN; S(comtable[numcomblocks].name); L;
	DataSeg;
	GN; S(comtable[numcomblocks].name); C(':'); Bssz;
			    I((Int(opd^[3])) div BYTESIZE); L;
	TextSeg;
$else
	Op('.comm'); GN; S(comtable[numcomblocks].name); X;
			    I((Int(opd^[3])) div BYTESIZE); L;
$end
    else
	Op('.globl'); GN; S(comtable[numcomblocks].name); L;
    end;
    if numcomblocks < NUMCOMBLOCKS then
	numcomblocks := numcomblocks + 1;
    end;
end docom;

procedure InitWord(offset : integer; value : integer);
var
    i, j, w, b : integer;
begin
    w := offset div WORDSIZE;
    b := w div WORDSPERBLOCK;
    j := w - b * WORDSPERBLOCK;
    if initData[b] = nil then
	new(initData[b]);
	initData[b]^.used := false;
	for i := 0 to WORDSPERBLOCK-1 do
	    initData[b]^.wdata[i] := 0;
	end;
    end;
    initData[b]^.wdata[j] := value;
    initData[b]^.used := true;
end InitWord;

procedure InitByte(offset : integer; value : integer);
var
    i, j, w, b : integer;
begin
    w := offset div BYTESIZE;
    b := w div BYTESPERBLOCK;
    j := w - b * BYTESPERBLOCK;
    if initData[b] = nil then
	new(initData[b]);
	initData[b]^.used := false;
	for i := 0 to WORDSPERBLOCK-1 do
	    initData[b]^.wdata[i] := 0;
	end;
    end;
    initData[b]^.bdata[j] := value;
    initData[b]^.used := true;
end InitByte;

procedure InitShortWord(offset : integer; value : integer);
var
    i, j, w, b : integer;
begin
    w := offset div HALFSIZE;
    b := w div SHORTWORDSPERBLOCK;
    j := w - b * SHORTWORDSPERBLOCK;
    if initData[b] = nil then
	new(initData[b]);
	initData[b]^.used := false;
	for i := 0 to WORDSPERBLOCK-1 do
	    initData[b]^.wdata[i] := 0;
	end;
    end;
    initData[b]^.swdata[j] := value;
    initData[b]^.used := true;
end InitShortWord;

procedure InitLongReal(offset : integer; value : longreal);
var
    i, j, w, b : integer;
begin
    w := offset div LONGREALSIZE;
    b := w div LONGREALSPERBLOCK;
    j := w - b * LONGREALSPERBLOCK;
    if initData[b] = nil then
	new(initData[b]);
	initData[b]^.used := false;
	for i := 0 to WORDSPERBLOCK-1 do
	    initData[b]^.wdata[i] := 0;
	end;
    end;
    initData[b]^.lrdata[j] := value;
    initData[b]^.used := true;
end InitLongReal;

procedure doini;
var
    offset, size, value, i, j, w, b : integer;
    lrvalue : longreal;
begin
     offset := Int(opd^[1]);
     if not initInProgress then
	Error('Initialization error: not in progress');
     end;
     if offset < 0 then
	initInProgress := false;
	w := (initSize + WORDSIZE-1) div WORDSIZE;
	for b := 0 to (w div WORDSPERBLOCK) - 1 do
	    if initData[b] = nil then
		Bssz; I(WORDSPERBLOCK*BYTESPERWORD); L;
	    else
		for i := 0 to WORDSPERBLOCK-1 do
		    Op('.long'); I(initData[b]^.wdata[i]); L;
		end;
	    end;
	end;
	b := w div WORDSPERBLOCK;
	j := w - b * WORDSPERBLOCK;
	if j = 0 then
	elsif initData[b] = nil then
	    Bssz; I(j*BYTESPERWORD); L;
	else
	    for i := 0 to j-1 do
		Op('.long'); I(initData[b]^.wdata[i]); L;
	    end;
	end;
	TextSeg;
    else
	size := Int(opd^[2]);
	case opd^[3][1] of
	| 'a', 'b', 'c', 'i', 'j', 'k':
	    value := Int(opd^[4]);
	    if (size = WORDSIZE) and (offset mod WORDSIZE = 0) then
		InitWord(offset,value);
	    elsif (size = BYTESIZE) and (offset mod BYTESIZE = 0) then
		InitByte(offset,value);
	    elsif (size = HALFSIZE) and (offset mod HALFSIZE = 0) then
		InitShortWord(offset, value);
	    else
		Error('Bad initial value size or alignment');
	    end;
	
	| 'n', 'p', 'S' :
	    Error('Unimplemented initial value');
	
	| 'R' :
	    lrvalue := atof(opd^[4]);
	    if (size = LONGREALSIZE) and (offset mod WORDSIZE = 0) then
		InitLongReal(offset, lrvalue);
	    else
		Error('Bad initial value size or alignment');
	    end;

	| 'r':
	    value := integer(float(atof(opd^[4])));
	    if (size = WORDSIZE) and (offset mod WORDSIZE = 0) then
		InitWord(offset,value);
	    else
		Error('Bad initial value size or alignment');
	    end;
	
	| 's':
	    for i := 1 to opdsizes^[4] do
		InitByte(offset,ord(opd^[4][i]));
		offset := offset + BYTESIZE;
	    end;
	    
	end;
    end;
end doini;

procedure docts;
var
    num, counter, line, block, entry, i : integer;
begin
    if opd11 = 'c' then
	counter := Int(opd^[2]);
	line := Int(opd^[3]);
	block := counter div COUNTSPERBLOCK;
	entry := counter mod COUNTSPERBLOCK;
	if countList[block] = nil then
	    new(countList[block]);
	    for i := 0 to COUNTSPERBLOCK-1 do
		countList[block]^[i] := 0;
	    end;
	end;
	countList[block]^[entry] := line;
	Op('incl'); S('countTable+'); I(counter*4); L;
    elsif opd11 = 'd' then
	DataSeg;
	num := Int(opd^[2]);
	S('countName:'); Op('.asciz'); C('"'); S(opd^[3]); C('"'); L;
	Op('.align'); I(2); L;
	S('countHeader:'); Op('.long'); I(0); X; S('countName'); X; I(num); X;
			S('countTable'); X; S('countLines'); L;
	S('countTable:'); Bssz; I(4*(num+1)); L;
	S('countLines:'); L;
	for i := 0 to num do
	    Op('.long');
		I(countList[i div COUNTSPERBLOCK]^[i mod COUNTSPERBLOCK]); L;
	end;
	TextSeg;
    elsif opd11 = 'i' then
	Op('pushab'); S('countHeader'); L;
	Op('calls'); LitI(1); X; GN; S('runtime__countlist'); L;
    else
	Error('docts: bad option');
    end;
end docts;

procedure docup;
begin
    CallProc(PCCUP, opd11, Int(opd^[2]), Int(opd^[3]), opd^[4], opdsizes^[4]);
end docup;

procedure dodbg;
begin
    if opd11 = 's' then
	DumpEES;
    elsif opd11 = 'r' then
	DumpReg;
    else
	DumpReg;
	DumpEES;
    end;
end dodbg; 

procedure dodec;
begin
    if opdcount#2 then error(9);end;
    Increment(-Int(opd^[2]));
end dodec;

procedure dodef;
var nump, numt, numm : integer;
    rbit, i : integer;
    r : Reg;
    defblk : integer;
begin
    (* define offsets for activation record
	offsets are negative from fp
	first two words are for the old display and the current ap
     *  tNNN = -8-numt*4		offset of generally temporaries
     *  mNNN = tNNN-numm*4		offset of most local variables
     *  pNNN = 4			skip count word at start of list
     *  vNNN = save mask for registers used as T storage

     See dosst, dorst for use of g offsets
     *  gNNN = mNNN			no out-of-block targets in proc
     *  gNNN = mNNN-(MAXDISPLEVEL+1)-SETJMPBUF
	(Pascal only)			an out-of-block target in proc

     The following have been moved to doexi so def can be earlier
     *  sNNN = gNNN-stackMemSize*4	offset of pcode translator temporaries
     *  aNNN = -sNNN			total activation record size
     *)
    if opdcount#5 then error(9);end;
    nump := (Int(opd^[1]) + WORDSIZE-1) div WORDSIZE;
    numt := (Int(opd^[2]) + WORDSIZE-1) div WORDSIZE;
    numm := (Int(opd^[3]) + WORDSIZE-1) div WORDSIZE;
    defblk := Int(opd^[4]);
    if defblk # curblockid then
	Error('Unexpected block on def');
    end;
    tmemoff := -8 - numt*4;
    mmemoff := tmemoff - numm*4;
    gmemoff := mmemoff;
    pmemoff := 4;
    rmemoff := 0;
    dmemoff := 0;

    (* discourage use of these before they are defined *)
    smemoff := -maxint-1;
    arsize := -maxint-1;
    regmask := 0;
    rbit := 1;
    for r := r1 to firsttreg do
	rbit := rbit * 2;
    end;
    for i := 1 to maxtoffset div WORDSIZE do
	if i <= numt then
	    regmask := regmask + rbit;
	end;
	rbit := rbit * 2;
    end;
$if vms then
    C('t'); I(defblk); Op('= '); I(tmemoff); L;
    C('m'); I(defblk); Op('= '); I(mmemoff); L;
    C('p'); I(defblk); Op('= '); I(pmemoff); L;
$else
    Op('.set'); C('t'); I(defblk); X; I(tmemoff); L;
    Op('.set'); C('m'); I(defblk); X; I(mmemoff); L;
    Op('.set'); C('p'); I(defblk); X; I(pmemoff); L;
$end;
end dodef;

procedure dodif;
var
    wordsize : sizerange;
begin
    if ees[top].size <= WORDSIZE then
	TwoOrThree('biclN',top,top-1,tset,WORDSIZE,true);
    else
	wordsize := (ees[top].size + WORDSIZE-1) div WORDSIZE;
	Eval(top-1);
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := wordsize;
	MultiWordBinOp('bicl2', '', top-1, top-2);
    end;
    Pop(1);
end dodif;

procedure dodiv;
var
    k : integer;
    done : boolean;
begin
    if opd11 = 'r' then
	TwoOrThree('divfN',top,top-1,treal,WORDSIZE,true);
	Pop(1);
    elsif opd11 = 'R' then
	TwoOrThree('divdN',top,top-1,tlongreal,2*WORDSIZE,true);
	Pop(1);
    elsif opd11 in CharSet{'a','j','k'} then
	done := false;
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	    (* divide by constant *)
	    if ees[top].constInt = 1 then
		(* do nothing *)
		done := true;
		Pop(1);
	    else
		k := PowerOfTwo(ees[top].constInt);
		if k > 0 then
		    Eval(top-1);
		    (* first clear low order bits that will be rotated around *)
		    (* (because value is a power of 2, value-1 is correct mask) *)
		    Op('bicl2'); LitI(ees[top].constInt-1); X;
				    Opnd(top-1); L;
		    Op('rotl'); LitI(-k); X; Opnd(top-1); X;
			Opnd(top-1); L;
		    done := true;
		    Pop(1);
		end;
	    end;
	end;
	if not done then
	    if opd11 = 'k' then
		(* can do as signed *)
		TwoOrThree('divlN',top,top-1,tinteger,WORDSIZE,true);
		Pop(1);
	    else
		(* unsigned, must call subroutine *)
		SaveRegs(top-2);
		MarkStack(2);
		PushArg(top-1,0);
		PushArg(top,1);
		Pop(2);
		CallProc(PCCEP,'j',WORDSIZE,2,RTLONGDIV,0);
	    end;
	end;
    else
	TwoOrThree('divlN',top,top-1,tinteger,WORDSIZE,true);
	Pop(1);
    end;
end dodiv;

procedure dodv2;
begin
    Error('dv2 operation not implemented');
end dodv2;

procedure doend;
begin
    if (opdcount#0) then error(9);end;
    if (top#0) then error(24);end;
    terminate := true;
$if vms then
    Op('.end');
    if mainprogblockid # 0 then
	S('main');	(* start at main *)
    end;
    L;
$end;
end doend;

(*
    Note: there are two kinds of linkage -- external (CALLS) and internal (JSB)
    The following shows what a stack frame looks like on internal call
    Attempts are made to make the stack frame look like CALLS frame for
	debugging purposes.
    
		Param 3  parameters		^
		Param 2				| higher addresses are up    |
		Param 1					    stack grows down V
	ap ->	Return address	(occupies parameter count position)
		Saved old fp
		Saved old ap
		Unused (occupies CALLS save mask position)
	fp ->	Unused (occupies CALLS condition handler pointer position)
		Saved new ap (for use in display)
		Saved old display register 
		T Memory	\
		M Memory	|- activation record
		 etc.		/
	sp1 ->	Last word of activation record
		Saved regs r11-r6 if necessary
	sp2 ->	Last saved reg
		Dynamic area for large value parameters
	sp3 ->	Last word of dynamic area

    Note:  sp will be left pointing at either sp1, sp2, or sp3 depending on
	whether or not there are any saved regs or dynamic area.
    
    For completeness, here is how CALLS leaves the stack (notice the family
	resemblance with the above)
		Param 3  parameters		^
		Param 2				| higher addresses are up    |
		Param 1					    stack grows down V
	ap ->	Number of parameter words
		Saved regs r11-r6 if necessary
		Return address
		Saved old fp
		Saved old ap
		Saved register bits and interrupt mask
	fp ->	Condition handler pointer
		Saved new ap (for use in display)
		Saved old display register 
		T Memory	\
		M Memory	|- activation record
		 etc.		/
	sp1 ->	Last word of activation record
		Dynamic area for large value parameters
	sp3 ->	Last word of dynamic area

    With CALLS, there won't be any registers saved down here.
*)
procedure doent;
const
    STABLINE = 68;
var i, j, parmcnt, linenum, lab : integer;
    r : Reg;
begin
    if (opdcount<6) then error(9);end;
    if (pclbsize=0) then error(19);end;

$IF vms THEN
    HashSymbol(pclabel,pclbsize);
$END

    curlev := Int(opd^[3]);
    curblockid := Int(opd^[4]);
    parmcnt := Int(opd^[5]);
    linenum := Int(opd^[6]);
    if opdcount >= 7 then
	nodisplay := opd^[7][1] = '1';
	internal := opd^[7][2] = '1';
    else
	nodisplay := false;
	internal := false;
    end;
    maxtoffset := -1;
    firsttreg := NULLREG;
    if opdcount >= 8 then
	i := Int(opd^[8]);
	if i > 6 then
	    i := 6;
	end;
	if i > 0 then
	    maxtoffset := i * WORDSIZE;
	    firsttreg := LASTREG;
	    dec(firsttreg, i-1);
	end;
    end;
    if (curlev>MAXDISPLEVEL) then Error('doent: display level too large');end;
    ClearStack;
    if PRINTNAMES then
	DataSeg;
	S('name'); I(curblockid); C(':'); Op('.asciz');
		C('"'); S(pclabel); C('"'); L;
	TextSeg;
    end;
    Op('.align'); I(2); L;
    Op('.globl'); GN; S(pclabel); L;
    GN; S(pclabel); C(':'); L;
    if not internal then
	Op('.word'); C('v'); I(curblockid); L;
    else
	(* internal entry sequence *)
	Op('subl2'); LitI(16); X; R(sp); L;
	Op('movl'); R(fp); X; S('12(sp)'); L;
	Op('movl'); R(ap); X; S('8(sp)'); L;
	Op('addl3'); LitI(16); X; R(sp); X; R(ap); L;
	Op('movl'); R(sp); X; R(fp); L;
    end;
    if linenum # 0 then
	(* There is no .stabd directive on VMS - CED 4/3/87 *)
$IF vms THEN
	Comment;	(* Make the .stabd directive a comment - CED 9/29/87 *)
$END
	Op('.stabd'); I(STABLINE); X; I(0); X; I(linenum); L;
    end;
    if (curblockid = mainprogblockid) then
$if VMS then
	Op('subl2'); LitI(4); X; R(sp); L;
        Op('jsb'); S('c$main_args'); L;
$end
	(* Push argv and argc for init *)
	Op('pushl'); S('12(ap)'); L;
	Op('pushl'); S('8(ap)'); L;
	Op('pushl'); S('4(ap)'); L;
	Op('calls'); LitI(3); X; GN; S('runtime__init'); L;
    end;
    if not nodisplay then
	(* save ap and old display *)
	Op('movl'); R(ap); X; I(APOFF); S('(fp)'); L;
	Op('movl'); GN; S('runtime__display+'); I(curlev*4); X;
			I(DISPOFF); S('(fp)'); L;
	(* set up new display *)
	Op('movl'); R(fp); X; GN; S('runtime__display+'); I(curlev*4); L;
    end;
    (* allocate activation record *)
    Op('subl3'); Lit; C('a'); I(curblockid); X; R(fp); X; R(sp); L;
    if internal then
	(* Ultrix uses r0-r5 for local scratch values, as does the p-code
	   translator.  r6-r11 are allocated to variables, and we already
	   know how many of these we are using, so we can push them
	   explicitly (faster then pushr, we hope).
	    
	   VMS use r0-r1 for scratch, and r2-r11 must be saved.  Since we
	   don't know yet what we are using, we save them via a mask that
	   we spit out later.
	*)
$IF vms THEN
	Op('pushr'); Lit; C('v'); I(curblockid); L;
$ELSE
	if firsttreg # NULLREG then
	    for r := firsttreg to LASTREG do
		Op('pushl'); R(r); L;
	    end;
	end;
$END
    end;
    if PRINTNAMES then
	Op('pushal'); S('name'); I(curblockid); L;
	Op('calls'); LitI(1); X; GN; S('runtime__trace'); L;
    end;
    if gprof then
	lab := NewLabel();
	Op('movab'); Lab(lab); X; R(r0); L;
	Op('jsb'); S('mcount'); L;
	DataSeg;
	Op('.align'); I(2); L;
	LabelC(lab); Op('.long'); I(0); L;
	TextSeg;
    end;
    ClearReg;
end doent;

procedure doequ;
begin
    Compare(associatedType[opd11],Int(opd^[2]));
    nextjump := jceq;       (* set up to do eq jump *)
end doequ;

procedure doexi;
$IF vms THEN
var
    regoff, i, mask : integer;
$END
begin
    (* define offsets for activation record
	see dodef for explanation
    *)
    smemoff := gmemoff - 4*stackMemSize;
    rmemoff := smemoff - 4*rmemoff;
    dmemoff := rmemoff - 8*dmemoff;
    arsize := -dmemoff;
$if VMS then
    regoff := 0;		    (* assume no registers are used *)
    mask := regmask;		    (* this mask contains reg.s used *)
    for i := 0 to 11 do		    (* regmask includes r0..r11 *)
	if BitAnd(mask,1) # 0 then  (* the lowest bit is on *)
	    inc(regoff);	    (* another reg. used - count it *)
	end;
	mask := BitShiftRight(mask,1);  (* look at the next bit *)
    end;
    regoff := regoff * 4;	    (* each register takes 4 bytes *)
    C('o'); I(curblockid); Op('= '); I(regoff); L;
    if gmemoff # mmemoff then
	C('g'); I(curblockid); Op('= '); I(gmemoff); L;
    end;
    C('s'); I(curblockid); Op('= '); I(smemoff); L;
    C('r'); I(curblockid); Op('= '); I(rmemoff); L;
    C('d'); I(curblockid); Op('= '); I(dmemoff); L;
    C('a'); I(curblockid); Op('= '); I(arsize); L;
    C('v'); I(curblockid); Op('= '); I(regmask); L;
$else
    if gmemoff # mmemoff then
	Op('.set'); C('g'); I(curblockid); X; I(gmemoff); L;
    end;
    Op('.set'); C('s'); I(curblockid); X; I(smemoff); L;
    Op('.set'); C('r'); I(curblockid); X; I(rmemoff); L;
    Op('.set'); C('d'); I(curblockid); X; I(dmemoff); L;
    Op('.set'); C('a'); I(curblockid); X; I(arsize); L;
    Op('.set'); C('v'); I(curblockid); X; I(regmask); L;
$end
    CheckFreeReg;
end doexi;

procedure @inline dofjp;
begin
    if (ees[top].ptype#tboolean) then error(2);end;
    if (opdcount#1) then error(9);end;

    Check(top,BOOLSIZE);
    Op('bitb'); LitI(1); X; Opnd(top); L;
    Op('jeql'); writelabel(1); L;
    Pop(1);
end dofjp;

procedure doflt;
var
    r : Reg;
    op : ShortString;
begin
    if not (ees[top].ptype in pcodetypeset{tinteger, tcardinal, treal, tlongreal})
	then error(2);end;
    Check(top,WORDSIZE);
    if opd11 = 'r' then
	if ees[top].ptype in pcodetypeset{tinteger,tcardinal} then
	    op := 'cvtlf';
	elsif ees[top].ptype in pcodetypeset{treal,tlongreal} then
	    op := 'movl';	(* real - no op, long real - ignore second word *)
	else
	    Error('doflt: unexpected type');
	end;
	if (ees[top].kind = EESDATA) and (ees[top].dreg # NULLREG) then
	    Op(op); Opnd(top); X; Opnd(top); L;
	else
	    r := AllocReg(REGEES,top,treal);
	    Op(op); Opnd(top); X; R(r); L;
	    Pop(1);
	    Push(EESDATA);
	    ees[top].dreg := r;
	end;
	ees[top].ptype := treal;
	ees[top].size := WORDSIZE;
    elsif opd11 = 'R' then
	if ees[top].ptype in pcodetypeset{tinteger,tcardinal} then
	    op := 'cvtld';
	elsif ees[top].ptype = treal then
	    op := 'cvtfd';
	else
	    op := 'movd';	(* longreal - no op *)
	end;
	r := AllocReg(REGEES,top,tlongreal);
	Op(op); Opnd(top); X; R(r); L;
	Pop(1);
	Push(EESDATA);
	ees[top].ptype := tlongreal;
	ees[top].size := 2*WORDSIZE;
	ees[top].dreg := r;
    else
	Error('doflt: not r or R');
    end;
end doflt;

procedure dofor;
var
    size    : integer;
    ptype   : pcodetype;
    r       : Reg;
begin
    ptype := associatedType[opd11];
    size := Int(opd^[2]);
    MakeVariable(top-2);	(* loop index *)
    ees[top-2].size := size;
    ees[top-2].ptype := ptype;
    Check(top-2,size);
    Check(top,WORDSIZE);	(* limit value *)
    if (ees[top-1].kind = EESADDR) and (ptype = taddress) then
	if (ees[top-1].addrMemType # ' ') or (ees[top-1].breg # NULLREG)
	    or (ees[top-1].sreg # NULLREG)
	    or (ees[top-1].addrOffset mod BYTESIZE # 0) then
	    Error('dofor: non-constant address increment');
	else
	    ees[top-1].kind := EESDATA;
	    ees[top-1].constInt := ees[top-1].addrOffset div BYTESIZE;
	    ClearAddress(top-1);
	end;
    end;
    Check(top-1,size);		(* increment *)
    if (ees[top-1].kind # EESDATA) or (ees[top-1].dreg # NULLREG) then
	if size # WORDSIZE then
	    Error('Variable for increment not wordsize');
	end;
	r := AllocReg(REGTEMP,0,tinteger);
	Op('addl2'); Opnd(top-1); X; Opnd(top-2); L;
	Op('subl3'); Opnd(top-2); X; Opnd(top); X; R(r); L;
	Op('xorl2'); Opnd(top-1); X; R(r); L;
	Op('jgeq'); writelabel(3); L;
	FreeReg(r);
    elsif size = WORDSIZE then
	(* All instruction sequences optimized for MicroVAX II *)
	if ees[top-1].constInt < 0 then
	    if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG)
			and (ees[top].constInt = 0) then
		if (ees[top-1].constInt = -1) then
		    (* sob decrements by 1, ends at 0 
		       ||| byte displacement field, so not used
		    Op('sobgeq'); Opnd(top-2); X; writelabel(3); L;
		    *)
		    Op('decl'); Opnd(top-2); L;
		    Op('jgeq'); writelabel(3); L;
		else
		    ees[top-1].constInt := -ees[top-1].constInt;
		    Op('subl2'); Opnd(top-1); X; Opnd(top-2); L;
		    Op('jgeq'); writelabel(3); L;
		end;
	    else
		if (ees[top-1].constInt = -1) then
		    Op('decl'); Opnd(top-2); L;
		else
		    ees[top-1].constInt := -ees[top-1].constInt;
		    Op('subl2'); Opnd(top-1); X; Opnd(top-2); L;
		end;
		Op('cmpl'); Opnd(top-2); X; Opnd(top); L;
		Op('jgeq'); writelabel(3); L;
	    end;
	else
	    if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG)
			and (ees[top].constInt = 0) then
	        if ees[top-1].constInt = 1 then
		    (* incl, jleq better than aobleq! *)
		    Op('incl'); Opnd(top-2); L;
		else
		    Op('addl2'); Opnd(top-1); X; Opnd(top-2); L;
		end;
		Op('jleq'); writelabel(3); L;
	    else
	        if ees[top-1].constInt = 1 then
		    (* ||| byte displacement, so not used
		    Op('aobleq'); Opnd(top); X; Opnd(top-2); X; writelabel(3);L;
		    *)
		    Op('incl'); Opnd(top-2); L;
		    Op('cmpl'); Opnd(top-2); X; Opnd(top); L;
		    Op('jleq'); writelabel(3); L;
		else
		    Op('addl2'); Opnd(top-1); X; Opnd(top-2); L;
		    Op('cmpl'); Opnd(top-2); X; Opnd(top); L;
		    Op('jleq'); writelabel(3); L;
		end;
	    end;
	end;
	(**** slower! should be generated only for variable increments
	    Op('acbl'); Opnd(top); X; Opnd(top-1); X; Opnd(top-2); X; 
			    writelabel(3); L;
	****)
    elsif size = BYTESIZE then
	(* do byte arithmetic as word to avoid overflow problems *)
	r := AllocReg(REGTEMP,0,tinteger);
	if ptype in pcodetypeset{tcardinal,tchar} then
	    Op('movzbl'); Opnd(top-2); X; R(r); L;
	else
	    Op('cvtbl'); Opnd(top-2); X; R(r); L;
	end;
	Op('addl2'); Opnd(top-1); X; R(r); L;
	Op('movb'); R(r); X; Opnd(top-2); L;
	Op('cmpl'); R(r); X; Opnd(top); L;
	if ees[top-1].constInt < 0 then
	    Op('jgequ'); writelabel(3); L;
	else
	    Op('jlequ'); writelabel(3); L;
	end;
	FreeReg(r);
    elsif size = HALFSIZE then
	(* do shortword arithmetic as word to avoid overflow problems *)
	r := AllocReg(REGTEMP,0,tinteger);
	if ptype in pcodetypeset{tcardinal,tchar} then
	    Op('movzwl'); Opnd(top-2); X; R(r); L;
	else
	    Op('cvtwl'); Opnd(top-2); X; R(r); L;
	end;
	Op('addl2'); Opnd(top-1); X; R(r); L;
	Op('movw'); R(r); X; Opnd(top-2); L;
	Op('cmpl'); R(r); X; Opnd(top); L;
	if ees[top-1].constInt < 0 then
	    Op('jgequ'); writelabel(3); L;
	else
	    Op('jlequ'); writelabel(3); L;
	end;
	FreeReg(r);

    else
	Error('dofor: not BYTESIZE, HALFSIZE, or WORDSIZE');
    end;
    Pop(3);
end dofor;

procedure dogeq;
begin
    Compare(associatedType[opd11],Int(opd^[2]));
    if opd11 in CharSet{'c','j','s'} then
	nextjump := jcgeu;	(* set up to do geu jump *)
    else
	nextjump := jcge;	(* set up to do ge jump *)
    end;
end dogeq;

procedure dogrt;
begin
    Compare(associatedType[opd11],Int(opd^[2]));
    if opd11 in CharSet{'c','j','s'} then
	nextjump := jcgtu;	(* set up to do gtu jump *)
    else
	nextjump := jcgt;	(* set up to do gt jump *)
    end;
end dogrt;

procedure doinc;
begin
	if opdcount#2 then error(9);end;
	Increment(Int(opd^[2]));
end doinc;

procedure @inline doind;
begin
    MakeVariable(top);
    ees[top].ptype := associatedType[opd11];
    ees[top].size := Int(opd^[2]);
end doind;

procedure doinn;
var
    r : Reg;
    lab : LabelNumber;
    i, mask : integer;
begin
    if (ees[top].ptype#tset) then error(2);end;
    if not (ees[top-1].ptype in pcodetypeset{tinteger,tcardinal,tchar}) then
	error(4);
    end;

    if ees[top].size <= WORDSIZE then
	(* Set is word. *)
	Check(top, WORDSIZE);
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    (* Element is constant, create mask now *)
	    mask := 1;
	    for i := 1 to ees[top-1].constInt do
		mask := mask * 2;
	    end;
	    Op('bitl'); Opnd(top); X; LitI(mask); L;
	    nextjump := jcne;
	else
	    (* Element is not constant; create mask at run time.  Since ashl
	       instruction takes byte shift count, end up with zero flag set
	       means element in set *)
	    Check(top-1, WORDSIZE);
	    if ees[top-1].sreg # NULLREG then
		(* Indexing to get count would (incorrectly) BYTE index *)
		Eval(top-1);
	    end;
	    lab := NewLabel();
	    r := AllocReg(REGTEMP, 0, tset);
	    Op('cmpl'); Opnd(top-1); X; LitI(ees[top].size-1); L;
	    JumpToLabel('jgtru', lab);
	    Op('ashl'); Opnd(top-1); X; LitI(1); X; R(r); L;
	    Op('bicl2'); Opnd(top); X; R(r); L;
	    FreeReg(r);
	    LabelC(lab); L;
	    nextjump := jceq;
	end;
	Pop(2);
    else
	(* Multi-word set *)
	Check(top-1,WORDSIZE);
	lab := NewLabel();
	r := AllocReg(REGTEMP,0,tboolean);
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG)
		and (ees[top-1].constInt < ees[top].size) then
	    (* no test necessary *)
	else
	    Op('clrl'); R(r); L;
	    Op('cmpl'); Opnd(top-1); X; LitI(ees[top].size); L;
	    JumpToLabel('jgequ', lab);
	end;
	Point(top); (* instruction requires address *)
	CheckRegs(top,BYTESIZE);    (* instruction requires byte index *)
	Op('extzv'); Opnd(top-1); X; LitI(1); X; Opnd(top); X; R(r); L;
	LabelC(lab); L;
	Pop(2);
	Push(EESDATA);
	ees[top].ptype := tboolean;
	ees[top].size := BOOLSIZE;
	ees[top].dreg := r;
	DeRegisterize(r,top);
    end;
end doinn;

procedure doint;
var
    wordsize : sizerange;
begin
    if ees[top].size <= WORDSIZE then
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    SwapEES(top, top-1);    (* Put constant on top, dest at top-1  *)
	    ees[top].constInt := - ees[top].constInt - 1;
	elsif (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	    ees[top].constInt := - ees[top].constInt - 1;
	else (* neither is constant, so runtime complement top *);
	    Eval(top);
	    Op('mcoml'); Opnd(top); X; Opnd(top); L;
	end;
	TwoOrThree('biclN',top,top-1,tset,WORDSIZE,true);
    else
	wordsize := (ees[top].size + WORDSIZE-1) div WORDSIZE;
	Eval(top);
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := wordsize;
	MultiWordBinOp('mcoml', '', top-1, top-1);
	Eval(top-1);
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := wordsize;
	MultiWordBinOp('bicl2', '', top-1, top-2);
    end;
    Pop(1);
end doint;

procedure doior;
begin
    TwoOrThree('bisbN',top,top-1,tboolean,BOOLSIZE,true);
    Pop(1);
end doior;

procedure @inline dolab;
var
    i : integer;
begin
    if pclbsize=0 then error(19);end;
    for i := 1 to pclbsize do opd^[1][i]:=pclabel[i];end;
    opdsizes^[1] := pclbsize;
(* |||    Op('.align'); I(2); X; I(1); L; *)
    writelabel(1); C(':'); L;
    ClearDisp;
end dolab;

procedure dolao;
begin
    Push(EESADDR);
    ees[top].ptype := taddress;
    ees[top].size := ADDRSIZE;
    ees[top].addrLevel := 0;
    ees[top].addrMemType := opd^[2][1];
    ees[top].addrOffset := Int(opd^[3]);
    ees[top].addrBlock := Int(opd^[4]);
end dolao;

procedure dolca;
var
    size : sizerange;
    constlab : integer;
    i,cn : integer;
    numChar : integer;
    c : char;
    procedure AddrConstInt(value,n : integer);
    var
	i : integer;
    begin
	DataSeg;
	Op('.align'); I(2); L;
	C('k'); I(constlab); C(':'); L;
	for i := 1 to n do
	    Op('.long'); I(value); L;
	end;
	TextSeg;
	Push(EESADDR);
	ees[top].ptype := taddress;
	ees[top].size := ADDRSIZE;
	ees[top].addrMemType := 'k';
	ees[top].addrBlock := constlab;
    end AddrConstInt;
begin
    size := Int(opd^[2]);
    constlab := currentConstant;
    currentConstant := currentConstant + 1;
    case opd11 of
    | 'a':
	AddrConstInt(Int(opd^[3]) div BYTESIZE,1);
    
    | 'b','i','j','k':
	AddrConstInt(Int(opd^[3]),1);
    
    | 'c':
	AddrConstInt(ord(opd^[3][1]),1);
    
    | 'n':
	AddrConstInt(0,size div WORDSIZE);
    
    | 'p':
	DataSeg;
	Op('.align'); I(2); L;
	C('k'); I(constlab); C(':'); L;
	Op('.long'); GN; S(opd^[3]); L;
	TextSeg;
	Push(EESADDR);
	ees[top].ptype := taddress;
	ees[top].size := ADDRSIZE;
	ees[top].addrMemType := 'k';
	ees[top].addrBlock := constlab;
    
    | 'r':
	DataSeg;
	Op('.align'); I(2); L;
	C('k'); I(constlab); C(':'); L;
$if VMS then
	Op('.float'); S(opd^[3]); L;
$else
	Op('.float'); S('0f'); S(opd^[3]); L;
$end;
	TextSeg;
	Push(EESADDR);
	ees[top].ptype := taddress;
	ees[top].size := ADDRSIZE;
	ees[top].addrMemType := 'k';
	ees[top].addrBlock := constlab;
    
    | 'R':
	DataSeg;
	Op('.align'); I(2); L;
	C('k'); I(constlab); C(':'); L;
$if VMS then
	Op('.double'); S(opd^[3]); L;
$else
	Op('.dfloat'); S('0d'); S(opd^[3]); L;
$end
	TextSeg;
	Push(EESADDR);
	ees[top].ptype := taddress;
	ees[top].size := ADDRSIZE;
	ees[top].addrMemType := 'k';
	ees[top].addrBlock := constlab;
    
    | 's':
	DataSeg;
	Op('.align'); I(2); L;
	C('k'); I(constlab); C(':'); L;
	numChar := opdsizes^[3];
$if VMS then
	for i := 0 to numChar-1 do
	    if i mod 8 = 0 then
		Op('.byte');
	    end;
	    c := opd^[3][i+1];
	    if (c in CharSet{' '..'~'}) and not (c in CharSet{'\\',''''}) then
		S('^A'''); C(c); C('''');
	    else
		I(ord(c));
	    end;
	    if (i mod 8 = 7) or (i = numChar-1) then
		L;
	    else
		C(',');
	    end;
	end;
$else (* UNIX *)
	Op('.ascii');
	C('"');
	for i := 1 to numChar do
	    if opd^[3][i] in (CharSet{' '..'~'} - CharSet{'\\',"'", '"'}) then
		C(opd^[3][i]);
	    else
		C('\\');
		cn := ord(opd^[3][i]);
		I(cn div 64);
		I((cn mod 64) div 8);
		I(cn mod 8);
	    end;
	end;
	C('"');
	L;
$end
	Bssz; I((size+BYTESIZE-1) div BYTESIZE - numChar + 1); L;
	TextSeg;
	Push(EESADDR);
	ees[top].ptype := taddress;
	ees[top].size := ADDRSIZE;
	ees[top].addrMemType := 'k';
	ees[top].addrBlock := constlab;
    
    | 'S':
	Push(EESADDR);
	ees[top].ptype := taddress;
	ees[top].size := ADDRSIZE;
	SetConst(size,top);

    else 
	error(1);
    end;
end dolca;

procedure @inline dolda;
begin
    Push(EESADDR);
    ees[top].ptype := taddress;
    ees[top].size := ADDRSIZE;
    ees[top].addrLevel := curlev - Int(opd^[2]);
    ees[top].addrMemType := opd^[3][1];
    ees[top].addrOffset := Int(opd^[4]);
    ees[top].addrBlock := Int(opd^[5]);
end dolda;

procedure @inline doldc;
var
    size : sizerange;
    i : integer;
    numChar : integer;
    bits, bitpos : integer;
    constlab : integer;
begin
    size := Int(opd^[2]);
    case opd11 of
    | 'a':
	Push(EESADDR);
	ees[top].ptype := taddress;
	ees[top].size := ADDRSIZE;
	ees[top].addrOffset := Int(opd^[3]);
    
    | 'b':
	Push(EESDATA);
	ees[top].ptype := tboolean;
	ees[top].size := BOOLSIZE;
	ees[top].constInt := Int(opd^[3]);
    
    | 'c':
	Push(EESDATA);
	ees[top].ptype := tchar;
	ees[top].size := BYTESIZE;
	ees[top].constInt := ord(opd^[3][1]);
	if ees[top].constInt < 0 then
	    ees[top].constInt := ees[top].constInt + 256;
	end;
    
    | 'i','j','k':
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := Int(opd^[3]);
    
    | 'n':
	if size > WORDSIZE then
	    (* do same thing as lca, except make it a variable *)
	    dolca;
	    ees[top].kind := EESVAR;
	    ees[top].size := Int(opd^[2]);
	    ees[top].ptype := trecord;
	else
	    Push(EESDATA);
	    ees[top].ptype := taddress;
	    ees[top].size := WORDSIZE;
	    ees[top].constInt := 0;
	end;
    
    | 'p':
	Push(EESDATA);
	ees[top].ptype := tproc;
	ees[top].size := WORDSIZE;
	ees[top].dreg := AllocReg(REGEES,top,taddress);
$if vms then
	HashSymbol(opd^[3], opdsizes^[3]);
$end
	Op('movab'); GN; S(opd^[3]); X; Opnd(top); L;
	
    | 'r':
	Push(EESDATA);
	ees[top].ptype := treal;
	ees[top].size := WORDSIZE;
$if VMS then
	ees[top].dreg := AllocReg(REGEES,top,treal);
	Op('movf'); Lit; S('^f'); S(opd^[3]); X; Opnd(top); L;
$else
	ees[top].constInt := integer(float(atof(opd^[3])));
$end
    
    | 'R':
	constlab := currentConstant;
	currentConstant := currentConstant + 1;
	DataSeg;
	Op('.align'); I(2); L;
	C('k'); I(constlab); C(':'); L;
$if VMS then
	Op('.double'); S(opd^[3]); L;
$else
	Op('.dfloat'); S('0d'); S(opd^[3]); L;
$end
	TextSeg;
	Push(EESVAR);
	ees[top].ptype := tlongreal;
	ees[top].size := 2*WORDSIZE;
	ees[top].addrMemType := 'k';
	ees[top].addrBlock := constlab;

    | 's':
	(* do same thing as lca, except make it a variable *)
	dolca;
	ees[top].kind := EESVAR;
	ees[top].size := Int(opd^[2]);
	ees[top].ptype := tstring;
    
    | 'S':
	if size > WORDSIZE then
	    (* do same thing as lca, except make it a variable *)
	    dolca;
	    ees[top].kind := EESVAR;
	    ees[top].size := Int(opd^[2]);
	else
	    numChar := Int(opd^[3]);
	    bits := 0;
	    bitpos := 1;
	    for i := 1 to numChar do
		if opd^[4][i] = '1' then
		    bits := bits + bitpos;
		end;
		bitpos := bitpos * 2;
	    end;
	    Push(EESDATA);
	    ees[top].constInt := bits;
	    ees[top].kind := EESDATA;
	    ees[top].size := WORDSIZE;
	end;
	ees[top].ptype := tset;

    | else
	error(1);
    end (* case *);
end doldc;

procedure doldo;
begin
    Push(EESVAR);
    ees[top].ptype := associatedType[opd11];
    ees[top].size := Int(opd^[2]);
    ees[top].addrLevel := 0;
    ees[top].addrMemType := opd^[3][1];
    ees[top].addrOffset := Int(opd^[4]);
    ees[top].addrBlock := Int(opd^[5]);
end doldo;

procedure doleq;
begin
    Compare(associatedType[opd11],Int(opd^[2]));
    if opd11 in CharSet{'c','j','s'} then
	nextjump := jcleu;	(* set up to do leu jump *)
    else
	nextjump := jcle;	(* set up to do le jump *)
    end;
end doleq;

procedure doles;
begin
    Compare(associatedType[opd11],Int(opd^[2]));
    if opd11 in CharSet{'c','j','s'} then
	nextjump := jcltu;	(* set up to do ltu jump *)
    else
	nextjump := jclt;	(* set up to do lt jump *)
    end;
end doles;

procedure doljp;
    var addrLevel : Level;
begin
    addrLevel := curlev - Int(opd^[2]);
    (* Push pointer to lowest data in target, call PCLOSE *)
    Op('subl3'); Lit; C('a'); I(Int(opd^[3])); X;
	GN; S('runtime__display+'); I(addrLevel*4); X;
	S('-(sp)'); L;
    Op('calls'); LitI(1); X; GN; S('PCLOSE'); L;
    (* Push target label, push addr of saved environ, call longjmp *)
    Op('pushab'); writelabel(1); L;
    Op('addl3'); Lit; C('g'); I(Int(opd^[3])); C('+'); I(4*(MAXDISPLEVEL+1)); X;
	GN; S('runtime__display+'); I(addrLevel*4); X;
	S('-(sp)'); L;
    Op('calls'); LitI(2); X; GN; S('longjmp'); L;
end doljp;

procedure @inline dolod;
begin
    Push(EESVAR);
    ees[top].ptype := associatedType[opd11];
    ees[top].size := Int(opd^[2]);
    ees[top].addrLevel := curlev - Int(opd^[3]);
    ees[top].addrMemType := opd^[4][1];
    ees[top].addrOffset := Int(opd^[5]);
    ees[top].addrBlock := Int(opd^[6]);
end dolod;

procedure domax;
var lab : LabelNumber;
begin
    Eval(top-1);
    lab := NewLabel();
    if opd11 = 'r' then
	Check(top,WORDSIZE);
	Op('cmpf'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jgeq', lab);
	Op('movf'); Opnd(top); X; Opnd(top-1); L;
    elsif opd11 = 'R' then
	Check(top,2*WORDSIZE);
	Op('cmpd'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jgeq', lab);
	Op('movd'); Opnd(top); X; Opnd(top-1); L;
    elsif opd11 = 'j' then
	Check(top,WORDSIZE);
	Op('cmpl'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jgequ', lab);
	Op('movl'); Opnd(top); X; Opnd(top-1); L;
    else
	Check(top,WORDSIZE);
	Op('cmpl'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jgeq', lab);
	Op('movl'); Opnd(top); X; Opnd(top-1); L;
    end;
    LabelC(lab); L;
    Pop(1);
end domax;

procedure domin;
var lab : LabelNumber;
begin
    Eval(top-1);
    lab := NewLabel();
    if opd11 = 'r' then
	Check(top,WORDSIZE);
	Op('cmpf'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jleq', lab);
	Op('movf'); Opnd(top); X; Opnd(top-1); L;
    elsif opd11 = 'R' then
	Check(top,2*WORDSIZE);
	Op('cmpd'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jleq', lab);
	Op('movd'); Opnd(top); X; Opnd(top-1); L;
    elsif opd11 = 'j' then
	Check(top,WORDSIZE);
	Op('cmpl'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jlequ', lab);
	Op('movl'); Opnd(top); X; Opnd(top-1); L;
    else
	Check(top,WORDSIZE);
	Op('cmpl'); Opnd(top-1); X; Opnd(top); L;
	JumpToLabel('jleq', lab);
	Op('movl'); Opnd(top); X; Opnd(top-1); L;
    end;
    LabelC(lab); L;
    Pop(1);
end domin;

procedure domod;
var
    r, rx : Reg;
    k : integer;
    done : boolean;
    lab : LabelNumber;
begin
    done := false;
    if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	k := PowerOfTwo(ees[top].constInt);
	if k > 0 then
	    Eval(top-1);
	    (* just mask off high order bits *)
	    (* (because value is a power of 2, -value is correct mask) *)
	    Op('bicl2'); LitI(-ees[top].constInt); X; Opnd(top-1); L;
	    Pop(1);
	    done := true;
	end;
    end;
    if done then
	(* all done *)
    else
	if opd11 = 'j' then
	    SaveRegs(top-2);
	    MarkStack(2);
	    PushArg(top-1,0);
	    PushArg(top,1);
	    Pop(2);
	    CallProc(PCCEP,'j',WORDSIZE,2,RTLONGMOD,0);
	else
	    Eval(top);
	    Eval(top-1);
	    r := AllocReg(REGTEMP,0,tinteger);
	    lab := NewLabel();
	    Op('divl3'); Opnd(top); X; Opnd(top-1); X; R(r); L;
	    Op('mull2'); Opnd(top); X; R(r); L;
	    Op('subl2'); R(r); X; Opnd(top-1); L;
	    JumpToLabel('jgeq', lab);
	    Op('addl2'); Opnd(top); X; Opnd(top-1); L;
	    LabelC(lab); L;
	    FreeReg(r);
	    Pop(1);
	end;
    end;
end domod;

procedure domp2;
var
    size, k : integer;
begin
    size := Int(opd^[2]);
    MakeVariable(top-1);
    ees[top-1].size := size;
    Check(top-1,size);
    Check(top,size);
    if opd11 = 'r' then
	if size # WORDSIZE then
	    Error('opmp2: bad size');
	end;
	ees[top-1].ptype := treal;
	Op('mulf2'); Opnd(top); X; Opnd(top-1); L;
    elsif opd11 = 'R' then
	if size # 2*WORDSIZE then
	    Error('opmp2: bad size');
	end;
	ees[top-1].ptype := tlongreal;
	Op('muld2'); Opnd(top); X; Opnd(top-1); L;
    else
	if size # WORDSIZE then
	    Error('opmp2: bad size');
	end;
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	    (* multiply by constant *)
	    if ees[top].constInt = 1 then
		(* do nothing *)
	    else
		k := PowerOfTwo(ees[top].constInt);
		if k = 1 then
		    Op('addl2'); Opnd(top-1); X; Opnd(top-1); L;
		elsif k > 0 then
		    Op('ashl'); LitI(k); X; Opnd(top-1); X; Opnd(top-1); L;
		else
		    Op('mull2'); Opnd(top); X; Opnd(top-1); L;
		end;
	    end;
	else
	    Op('mull2'); Opnd(top); X; Opnd(top-1); L;
	end;
    end;
    Pop(2);
end domp2;

procedure @inline domst;
begin
(* ||| should move SaveRegs to proc call site, inc callNest in MarkStack,
   and changes SaveRegs to SaveReg all the active regs below current callNest,
   instead of just equal.  Restore should only restore current.  Construct
   horrible gross test cases. *)
    SaveRegs(top);
    MarkStack(Int(opd^[1]));
end domst;

procedure domup;
var
    k, saveconst : integer;
begin
    if opd11 = 'r' then
	TwoOrThree('mulfN',top,top-1,treal,WORDSIZE,true);
    elsif opd11 = 'R' then
	TwoOrThree('muldN',top,top-1,tlongreal,2*WORDSIZE,true);
    elsif opd11 = 'a' then
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG)
	    and (ees[top-1].kind = EESADDR) and (ees[top-1].breg = NULLREG)
	    and (ees[top-1].addrMemType = ' ') and (ees[top-1].sreg = NULLREG)
	then
	    (* both operands are constant, top-1 is address *)
	    ees[top-1].addrOffset := ees[top-1].addrOffset * ees[top].constInt;
	elsif (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG)
	    and (ees[top].kind = EESADDR) and (ees[top].breg = NULLREG)
	    and (ees[top].addrMemType = ' ') and (ees[top].sreg = NULLREG)
	then
	    (* both operands are constant, top is address *)
	    SwapEES(top,top-1);
	    ees[top-1].addrOffset := ees[top-1].addrOffset * ees[top].constInt;
	else
	    if (ees[top-1].kind = EESADDR) and (ees[top-1].breg = NULLREG)
	    and (ees[top-1].addrMemType = ' ') and (ees[top-1].sreg = NULLREG)
	    then
		(* top-1 is constant *)
		SwapEES(top,top-1);	(* make top constant *)
	    end;
	    if (ees[top].kind # EESADDR) or (ees[top].breg # NULLREG)
		or (ees[top].addrMemType # ' ') or (ees[top].sreg # NULLREG)
		or (ees[top-1].kind = EESADDR)
	    then
		TwoOrThree('mullN',top,top-1,taddress,WORDSIZE,true);
	    else
		saveconst := ees[top].addrOffset * ees[top-1].constInt;
		ees[top-1].constInt := 0;
		if (ees[top-1].kind = EESVAR) and not ees[top-1].indirect
			and AddrIsTReg(top-1) 
			and (ees[top-1].size = WORDSIZE) then
		    (* make it dreg: will become sreg below *)
		    ees[top-1].dreg := MemTReg(ees[top-1].addrOffset);
		    ClearAddress(top-1);
		else
		    Eval(top-1);
		end;
		ees[top-1].kind := EESADDR;
		ees[top-1].sunits := ees[top].addrOffset;
		ees[top-1].sreg := ees[top-1].dreg;
		ees[top-1].dreg := NULLREG;
		ees[top-1].addrOffset := saveconst;
	    end;
	end;
	ees[top-1].ptype := taddress;
	ees[top-1].size := WORDSIZE;
    (* else not address *)
    elsif (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG)
        and (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG)
    then
	(* both operands are constant *)
	ees[top-1].constInt := ees[top-1].constInt * ees[top].constInt;
    else
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    (* top-1 is constant, so switch and make top constant*)
	    SwapEES(top,top-1);
	end;
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	    (* multiply by constant *)
	    if ees[top].constInt # 1 then
		(* do constant part *)
		(* note: if saveconst # 0, avoid stores in TwoOrThree and put *)
		(*  value as constant part of new top *)
		saveconst := ees[top].constInt * ees[top-1].constInt;
		ees[top-1].constInt := 0;
		k := PowerOfTwo(ees[top].constInt);
		if k = 1 then
		    TwoOrThree('addlN', top-1, top-1, tinteger, WORDSIZE,
			saveconst=0);
		elsif k = 2 then
		    Eval(top-1);
                    if ees[top-1].dreg in 
			    RegSet{RETURNREG..LASTREG,rt0..rt9} then
                        Op('moval'); S('0['); Opnd(top-1); C(']'); X;
				Opnd(top-1); L;
		    else
			Op('ashl'); LitI(k); X; Opnd(top-1); X; Opnd(top-1); L;
		    end;
		elsif k > 0 then
		    Eval(top-1);
		    Op('ashl'); LitI(k); X; Opnd(top-1); X; Opnd(top-1); L;
		else
		    TwoOrThree('mullN',top,top-1,tinteger,WORDSIZE,saveconst=0);
		end;
		if saveconst # 0 then
		    ees[top-1].constInt := saveconst;
		end;
	    end;
	else
	    TwoOrThree('mullN',top,top-1,tinteger,WORDSIZE,true);
	end;
    end;
    Pop(1);
end domup;

procedure doneg;
begin
    Eval(top);
    if opd11 = 'r' then
	Op('mnegf'); Opnd(top); X; Opnd(top); L;
    elsif opd11 = 'R' then
	Op('mnegd'); Opnd(top); X; Opnd(top); L;
    else
	Op('mnegl'); Opnd(top); X; Opnd(top); L;
    end;
end doneg;

procedure doneq;
begin
    Compare(associatedType[opd11],Int(opd^[2]));
    nextjump := jcne;       (* set up for ne jump *)
end doneq;

procedure donot;
begin
    Eval(top);
    Op('xorl2'); LitI(1); X; Opnd(top); L;
end donot;

procedure doodd;
begin
    PushConst(tinteger,WORDSIZE,-2);
    TwoOrThree('biclN',top,top-1,tboolean,WORDSIZE,true);
    Pop(1);
    ees[top].ptype := tboolean;
    ees[top].size := BOOLSIZE;
end doodd;

procedure doord;
begin
    ees[top].ptype := tcardinal;
end doord;

procedure @inline dopar;
begin
    PushArg(top, Int(opd^[1]));
    Pop(1);
end dopar;

procedure doret;
var
    r : Reg;
    off : integer;
begin
    if (opdcount#1) then error(9);end;
    if (opdsizes^[1]#1) then error(1);end;
    if ((opd11='P') and (top#0)) or
	 (opd11#'P') and (top#1)
    then
	Error('Inconsistent stack on return');
    end;
    if (opd11#'P') then
	if ees[top].ptype = tlongreal then
	    Check(top,2*WORDSIZE);
	    if (ees[top].kind # EESDATA) or (ees[top].dreg # RETURNREG)
	    then
		Op('movq'); Opnd(top); X; R(RETURNREG); L;
	    end;
	elsif ees[top].size > WORDSIZE then
	    if IsBaseOffset(top) then
		MoveAddress(top); R(RETURNREG); L;
	    else
		Point(top);
		if ees[top].breg # RETURNREG then
		    Op('movl'); R(ees[top].breg); X; R(RETURNREG); L;
		end;
	    end;
	else
	    Check(top,WORDSIZE);
	    if (ees[top].kind # EESDATA) or (ees[top].dreg # RETURNREG)
	    then
		Op('movl'); Opnd(top); X; R(RETURNREG); L;
	    end;
	end;
	Pop(1);
    end;
    if not nodisplay then
	(* restore display *)
	Op('movl'); I(DISPOFF); C('('); R(fp); C(')'); X;
		GN; S('runtime__display+'); I(curlev*4); L;
    end;
    if curblockid = mainprogblockid then
$if VMS then
	Op('pushl'); LitI(1); L;
$else
	Op('pushl'); LitI(0); L;
$end
	Op('calls'); LitI(1); X; GN; S('runtime__term'); L;
    elsif not internal then
	Op('ret'); L;
    else
$if vms then
	(* Put sp back to pushed registers, then popr them off *)
	Op('subl3'); Lit;
	    C('<'); C('a'); I(curblockid); C('+');
		    C('o'); I(curblockid); C('>'); X;
	    R(fp); X;
	    R(sp); L;
	Op('popr'); Lit; C('v'); I(curblockid); L;
$else
	if firsttreg # NULLREG then
	    off := 4;
	    (* restore regs from after end of activation record *)
	    (*  (-arsize-4)(fp), (-arsize-8)(fp), etc. *)
	    for r := firsttreg to LASTREG do
		Op('movl'); S('(-a'); I(curblockid); C('-'); I(off);
			S(')(fp)'); X; R(r); L;
		off := off + 4;
	    end;
	end;
$end
	Op('movl'); S('8(fp)'); X; R(ap); L;
	Op('movab'); S('12(fp)'); X; R(sp); L;
	Op('movl'); S('(sp)+'); X; R(fp); L;
	Op('rsb'); L;
    end;
    top := 0;
end doret;

procedure dornd;
var
    r : Reg;
begin
    r := AllocReg(REGEES,top,tinteger);
    if opd11 = 'r' then
	Check(top,WORDSIZE);
	Op('cvtrfl'); Opnd(top); X; R(r); L;
    elsif opd11 = 'R' then
	Check(top,2*WORDSIZE);
	Op('cvtrdl'); Opnd(top); X; R(r); L;
    else
	Error('Bad type');
    end;
    Pop(1);
    Push(EESDATA);
    ees[top].dreg := r;
    ees[top].ptype := tinteger;
    ees[top].size := WORDSIZE;
end dornd;

procedure dorst;
begin
    (* Copy back display from saved g memory *)
    Op('movc3'); LitI(4*(MAXDISPLEVEL+1)); X;
	C('g'); I(curblockid); S('(fp)'); X;
	GN; S('runtime__display'); L;
end dorst;

procedure dosac;
var
    size : integer;
begin
    (* top-1 is pointer *)
    (* top is the size as an address, get value in bytes *)
    if (ees[top].kind = EESADDR) and (ees[top].breg = NULLREG) and
	(ees[top].sreg = NULLREG) and (ees[top].addrMemType = ' ')
    then
	size := (((ees[top].addrOffset + WORDSIZE-1) div WORDSIZE)
		* BYTESPERWORD);
	Pop(1);
	PushConst(tinteger,WORDSIZE,size);
    else
	(* calculate size *)
	Eval(top);

	(* round up to multiple of words *)
	Op('addl2'); LitI(BYTESPERWORD - 1); X; Opnd(top); L;
	Op('bicl2'); LitI(BYTESPERWORD - 1); X; Opnd(top); L;
    end;

    (* subtract size from sp to allocate *)
    Op('subl2'); Opnd(top); X; R(sp); L;

    (* convert bytes to words *)
    if ees[top].dreg = NULLREG then
	(* constant *)
	ees[top].constInt := ees[top].constInt div BYTESPERWORD;
    else
	Op('ashl'); LitI(-2); X; Opnd(top); X; Opnd(top); L;
    end;

    (* get register for sp (dest), register for pointer (source) *)
    PushReg(taddress,WORDSIZE,AllocReg(REGREG,0,taddress));
    PushReg(taddress,WORDSIZE,AllocReg(REGSP,0,taddress));

    (* top=<reg> top-1=<reg> top-2=<length> top-3=<address of pointer> *)
    SwapEES(top,top-2);
    (* top=<length> top-1=<reg> top-2=<reg> top-3=<address of pointer> *)

    (* address of data pointer *)
(* ||| This line causes certain programs running with optimization to take the
  address of a register.  I'm suspicious removing this line, but all the test
  cases work perfectly, and, in fact, generate better code than with the line
  in.
    Point(top-3);
||| *)

    (* source of data *)
    Op('movl'); Opnd(top-3); X; Opnd(top-1); L;

    (* top=<length> top-1=<pointer> top-2=<new area> top-3=<pointer> *)
    (* copy data *)
    MultiWordBinOp('movl', 'movq', top-1, top-2);

    (* top=<pointer> top-1=<new area> top-2=<pointer> *)

    (* set pointer to point to new area *)
    Op('movl'); R(sp); X; Opnd(top-2); L;

    Pop(3);
end dosac;

procedure dosal;
var
    size : integer;
begin
    (* top-1 is address of pointer *)
    (* top is the size in bytes *)
    if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
	ees[top].constInt := 
	    ((ees[top].constInt + BYTESPERWORD - 1) div BYTESPERWORD)
		* BYTESPERWORD;
    else
	(* calculate size *)
	Eval(top);

	(* round up to multiple of words *)
	Op('addl2'); LitI(BYTESPERWORD - 1); X; Opnd(top); L;
	Op('bicl2'); LitI(BYTESPERWORD - 1); X; Opnd(top); L;
    end;

    (* subtract size from sp to allocate *)
    Op('subl2'); Opnd(top); X; R(sp); L;

    (* set pointer *)
    Point(top-1);
    Op('movl'); R(sp); X; Opnd(top-1); L;

    Pop(2);
end dosal;

procedure dosav;
var
    e : EESElement;

    procedure @inline SimpleOperand(const e : EESElement) : boolean;
    begin
	return (ees[e].kind = EESDATA) or
	    ((ees[e].kind = EESADDR) and (ees[e].sreg = NULLREG) and
		not ees[e].indirect);
    end SimpleOperand;

begin
    e := -Int(opd^[1])-1;
    if e < NUMTEMPS then
	Error('sav: not enough temporaries declared');
    else
	case opd^[2][1] of
	| 'r' :
	    (* replace a value already there *)
	    if not ees[e].inUse then
		Error('sav r: not in use');
	    end;
	    if ees[e].size <= WORDSIZE then
		if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) then
		    (* Don't eval *)
		else
		    Eval(top);
		end;
		Op('movl'); Opnd(top); X; Opnd(e); L;
	    else
		Store(top,e);
	    end;
	    Pop(1);

	| 't' :
	    (* transfer a value.  Evaluate it even if simple *)
	    if ees[e].inUse then
		Error('sav t: in use');
	    end;
	    Eval(top);
	    SwapEES(top,e);
	    Pop(1);

	| 'm' :
	    (* move a value, or just the descriptor if simple *)
	    if ees[e].inUse then
		Error('sav m: in use');
	    end;
	    if SimpleOperand(top) then
		(* Don't need to Eval *)
	    else
		Eval(top);
	    end;
	    SwapEES(top,e);
	    Pop(1);

	| 'c' :
	    (* copy a value, or just the descriptor if simple *)
	    if ees[e].inUse then
		Error('sav c: in use');
	    end;
	    if SimpleOperand(top) then
		(* Don't need to Eval *)
	    else
		Eval(top);
	    end;
	    SwapEES(top,e);
	    (* copy stack element *)
	    ees[top] := ees[e];
	    (* deallocate memory (temp has it) *)
	    ees[top].smemoffset := 0;
	    ees[top].smemsize := 0;
	    (* reallocate and copy register *)
	    if ees[top].dreg # NULLREG then
		ees[top].dreg := AllocReg(REGEES,top,ees[top].ptype);
		Op('movl'); R(ees[e].dreg); X; R(ees[top].dreg); L;
	    end;
	    if ees[top].breg # NULLREG then
		ees[top].breg := AllocReg(REGEES,top,taddress);
		Op('movl'); R(ees[e].breg); X; R(ees[top].breg); L;
	    end;

	| 'd' :
	    (* dispose of a value *)
	    Pop(1);

	| else
	    Error('sav: not r, c, m, t, or d');
	end;
    end;
end dosav;

procedure dosb2;
var
    size : integer;
begin
    size := Int(opd^[2]);
    MakeVariable(top-1);
    ees[top-1].size := size;
    Check(top-1,size);
    Check(top,size);
    if opd11 = 'r' then
	if size # WORDSIZE then
	    Error('opsb2: bad size');
	end;
	ees[top-1].ptype := treal;
	Op('subf2'); Opnd(top); X; Opnd(top-1); L;
    elsif opd11 = 'R' then
	if size # 2*WORDSIZE then
	    Error('opsb2: bad size');
	end;
	ees[top-1].ptype := tlongreal;
	Op('subd2'); Opnd(top); X; Opnd(top-1); L;
    else
	if (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG) and
		(ees[top].constInt = 1)	then
	    if size = WORDSIZE then
		Op('decl'); Opnd(top-1); L;
	    elsif size = HALFSIZE then
		Op('decw'); Opnd(top-1); L;
	    elsif size = BYTESIZE then
		Op('decb'); Opnd(top-1); L;
	    else
		Error('opsb2: bad size');
	    end;
	else
	    if size = WORDSIZE then
		Op('subl2');
	    elsif size = HALFSIZE then
		Op('subw2');
	    elsif size = BYTESIZE then
		Op('subb2');
	    else
		Error('opsb2: bad size');
	    end;
	    Opnd(top); X; Opnd(top-1); L;
	end;
    end;
    Pop(2);
end dosb2;

procedure dosdf;
var
    wordsize : sizerange;
begin
    if ees[top].size <= WORDSIZE then
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    SwapEES(top, top-1);    (* Put constant on top, dest at top-1  *)
	end;
	TwoOrThree('xorlN',top,top-1,tset,WORDSIZE,true);
    else
	wordsize := (ees[top].size + WORDSIZE-1) div WORDSIZE;
	Eval(top-1);
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := wordsize;
	MultiWordBinOp('xorl2', '', top-1,top-2);
    end;
    Pop(1);
end dosdf;

procedure doset;
var
    size, numopd, mask : integer;
    setees : EESElement;
    addr : boolean;
    r : Reg;
begin
    size := Int(opd^[2]);
    numopd := Int(opd^[3]);
    setees := top - numopd;
    if size <= WORDSIZE then
	(* one word, evaluate it *)
	Eval(setees);
	if numopd = 1 then
	    Check(top,WORDSIZE);
	    Op('insv'); LitI(1); X; Opnd(top); X; LitI(1); X; Opnd(setees); L;
	    Pop(1);
	else
	    (* calculate (top) - (top-1) + 1 *)
	    Increment(1);	(* add 1 to top *)
	    Check(top,WORDSIZE);
	    Check(top-1,WORDSIZE);
	    r := AllocReg(REGTEMP,0,tinteger);
	    Op('subl3'); Opnd(top-1); X; Opnd(top); X; R(r); L;
	    (* insert (top)-(top-1)-1 one bits into set starting at (top-1) *)
	    Op('insv'); LitI(-1); X; Opnd(top-1); X; R(r); X; Opnd(setees); L;
	    FreeReg(r);
	    Pop(2);
	end;
    else
	Eval(setees);
	if (ees[setees].kind = EESVAR) and (ees[setees].ptype = tset)
	then
	    (* change variable to address *)
	    ees[setees].kind := EESADDR;
	    addr := false;
	else
	    addr := true;
	end;
	if numopd = 2 then
	    (* push copy of address onto stack *)
	    Push(EESDATA);
	    ees[top].dreg := AllocReg(REGEES,top,taddress);
	    Op('movab'); Opnd(setees); X; Opnd(top); L;
	    (* do range with subroutine *)
	    SaveRegs(top-3);
	    MarkStack(3);
	    PushArg(top-2,0);
	    PushArg(top-1,1);
	    PushArg(top,2);
	    Pop(3);
	    CallProc(PCCEP,'P',0,3,RTMAKESET,0);
	    (* leave address on stack *)
	else
	    Check(top,WORDSIZE);
	    CheckRegs(setees,BYTESIZE);	(* instruction requires byte index *)
	    Op('insv'); LitI(1); X; Opnd(top); X; LitI(1); X; Opnd(setees); L;
	    Pop(1);
	end;
	if not addr then
	    ees[top].kind := EESVAR;
	end;
    end;
end doset;

procedure dosex;
var
    mask, size : integer;
begin
    if (ees[top-1].ptype#taddress) then error(2);end;
    if not (ees[top].ptype in pcodetypeset{tinteger,tcardinal,tchar}) then
	error(4);
    end;
    size := Int(opd^[2]);
    MakeVariable(top-1);
    ees[top-1].size := size;
    ees[top-1].ptype := tset;
    if ees[top-1].addrOffset mod BYTESIZE # 0 then
	Eval(top);
	ees[top].constInt := ees[top].constInt +
			(ees[top-1].addrOffset mod BYTESIZE);
	ees[top-1].addrOffset :=
			(ees[top-1].addrOffset div BYTESIZE) * BYTESIZE;
    else
	Check(top,WORDSIZE);
    end;
    Check(top-1,ees[top-1].size);
    if (ees[top-1].size <= WORDSIZE) and (ees[top].kind = EESDATA) and
	    (ees[top].dreg = NULLREG)
    then
	mask := power(2,ees[top].constInt);
	if ees[top-1].size <= BYTESIZE then
	    Op('bicb2'); LitI(mask); X; Opnd(top-1); L;
	else
	    Op('bicl2'); LitI(mask); X; Opnd(top-1); L;
	end;
    else
	Check(top,WORDSIZE);
	CheckRegs(top-1,BYTESIZE);	(* instruction requires byte index *)
	Op('insv'); LitI(0); X; Opnd(top); X; LitI(1); X; Opnd(top-1); L;
    end;
    Pop(2);
end dosex;

procedure dosin;
var
    mask, size : integer;
begin
    if (ees[top-1].ptype#taddress) then error(2);end;
    if not (ees[top].ptype in pcodetypeset{tinteger,tcardinal,tchar}) then
	error(4);
    end;
    size := Int(opd^[2]);
    MakeVariable(top-1);
    ees[top-1].size := size;
    ees[top-1].ptype := tset;
    if ees[top-1].addrOffset mod BYTESIZE # 0 then
	Eval(top);
	ees[top].constInt := ees[top].constInt +
			(ees[top-1].addrOffset mod BYTESIZE);
	ees[top-1].addrOffset :=
			(ees[top-1].addrOffset div BYTESIZE) * BYTESIZE;
    else
	Check(top,WORDSIZE);
    end;
    Check(top-1,ees[top-1].size);
    if (ees[top-1].size <= WORDSIZE) and (ees[top].kind = EESDATA) and
	    (ees[top].dreg = NULLREG)
    then
	mask := power(2,ees[top].constInt);
	if ees[top-1].size <= BYTESIZE then
	    Op('bisb2'); LitI(mask); X; Opnd(top-1); L;
	else
	    Op('bisl2'); LitI(mask); X; Opnd(top-1); L;
	end;
    else
	CheckRegs(top-1,BYTESIZE);	(* instruction requires byte index *)
	Op('insv'); LitI(1); X; Opnd(top); X; LitI(1); X; Opnd(top-1); L;
    end;
    Pop(2);
end dosin;

procedure dosml;
var
    r : Reg;
begin
    if (opdcount#0) then error(9);end;
    if (ees[top].ptype#tset) then error(2);end;

    if (ees[top].size>WORDSIZE) then
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := ees[top].size;
	SaveRegs(top-2);
	MarkStack(2);
	PushArg(top-1,0);
	PushArg(top,1);
	Pop(2);
	CallProc(PCCEP,'j',WORDSIZE,2,RTSMALLEST,0);
    else
	Eval(top); (* instruction requires register *)
	r := AllocReg(REGEES,top,tinteger);
	Op('ffs'); LitI(0); X; LitI(ees[top].size); X; Opnd(top);
			    X; R(r); L;
	Pop(1);
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].dreg := r;
    end;
end dosml;

procedure dosro;
var
    ptype : pcodetype;
    size : sizerange;
begin
    ptype := associatedType[opd11];
    size := Int(opd^[2]);
    Push(EESVAR);
    ees[top].ptype := ptype;
    ees[top].size := size;
    ees[top].addrLevel := 0;
    ees[top].addrMemType := opd^[3][1];
    ees[top].addrOffset := Int(opd^[4]);
    ees[top].addrBlock := Int(opd^[5]);
    Store(top-1,top);
    Pop(2);
end dosro;

procedure dosst;
    var lab : LabelNumber;
begin
    (* Set gmemoffset appropriately *)
    assert(gmemoff = mmemoff);
    gmemoff := gmemoff - 4*(MAXDISPLEVEL+1) - SETJMPBUF;

    (* copy entire contents of display into g memory *)
    Op('movc3'); LitI(4*(MAXDISPLEVEL+1)); X;
	GN; S('runtime__display'); X;
	C('g'); I(curblockid); S('(fp)'); L;
    (* Push address to store environment and call setjmp *)
    Op('pushab'); C('g'); I(curblockid); C('+'); 
	I(4*(MAXDISPLEVEL+1)); S('(fp)'); L;
    Op('calls'); LitI(1); X; GN; S('setjmp'); L;
    (* Jump to label passed to longjmp if that's how we got here *)
    lab := NewLabel();
    Op('tstl'); R(RETURNREG); L;
    JumpToLabel('jeql', lab);
    Op('jmp'); C('('); R(RETURNREG); C(')'); L;
    LabelC(lab); L;
end dosst;

procedure dostn;
begin
    Push(EESVAR);
    ees[top].ptype := associatedType[opd11];
    ees[top].size := Int(opd^[2]);
    ees[top].addrLevel := curlev - Int(opd^[3]);
    ees[top].addrMemType := opd^[4][1];
    ees[top].addrOffset := Int(opd^[5]);
    ees[top].addrBlock := Int(opd^[6]);
    if AddrIsTReg(top) then
	(* store into t reg *)
	Store(top-1,top);
	(* use t reg as value on top of stack *)
	SwapEES(top-1,top);
    else
	(* force Eval of value to keep put it in a register *)
	Eval(top-1);
	Store(top-1,top);
    end;
    Pop(1);
end dostn;

procedure dosto;
begin
    MakeVariable(top);
    ees[top].ptype := associatedType[opd11];
    ees[top].size := Int(opd^[2]);
    Store(top-1,top);
    Pop(2);
end dosto;

procedure @inline dostr;
begin
    Push(EESVAR);
    ees[top].ptype := associatedType[opd11];
    ees[top].size := Int(opd^[2]);
    ees[top].addrLevel := curlev - Int(opd^[3]);
    ees[top].addrMemType := opd^[4][1];
    ees[top].addrOffset := Int(opd^[5]);
    ees[top].addrBlock := Int(opd^[6]);
    Store(top-1,top);
    Pop(2);
end dostr;

procedure dosub;
begin
    if opd11 = 'r' then
	TwoOrThree('subfN',top,top-1,treal,WORDSIZE,true);
    elsif opd11 = 'R' then
	TwoOrThree('subdN',top,top-1,tlongreal,2*WORDSIZE,true);
    elsif (ees[top].kind = EESDATA) and (ees[top].dreg = NULLREG)
	and (ees[top-1].kind in EESKindSet{EESDATA,EESVAR})
    then
	(* top is constant *)
	ees[top-1].constInt := ees[top-1].constInt - ees[top].constInt;
    else
	TwoOrThree('sublN',top,top-1,tinteger,WORDSIZE,true);
    end;
    Pop(1);
end dosub;

procedure @inline dosym;
var
    size, offset, block  : integer;
    stabkind : char;
begin
    stabkind := opd11;
    if stabkind = 'l' then
	if opdcount # 4 then
	    error(501);
	end;
$IF vms THEN
	Comment;	(* Make the .stab directives a comment - CED 9/29/87 *)
	S(' Source line # '); S(opd^[4]); L;
$ELSE (* unix *)
	Op('.stabd'); S(opd^[2]); X; S(opd^[3]); X; S(opd^[4]); L;
    elsif stabkind = 'X' then
	Op('.stabs'); C('"'); SOEscape(opd^[2],opdsizes^[2]); C('"'); X;
		S(opd^[3]); X; S(opd^[4]); X; I(0); X; I(0); L;
    else
	Op('.stabs'); C('"'); SOEscape(opd^[2],opdsizes^[2]); C('"'); X;
		S(opd^[3]); X; S(opd^[4]); X;
	case stabkind of
	| 's' :
	    S(opd^[5]); X; writelabel(6); L;
	| 't', 'G' :
	    size := Int(opd^[5]);
	    size := (size + BYTESIZE - 1) div BYTESIZE;
	    I(size); X; S(opd^[6]); L;
	| 'p' :
	    size := Int(opd^[5]);
	    size := (size + BYTESIZE - 1) div BYTESIZE;
	    offset := Int(opd^[7]);
	    offset := (WORDSIZE + offset + BYTESIZE - 1) div BYTESIZE;
	    I(size); X; I(offset); L;
	| 'v' :
	    size := Int(opd^[5]);
	    size := (size + BYTESIZE - 1) div BYTESIZE;
	    offset := Int(opd^[7]);
	    offset := (offset + BYTESIZE - 1) div BYTESIZE;
	    I(size); X;
	    C(opd^[6][1]); S(opd^[8]); C('+'); I(offset); L;
	| 'r' :
	    size := Int(opd^[5]);
	    size := (size + BYTESIZE - 1) div BYTESIZE;
	    offset := Int(opd^[7]);
	    I(size); X;
	    I(ord(TRegReg(MemTReg(offset))) - ord(r0)); L;
	| 'F' :
	    size := Int(opd^[5]);
	    size := (size + BYTESIZE - 1) div BYTESIZE;
	    I(size); X; GN; S(opd^[6]); L;
	| 'P' :
	    size := Int(opd^[5]);
	    size := (size + BYTESIZE - 1) div BYTESIZE;
	    I(size); X; I(0); L;
	| 'm' :
	    I(0); X; I(0); L;
	| else
	    error(502);
	    L;
	end;
$END (* unix *)
    end;
end dosym;

procedure dotjp;
begin
    if (ees[top].ptype#tboolean) then error(2);end;
    if (opdcount#1) then error(9);end;

    Check(top,BOOLSIZE);
    Op('bitb'); LitI(1); X; Opnd(top); L;
    Op('jneq'); writelabel(1); L;
    Pop(1);
end dotjp;

procedure dotrc;
var
    r : Reg;
begin
    r := AllocReg(REGEES,top,tinteger);
    if opd11 = 'r' then
	Check(top,WORDSIZE);
	Op('cvtfl'); Opnd(top); X; R(r); L;
    elsif opd11 = 'R' then
	Check(top,2*WORDSIZE);
	Op('cvtdl'); Opnd(top); X; R(r); L;
    else
	Error('Bad type');
    end;
    Pop(1);
    Push(EESDATA);
    ees[top].dreg := r;
    ees[top].ptype := tinteger;
    ees[top].size := WORDSIZE;
end dotrc;

procedure dotyp;
var
    ptype : pcodetype;
begin
    if (opdcount#1) then error(9);end;
    if (opdsizes^[1]#1) then error(1);end;

    ptype := associatedType[opd11];
    if ees[top].size > WORDSIZE then
	ees[top].ptype := ptype;

    elsif (ees[top].kind in EESKindSet{EESDATA,EESVAR}) 
	    and (datasize(ptype,ees[top].size) = ees[top].size) then
	ees[top].ptype := ptype;
	ees[top].size := datasize(ptype, ees[top].size);

    else
	Eval(top);
	ees[top].ptype := ptype;
	ees[top].size := datasize(ptype, ees[top].size);
    end;
end dotyp;

procedure doujp;
begin
    if opdcount#1 then error(9);end;
    Op('jbr'); writelabel(1); L;
end doujp;

procedure douni;
var
    wordsize : sizerange;
begin
    if ees[top].size <= WORDSIZE then
	if (ees[top-1].kind = EESDATA) and (ees[top-1].dreg = NULLREG) then
	    SwapEES(top, top-1);    (* Put constant on top, dest at top-1  *)
	end;
	TwoOrThree('bislN',top,top-1,tset,WORDSIZE,true);
    else
	wordsize := (ees[top].size + WORDSIZE-1) div WORDSIZE;
	Eval(top-1);
	Push(EESDATA);
	ees[top].ptype := tinteger;
	ees[top].size := WORDSIZE;
	ees[top].constInt := wordsize;
	MultiWordBinOp('bisl2', '', top-1, top-2);
    end;
    Pop(1);
end douni;

procedure douse;
var
    e : EESElement;
begin
    e := -Int(opd^[1])-1;
    if e < NUMTEMPS then
	Error('use: not enough temporaries declared');
    else
	if not ees[e].inUse then
	    Error('use: not in use');
	end;
	Push(EESDATA);
	if opd^[2][1] = 'c' then
	    (* copy stack element (will get type, etc., from saved element *)
	    ees[top] := ees[e];
	    (* deallocate memory (temp has it) *)
	    ees[top].smemoffset := 0;
	    ees[top].smemsize := 0;
	    (* reallocate and copy register *)
	    if ees[top].dreg # NULLREG then
		ees[top].dreg := AllocReg(REGEES,top,ees[top].ptype);
		Op('movl'); R(ees[e].dreg); X; R(ees[top].dreg); L;
	    end;
	    if ees[top].breg # NULLREG then
		ees[top].breg := AllocReg(REGEES,top,taddress);
		Op('movl'); R(ees[e].breg); X; R(ees[top].breg); L;
	    end;
	elsif opd^[2][1] = 'm' then
	    SwapEES(e,top);
	    ClearEES(e);
	elsif opd^[2][1] = 'd' then
	    SwapEES(e,top);
	    ClearEES(e);
	    Pop(1);
	else
	    Error('use: not c, m or d');
	end;
    end;
end douse;

procedure doxjp;
var
    lb, ub : integer;
begin

    lb := Int(opd^[3]);
    ub := Int(opd^[4]);
    Eval(top);
    Op('cmpl'); Opnd(top); X; LitI(lb); L;
    Op('jlss'); writelabel(2); L;
    Op('cmpl'); Opnd(top); X; LitI(ub); L;
    Op('jgtr'); writelabel(2); L;
$IF vms THEN
    Op('movl'); C('<'); writelabel(1); C('+'); I(-lb*4); S('>[');
		    Opnd(top); C(']'); X; Opnd(top); L;
$ELSE
    Op('movl'); C('('); writelabel(1); C('+'); I(-lb*4); S(')[');
		    Opnd(top); C(']'); X; Opnd(top); L;
$END
    Op('jmp'); C('('); Opnd(top); C(')'); L;
    Pop(1);
end doxjp;

(* Added by CED on 8/13/87. *)
(*

    XFC means Extended Function Call. It gives access to a micro-code
procedure. For each parameter, put it in a register (XFC takes no operands).
For now, R0 contains the opcode. In the future, R1 and R2 will contain the
operands. I don't check to see how many operands have been pushed onto the
stack (3rd operand of xfc p-code instruction). I assume 1. I also don't
properly allocate and de-allocate r0 because I don't think it is necessary and
it should always be free. r1 is usually used as a temp. So, in future I may
have to use StealReg before I can allocate either of them.

*)
procedure doxfc;
begin
    Op('movl'); Opnd(top); X; R(r0); L;
    Pop(1);
    CallProc(PCXFC, opd11, Int(opd^[2]), 1, '', 0)
end doxfc;

procedure dozzz;
begin
    Error('Unknown P-code opcode');
end dozzz;

procedure DoJumps (): boolean;
var lab1, lab2 : LabelNumber;
	(* if tjp, generate jump for nextjump condition *)
	(* if fjp, generate jump for not nextjump condition *)
	(* otherwise, generate code to set a boolean and push it on top *)
	(* when done, indicate whether instruction was done *)
begin
    if opcode = PCNOT then
	nextjump := jumpopposite[nextjump];
	return TRUE;
    elsif opcode = PCTJP then
	Op(jumpnames[nextjump]); writelabel(1); L;
	nextjump := jcnone;     (* jump was handled *)
	return TRUE;
    elsif opcode = PCFJP then
	Op(jumpnames[jumpopposite[nextjump]]); writelabel(1); L;
	nextjump := jcnone;     (* jump was handled *)
	return TRUE;
    else
	PushReg(tboolean,BOOLSIZE,AllocReg(REGEES,top+1,tboolean));
	lab1 := NewLabel();
	lab2 := NewLabel();
	JumpToLabel(jumpnames[nextjump], lab1);
	Op('clrl'); Opnd(top); L;
	JumpToLabel('jbr', lab2);
	LabelC(lab1); L;
	Op('movl'); LitI(1); X; Opnd(top); L;
	LabelC(lab2); L;
	nextjump := jcnone;     (* jump was handled *)
	return FALSE;
    end;
end DoJumps;

procedure Generate;
begin
(*
    if ECHOPCODE then 
	echopcodeline;
    end;
*)
    if (nextjump = jcnone) or (not DoJumps()) then
	case opcode of
	| PCABS:doabs
	| PCAD2:doad2
	| PCADD:doadd
	| PCAND:doand
	| PCBGN:dobgn
	| PCBIT:dobit
	| PCCAP:docap
	| PCCEP:docep
	| PCCHK:dochk
	| PCCHR:dochr
	| PCCIP:docip
	| PCCJP:docjp
	| PCCOM:docom
	| PCCTS:docts
	| PCCUP:docup
	| PCDBG:dodbg
	| PCDEC:dodec
	| PCDEF:dodef
	| PCDIF:dodif
	| PCDIV:dodiv
	| PCDV2:dodv2
	| PCEND:doend
	| PCENT:doent
	| PCEQU:doequ
	| PCEXI:doexi
	| PCFJP:dofjp
	| PCFLT:doflt
	| PCFOR:dofor
	| PCGEQ:dogeq
	| PCGRT:dogrt
	| PCINC:doinc
	| PCIND:doind
	| PCINI:doini
	| PCINN:doinn
	| PCINT:doint
	| PCIOR:doior
	| PCLAB:dolab
	| PCLAO:dolao
	| PCLCA:dolca
	| PCLDA:dolda
	| PCLDC:doldc
	| PCLDO:doldo
	| PCLEQ:doleq
	| PCLES:doles
	| PCLOD:dolod
	| PCMAX:domax
	| PCMIN:domin
	| PCMOD:domod
	| PCMP2:domp2
	| PCMST:domst
	| PCMUP:domup
	| PCNEG:doneg
	| PCNEQ:doneq
	| PCNOT:donot
	| PCODD:doodd
	| PCORD:doord
	| PCPAR:dopar
	| PCRET:doret
	| PCSAC:dosac
	| PCSAL:dosal
	| PCSAV:dosav
	| PCSB2:dosb2
	| PCSDF:dosdf
	| PCSET:doset
	| PCSEX:dosex
	| PCSIN:dosin
	| PCSML:dosml
	| PCSRO:dosro
	| PCSTN:dostn
	| PCSTO:dosto
	| PCSTR:dostr
	| PCSUB:dosub
	| PCSYM:dosym
	| PCTJP:dotjp
	| PCTRC:dotrc
	| PCTYP:dotyp
	| PCUJP:doujp
	| PCUNI:douni
	| PCUSE:douse
	| PCXJP:doxjp
	| PCSST:dosst
	| PCLJP:doljp
	| PCRST:dorst
	| PCADR:doadr
	| PCRND:dornd
	| PCXFC:doxfc		(* CED - 8/13/87 *)
	| PCZZZ:dozzz
	| else
	    error(56);
	end;
    end;
    FreeDisp;
end Generate;

begin
    curblockid := -1;
    numcomblocks := 0;
	
    (* stuff for delayed jump code *)
    nextjump := jcnone;

    jumpopposite[jcnone] := jcnone;
    jumpopposite[jceq] := jcne;
    jumpopposite[jcne] := jceq;
    jumpopposite[jcgt] := jcle;
    jumpopposite[jcle] := jcgt;
    jumpopposite[jclt] := jcge;
    jumpopposite[jcge] := jclt;
    jumpopposite[jcgtu] := jcleu;
    jumpopposite[jcleu] := jcgtu;
    jumpopposite[jcltu] := jcgeu;
    jumpopposite[jcgeu] := jcltu;

    jumpnames[jcnone] := '????';
    jumpnames[jceq] := 'jeql';
    jumpnames[jcne] := 'jneq';
    jumpnames[jcgt] := 'jgtr';
    jumpnames[jcle] := 'jleq';
    jumpnames[jclt] := 'jlss';
    jumpnames[jcge] := 'jgeq';
    jumpnames[jcgtu] := 'jgtru';
    jumpnames[jcleu] := 'jlequ';
    jumpnames[jcltu] := 'jlssu';
    jumpnames[jcgeu] := 'jgequ';

end OpSubs.
