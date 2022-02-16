implementation module GenPC;

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


from io import
    Writef, Writec, output;

from MemLib import
    ALLOCATE;

from Machine import
    HugeInteger, WORDSIZE, BOOLEANSIZE, HALFSIZE, BYTESIZE, CharSet;
$if pascal then
from Globals import bufferFlag;
from Symbols import FileVarNode, programFileVarList, builtinScope;
$end

from Strings import
    String, AddChar, NonHashText, AddString, WriteString, WriteStringConst;

from Tokens import
    Token, TokenSet;

from Globals import
    target, TARGETVAX, genDebugInfoFlag, optimFlag, 
    internalCallFlag, TraceGenpc, DEBUG, MODULEINITNAME, compileModuleName,
    mainFileName, OptNreg, OptNcall, TraceOptim;

from Symbols import
    NULLLABEL, NUMOPTTEMPS, NUMFASTWORDS, DisplayLevel, LabelNumber, 
    EvalMode, EvalModeSet, EnumList, EnumNode, MemoryOffset,
    PointerKind, CheckKind, GlobalSymKind, ConstNode, ParamKind,
    TypeNode, VarNode, FieldNode, VariantNode, ModuleNode,
    ExprNode, ExprList, StmtNode, StmtList, CaseNode, CaseTreeNode,
    ConstSetNode, ConstSetList, ExprSetNode, ParamNode, ParamList,
    CodeNode, GlobalVarNode, SYMTYPE, MemoryType, DataType, ProcNode,
    MODDEFINITION, BIPNOTBIP, ExprKind, Address,
    StmtKind, integerTypeNode, charTypeNode, cardinalTypeNode,
    bitsetTypeNode, booleanTypeNode, addressTypeNode, stringTypeNode,
    cardIntTypeNode, procTypeNode, realTypeNode,
    globalModule, globalProc, currLine,
    currFile, FormerNode, FormerElementNode, FormerElementKind,
    InlineParamNode, InlineParamList, IPPARAM, TempNumber, OptNode, OptUsage;

from Errors import
    Error, ExprError, StmtError;

from Consts import
    ConstSet, OrdOf, DefaultExitStatus;

from TypeInfo import
    NumberOf, LowerBoundOf, SizeOf, WordSizeOf, ReferenceByPoint, BaseType;

from Alloc import
    globalVarList, RoundUp;

from BuildExpr import
    WriteExpr;

$if pascal then
from CheckExpr import
    MakeExprConstString;
$end

from OCount import
    EvalState, InitTemps, UpdateTemps, AllocTemp, FreeTemp;

from PCodeOps import
    PCodeOp;

from PCode import
    InitPcode, GenOp, Lab, GenT, GenMt, I,
    GenOpL, GenOpTL, GenOpT, NewLabel, GenString, W, C,
    GenReal, GenSet, X, EndLine, operPcode;

from GenCode import
    gprofFlag, genCountFlag, GenCode, GenProcName, GenAddress, codeFile;

from BuiltinPC import
    GenBuiltin;

from SymbolDump import
    InitStab, StabGlobalPort, StabProc, StabModule, StabScope, StabLine;

const
    MAINPROGNAME = 'main';

const
    (* ||| This should be shared with xlate *)
    NUMREGS = 6;

type
    ExitInfo = record
	exitLabel : LabelNumber;
	loopLevel : integer;
    end;
var
    loopExit, forExit, whileExit, repeatExit : ExitInfo;
    currLevel : DisplayLevel;
    loopNestLevel, exitLevel : integer;
    genProcNode : ProcNode;
    currCounter, currCounterLine : integer;
    exited, returned : boolean;
    dummyVar: VarNode;

procedure GenLibCall(const name		 : array of char;
		     const retType       : TypeNode; 
		     const retSize       : MemoryOffset;
		     const numParamWords : integer);
begin
    GenOp(PCCEP);
    GenT(retType);
    X;
    I(retSize);
    X;
    I(numParamWords);
    X;
    W(name);
    EndLine;
end GenLibCall;


procedure GenCounter(line : integer; stl : StmtList);
begin
    if genCountFlag then
	if stl # nil then
	    if stl^.first # nil then
		line := stl^.first^.lineNumber;
	    end;
	end;
	currCounter := currCounter + 1;
	GenOp(PCCTS); C('c'); X; I(currCounter);
		X; I(line); EndLine;
	if line < currCounterLine then
	    Error('Line numbers out of order?');
	end;
    end;
end GenCounter;

procedure GenInitialValue(value : FormerNode; baseOffset : MemoryOffset);
var
    fen : FormerElementNode;
    csn : ConstSetNode;
    limit, index, lowerBound, size : MemoryOffset;

    procedure GenEnumNameTable(enumList : EnumList);
	var
	    enum		: EnumNode;
	    offset		: MemoryOffset;
	    numTableEntries     : integer;
	    shortWordIndex      : integer;
	    stringSize		: MemoryOffset;
    begin
	enum := enumList^.first;
	numTableEntries := enumList^.last^.enumOrd - enum^.enumOrd + 1;
	(* Number of entries in table *)
	offset := 0;
	GenOp(PCINI); I(offset); X; I(HALFSIZE);
	    X; C('i'); X; I(numTableEntries); EndLine;
	offset := offset + HALFSIZE;
    
	(* Generate the table of relative offsets of the start of each name *)
	shortWordIndex := 2 * (numTableEntries + 1);
	loop
	    GenOp(PCINI); I(offset); X; 
		I(HALFSIZE); X; C('i'); 
		X; I(shortWordIndex); EndLine;
	    offset := offset + HALFSIZE;
	    if enum = nil then 
		exit;
	    end;
	    shortWordIndex := shortWordIndex + enum^.name^.length + 1;
	    enum := enum^.next;
	end (* loop *);
    
	(* Now generate the actual characters of each name *)
	enum := enumList^.first;
	repeat
	    stringSize := BYTESIZE * (enum^.name^.length + 1);
	    GenOp(PCINI); I(offset); X; I(stringSize);
		X; C('s'); X; C('"');
		WriteString(codeFile, enum^.name); C('\\');
		C('0'); C('"'); EndLine;
	    offset := offset + stringSize;
	    enum := enum^.next;
	until enum = nil;
    end GenEnumNameTable;

    procedure GenInitialValueConst(cn : ConstNode; size, offset : integer);
    begin
        case cn^.kind of
	| DTINTEGER, DTCARDINAL :
	    GenOp(PCINI); I(offset); X; I(size);
	    X; C('i'); X; I(trunc(cn^.cardVal)); EndLine;
	| DTCHAR :
	    GenOp(PCINI); I(offset); X; I(size);
	    X; C('i'); X; 
	    I(ord(cn^.charVal)); EndLine;
	| DTBOOLEAN :
	    GenOp(PCINI); I(offset); X; I(size);
	    X; C('i'); X; 
	    I(ord(cn^.boolVal)); EndLine;
	| DTENUMERATION :
	    GenOp(PCINI); I(offset); X; I(size);
	    X; C('i'); X; 
	    I(cn^.enumVal^.enumOrd); EndLine;
	| DTREAL, DTLONGREAL :
	    GenOp(PCINI); I(offset); X; I(size); X;
	    if size = SizeOf(realTypeNode) then C('r') else C('R') end;
	    X; GenReal(cn^.realVal);
	    EndLine;
	| DTSTRING :
	    GenOp(PCINI); I(offset); X; I(size);
	    X; C('s'); X; C('"');
	    WriteStringConst(codeFile,cn^.strVal); C('"'); EndLine;
	| DTSET :
	    GenOp(PCINI); I(offset); X; I(size);
	    X; C('S'); X; GenSet(cn^.setVal); EndLine;
	| DTPROC :
	    GenOp(PCINI); I(offset); X; I(size);
	    X; C('p'); X; GenString(cn^.procVal^.globalName);
	    EndLine;
	| DTARRAY :
	    GenInitialValue(cn^.arrayVal,offset);
	| DTRECORD :
	    GenInitialValue(cn^.recordVal,offset);
	end (* case *);
    end GenInitialValueConst;

begin (* GenInitialValue *)
    case value^.kind of
    | FEENUMNAMETABLE :
	GenEnumNameTable(value^.formerType^.enumList);
	
    | FEARRAYCONST :
	lowerBound := trunc(LowerBoundOf(value^.formerType^.indexType));
	size := value^.formerType^.elementSize;
	fen := value^.value^.first;
	while fen # nil do 
	    csn := fen^.indexConst^.first;
	    while csn # nil do 
		index := trunc(OrdOf(csn^.lower));
		GenInitialValueConst(fen^.arrayConst,size,
			baseOffset+size*(index-lowerBound));
		if csn^.upper # nil then 
		    limit := trunc(OrdOf(csn^.upper));
		    index := index + 1;
		    while index <= limit do 
			GenInitialValueConst(fen^.arrayConst,size,
				baseOffset+size*(index-lowerBound));
			index := index + 1;
		    end;
		end;
		csn := csn^.next;
	    end;
	    fen := fen^.next;
	end;
    | FERECORDCONST :
	fen := value^.value^.first;
	while fen # nil do
	    (* ||| There's gotta be a better way to do this *)
	    if (fen^.recordConst^.kind = DTREAL) and
		(BaseType(fen^.recordField^.fieldType)^.kind = DTLONGREAL) then
		fen^.recordConst^.kind := DTLONGREAL;
	    end; (* CED 6/17/88 - Make sure xlate gets the right type *)
	    GenInitialValueConst(fen^.recordConst,fen^.recordField^.size,
		    baseOffset+fen^.recordField^.offset);
	    fen := fen^.next;
	end;
    | FECONST :
	fen := value^.value^.first;
	if fen^.tagsConst # nil then
	    Error('Unexpected tags?');
	end;
	if fen^.next # nil then
	    Error('Unexpected values?');
	end;
	if (fen^.valueConst^.kind = DTREAL) and
	    (BaseType(value^.formerType)^.kind = DTLONGREAL) then
	    fen^.valueConst^.kind := DTLONGREAL;
	end; (* CED 6/17/88 - Make sure xlate gets the right type *)
	GenInitialValueConst(fen^.valueConst,value^.formerType^.size,
		    baseOffset);
    end (* case *);
end GenInitialValue;

procedure GenComs;
var
    gvn : GlobalVarNode;
begin
    gvn := globalVarList^.first;
    while gvn # nil do
	if gvn^.used or gvn^.defineMemory then
	    GenOp(PCCOM);
	    I(gvn^.number);
	    X;
	    GenString(gvn^.globalName);
	    X;
	    I(RoundUp(gvn^.size,WORDSIZE));
	    X;
	    if gvn^.value # nil then
		if gvn^.extern = GSPRIVATE then
		    C('I');
		else
		    C('i');
		end;
		EndLine;
		GenInitialValue(gvn^.value, 0);
		GenOp(PCINI); I(-1); EndLine;
	    elsif gvn^.defineMemory then
		if gvn^.extern = GSEXTERNAL then
		    C('e');
		else
		    C('d');
		end;
		EndLine;
	    else
		C('e');
		EndLine;
	    end;
	end;
	gvn := gvn^.next;
    end;
end GenComs;

procedure GenTagList(csl : ConstSetList);
var
    csn : ConstSetNode;
begin
    if csl # nil then
	csn := csl^.first;
	while csn # nil do
	    I(trunc(OrdOf(csn^.lower)));
	    if (csn^.upper # nil) and (csn^.upper # csn^.lower) then
		C(':');
		I(trunc(OrdOf(csn^.upper)));
	    end;
	    csn := csn^.next;
	    if csn # nil then
		C(';');
	    end;
	end;
    end;
end GenTagList;


procedure IsRegister(const address : Address) : Boolean;
(* Return true if MEMFAST address will be turned into register by p-code
   translator.  This is a gross kludge, but SymbolDumpPC and CheckStmtFor
   depend on it. *)
begin
     return (address.kind = MEMFAST) and (address.proc^.tempMap # nil)
      and (address.offset < (NUMFASTWORDS * WORDSIZE)) and
      (address.proc^.tempMap^.map[(address.offset div WORDSIZE) + NUMOPTTEMPS]
       < (NUMREGS * WORDSIZE));
end IsRegister;



procedure GenAddress(const vn : VarNode);
begin
    with vn^ do
        if address.kind = MEMGLOBAL then
	    GenMt(address.kind);
	    X;
	    I(0);
	    X;
	    I(address.gvn^.number);
        elsif (address.kind = MEMFAST) and (address.proc^.tempMap # nil) then
	    GenMt(address.kind);
	    X;
	    if address.offset < (NUMFASTWORDS*WORDSIZE) then
		(* in reassigned storage, map it to proper place *)
		I(address.proc^.tempMap^.map[
		    (address.offset div WORDSIZE)+NUMOPTTEMPS]);
	    else
		(* beyond reassigned storage, account for opt temps in front *)
		I(address.proc^.tempMap^.numOptTemp*WORDSIZE
			+ address.offset);
	    end;
	    X;
	    I(address.proc^.block);
        elsif address.kind = MEMINLINE then
	    Error('GenAddress: inline?');
	else
	    GenMt(address.kind);
	    X;
	    I(address.offset);
	    X;
	    I(address.proc^.block);
 	end;
   end;
end GenAddress;

procedure GenVarT(vn : VarNode; tn : TypeNode; mode : EvalMode);
begin
    if vn^.address.kind = MEMGLOBAL then
	case mode of
	| EVALGET   : GenOp(PCLDO)
	| EVALPUT   : GenOp(PCSRO)
	| EVALPOINT : GenOp(PCLAO)
	end;
	if mode # EVALPOINT then
	    GenT(tn);
	    X;
	end;
	I(SizeOf(tn));
	X;
	GenAddress(vn);
	EndLine;
    else
	case mode of
	| EVALGET   : GenOp(PCLOD)
	| EVALPUT   : GenOp(PCSTR)
	| EVALPOINT : GenOp(PCLDA)
	end;
	if mode # EVALPOINT then
	    GenT(tn);
	    X;
	end;
	I(SizeOf(tn));
	X;
	I(currLevel - vn^.address.level);
	X;
	GenAddress(vn);
	EndLine;
    end;
end GenVarT;

procedure GenVar(vn : VarNode; mode : EvalMode);
begin
    GenVarT(vn,vn^.varType,mode);
end GenVar;

procedure GenTemp(op : PCodeOp; on : OptNode; en : ExprNode);
var
    tn : TypeNode;
begin
    with dummyVar^ do
	address.kind := MEMFAST;
        address.level := currLevel;
        address.proc := genProcNode;
        address.offset := (on^.tempNumber-1-NUMOPTTEMPS) * WORDSIZE;
    end;
    tn := en^.exprType;
    GenOpT(op,tn); X; 
	I(RoundUp(SizeOf(tn), WORDSIZE)); X; I(0);
	X; GenAddress(dummyVar); EndLine;
end GenTemp;

procedure GenStore(en : ExprNode; tn : TypeNode);
begin
    if en^.kind = EXPRVAR then
	GenVarT(en^.exprVar,tn,EVALPUT);
    else
	GenExpr(en,EVALGET);
	GenIndirectVar(tn,EVALPUT);
    end;
end GenStore;

procedure GenConstAddress(i : integer);
begin
    GenOpT(PCLDC,addressTypeNode); X; I(WORDSIZE); X; I(i); EndLine;
end GenConstAddress;

procedure GenConstInteger(i : integer);
begin
    GenOpT(PCLDC,integerTypeNode); X; I(WORDSIZE); X; I(i); EndLine;
end GenConstInteger;

procedure GenConstBoolean(b : boolean);
begin
    GenOpT(PCLDC,booleanTypeNode); X; I(BOOLEANSIZE); X; I(ord(b)); EndLine;
end GenConstBoolean;

procedure GenExprCheck(en : ExprNode; mode : EvalMode);
var
    tagFn : FieldNode;
    vn : VariantNode;
begin
    GenExpr(en^.checkExpr,mode);
    if en^.doCheck then
	case en^.exprCheck of
	| CHECKSUBSCR :
	    GenOp(PCCHK); C('s'); X; GenT(en^.checkType); X;
		    I(en^.checkLower); X;
		    I(en^.checkUpper);
	    EndLine;
	
	| CHECKSUBSCROPEN :
	    if en^.checkUpper = 0 then
		GenVar(en^.arrVar,EVALPOINT);
	    else
		GenVar(en^.arrVar,EVALGET);
	    end;
	    GenOpT(PCINC,addressTypeNode); X; I(en^.checkLower); EndLine;
	    GenIndirectVar(cardIntTypeNode,EVALGET);
	    GenOp(PCCHK); C('o'); EndLine;
	
	| CHECKSUBARRAY :
	    (* already have first count, get subscript and limit *)
	    GenVar(en^.checkVar, EVALGET);
	    if en^.arrVar = nil then
		GenConstInteger(en^.checkLower+1); (* fixed array *)
	    else
		if en^.checkUpper = 0 then	(* open array *)
		    GenVar(en^.arrVar,EVALPOINT);
		else
		    GenVar(en^.arrVar,EVALGET);
		end;
		GenOpT(PCINC,addressTypeNode); X; I(en^.checkLower); EndLine;
		GenIndirectVar(cardIntTypeNode,EVALGET);
	    end;
	    GenOp(PCCHK); C('x'); EndLine;
	
	| CHECKRANGE :
	    GenOp(PCCHK); C('r'); X; GenT(en^.checkType); X;
		    I(en^.checkLower); X; I(en^.checkUpper); EndLine;
	
	| CHECKPTRREF, CHECKDYNARRAY :
	    if en^.checkPtr = PTRNOCHECK then
		(* do nothing *)
	    else
		GenOp(PCCHK);
		if en^.exprCheck = CHECKPTRREF then
		    C('a');
		else
		    C('d');
		end;
		X;
		if en^.checkPtr = PTRMODULA then
		    C('m');
		elsif en^.checkPtr = PTRPASCAL then
		    C('p');
		else
		    C('n');
		end;
		EndLine;
	    end;
	
	| CHECKVARIANT :
	    vn := en^.checkVariant;
	    while vn # nil do
		tagFn := vn^.tagField;
		if tagFn^.offset >= 0 then
		    (* tag field exists, generate tag values *)
		    GenOp(PCCHK); C('v'); X;
			I(tagFn^.offset+en^.checkLower); X;
			I(SizeOf(tagFn^.fieldType)); X;
		    if vn^.elsePart then (* else variant - want inverse test *)
			C('~');
		    end;
		    GenTagList(vn^.tag);
		    EndLine;
		end;
		vn := tagFn^.containingVariant;
	    end;
	    
	end;
    end;
end GenExprCheck;

procedure GenStmtList(stmts : StmtList);
var
    stn : StmtNode;
    listExited, listReturned : boolean;
begin
    listExited := false;
    listReturned := false;
    if stmts # nil then
	stn := stmts^.first;
	while stn # nil do
	    GenStmt(stn);
	    stn := stn^.next;
	    if (stn # nil) and (exited or returned) then
		GenCounter(stn^.lineNumber,nil);
		listExited := listExited or exited;
		listReturned := listReturned or returned;
		exited := false;
		returned := false;
	    end;
	end;
    end;
    exited := exited or listExited;
    returned := returned or listReturned;
end GenStmtList;

procedure GenParamCopies(params : ParamList);
var
    param   : ParamNode;
    atn     : TypeNode;
    i       : integer;
    rowSize : MemoryOffset;
begin
    param := params^.first;
    while param # nil do
	(* if it is a value open array, must copy it onto stack *)
	if param^.kind = PARAMARRAYVALUE then
	    (* point to parameter address *)
	    GenVarT(param^.paramVar,addressTypeNode,EVALPOINT);

	    (* save variable address *)
	    GenOp(PCSAV); I(0); X; C('m'); EndLine;
	    (* get total number elements in array *)
	    atn := param^.paramType;
	    for i := 1 to param^.paramType^.descripCount do
		(* get address of pointer variable *)
		GenOp(PCUSE); I(0); X; C('c'); EndLine;
		GenOpT(PCINC,addressTypeNode); X; I(i*WORDSIZE); EndLine;
		(* get count *)
		GenIndirectVar(cardIntTypeNode,EVALGET);
		if i > 1 then
		    (* multiply by previous values *)
		    GenOpT(PCMUP,cardIntTypeNode); X; I(WORDSIZE); EndLine;
		end;
		rowSize := atn^.elementSize;
		atn := atn^.elementType;
	    end;
	    GenConstAddress(rowSize);
	    (* multiply by element size *)
	    GenOpT(PCMUP,addressTypeNode); X; I(WORDSIZE); EndLine;
	    (* save size *)
	    GenOp(PCSAV); I(1); X; C('m'); EndLine;
	    (* get addrss, size *)
	    GenOp(PCUSE); I(0); X; C('m'); EndLine;
	    GenOp(PCUSE); I(1); X; C('m'); EndLine;
	    (* allocate space on stack, copy parameter value *)
	    GenOp(PCSAC); EndLine;
	elsif (param^.kind = PARAMVALUE) and param^.reference then
	    GenVarT(param^.paramVar,addressTypeNode,EVALPOINT);
	    GenConstAddress(WordSizeOf(param^.paramType));
	    (* allocate space on stack, copy parameter value *)
	    GenOp(PCSAC); EndLine;
	end;
	param := param^.next;
    end;
end GenParamCopies;

procedure GenGlobalProc(proc: ProcNode);
var
    submod : ModuleNode;
    oklabel : LabelNumber;
begin
    if proc^.initFlagVar # nil then
	GenVar(proc^.initFlagVar,EVALGET);
	oklabel := NewLabel();
	GenOp(PCFJP); Lab(oklabel); EndLine;
	GenOpTL(PCRET,nil);
	Lab(oklabel); GenOpL(PCLAB);
	GenConstBoolean(true);
	GenVar(proc^.initFlagVar,EVALPUT);
    end;
    if genCountFlag then
	GenOp(PCCTS); C('i'); EndLine;
    end;

    (* generate initialization calls if this is global proc *)
    submod := globalModule^.modules^.first;
    while (submod # nil) do
	if (submod^.kind = MODDEFINITION) and 
		submod^.doinit and not submod^.noinit then
	    GenOp(PCMST); I(0); EndLine;
	    AddString(submod^.name);
	    AddChar('_');
	    GenCall(false, NonHashText(MODULEINITNAME), procTypeNode, 0);
	end;
	submod := submod^.next;
    end;
end GenGlobalProc;

procedure GenProcName(const proc : ProcNode);
begin
    if (proc = globalProc) and (proc^.globalName = nil) then
	(* program module *)
	W(MAINPROGNAME);
    else
	GenString(proc^.globalName);
    end;
end GenProcName;

procedure GenProcEntry(proc : ProcNode);
var
    numParams : integer;
    param : ParamNode;
$if pascal then
    vn  : VarNode;
    fvn : FileVarNode;
$end
begin
    genProcNode := proc;
    StabProc(proc);
    StabLine(currFile,currLine);
    GenProcName(proc);
    GenOpT(PCENT,proc^.procType^.funcType);
    X;
    if proc^.procType^.funcType # nil then
	I(SizeOf(proc^.procType^.funcType));
    else
	I(0);
    end;
    X;
    I(proc^.displayLevel);
    X;
    I(proc^.block);
    X;
    numParams := 0;
    if proc^.procType^.paramList # nil then
	param := proc^.procType^.paramList^.first;
	while param # nil do
	    numParams := numParams + 1;
	    param := param^.next;
	end;
    end;
    I(numParams);
    X;
    if genDebugInfoFlag then
	I(proc^.lineNumber);
    else
	I(0);
    end;
    X;
    if optimFlag then
	I(ord(not (proc^.displayLevel in proc^.containsUpLevel)));
    else
	I(ord(not proc^.containsProcs));
    end;
    I(ord((proc # globalProc) and (
	    ((proc^.extern = GSNORMAL) and internalCallFlag)
	    or (proc^.internalProc) and not OptNcall)));
    X;
    if (proc^.tempMap = nil) or OptNreg then
	I(-1);
    else
	I(proc^.tempMap^.numReg+1);
    end;
    EndLine;
    GenOp(PCDEF);
    I(RoundUp(proc^.mem^.maximum[MEMPARAM],WORDSIZE));
    X;
    I(RoundUp(proc^.mem^.maximum[MEMFAST],WORDSIZE));
    X;
    I(RoundUp(proc^.mem^.maximum[MEMNORMAL],WORDSIZE));
    X;
    I(proc^.block);
    X;
    GenProcName(proc);
    EndLine;
    if proc # globalProc then
	StabScope(proc^.scope);
    end;
    if proc^.tailRecursion then
	proc^.tailRecursionEntry := NewLabel();
	Lab(proc^.tailRecursionEntry); GenOpL(PCLAB);
    end;
    currLevel := proc^.displayLevel;
    if proc^.procType^.paramList # nil then
	GenParamCopies(proc^.procType^.paramList);
    end;
    if proc = globalProc then
	GenGlobalProc(proc);
    end;
$if pascal then
    if proc = globalProc then       (* special Pascal entry code *)
	GenOp(PCMST); I(1); EndLine;
	GenConstInteger(ord(proc^.doCheck));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('PCSTART', nil, 0, 1);
	if bufferFlag # 1 then	(* File output buffering option *)
	    GenOp(PCMST); I(1); EndLine;
	    GenConstInteger(bufferFlag);
	    GenOp(PCPAR); I(0); EndLine;
	    GenLibCall('BUFF', nil, 0, 1);
	end;
	if programFileVarList # nil then
	    fvn := programFileVarList^.first;
	    while fvn # nil do
		(* DEFNAME(file, name, namelength, datasize) *)
		vn := fvn^.fileVar;
		GenOp(PCMST); I(4); EndLine;
		GenVarT(vn, vn^.varType, EVALPOINT);
		GenOp(PCPAR); I(0); EndLine;
		GenExprString(MakeExprConstString(vn^.name), EVALPOINT);
		GenOp(PCPAR); I(1); EndLine;
		GenConstInteger(vn^.name^.length);
		GenOp(PCPAR); I(2); EndLine;
		if vn^.varType^.isTextFile then
		    GenConstInteger(0);
		else
		    GenConstInteger(
		       (SizeOf(vn^.varType^.fileType)+BYTESIZE-1) div BYTESIZE);
		end;
		GenOp(PCPAR); I(3); EndLine;
		GenLibCall('DEFNAME', nil, 0, 4);
		fvn := fvn^.next;
	    end (* while *);
	end (* if files declared in program header *);
    end (* if proc = globalProc *);
    if proc^.OOBLabelList # nil then (* Save state for OOB goto *)
	GenOpL(PCSST);
    end;
$end
end GenProcEntry;

procedure GenProcExit(proc : ProcNode);
begin
$if pascal then
    if proc = globalProc then   (* special Pascal exit code *)
	GenOp(PCMST); I(1); EndLine;
	GenConstInteger(trunc(DefaultExitStatus));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('PCEXIT', nil, 0, 1);
    elsif proc^.containsFiles then
	GenOp(PCMST); I(1); EndLine;
	GenOp(PCLDA);	  (* Gross hack to load ap on stack *)
	I(WORDSIZE); X;    (* Size *)
	I(0); X;	    (* Relative display level *)
	C('p'); X;	    (* Parameter memory *)
	I(-WORDSIZE); X;   (* Offset from # params passed *)
	I(proc^.block); (* Block # *)
	EndLine;
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('PCLOSE', nil, 0, 1);
    end;
$end	   
    if (proc = globalProc) and (proc^.globalName = nil) then
	GenOp(PCMST); I(1); EndLine;
        GenConstInteger(trunc(DefaultExitStatus));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('runtime__term', nil, 0, 1);
    elsif proc^.procType^.funcType = nil then
	GenOpTL(PCRET,nil);
    else
$if modula2 then
	GenOpTL(PCCHK,procTypeNode);
$else (* pascal *)
	(* Load and return proc^.returnVar *)
	GenVar(proc^.returnVar, EVALGET);
	GenOpTL(PCRET, proc^.procType^.funcType);
$end
    end;
    GenOpL(PCEXI);
end GenProcExit;

procedure GenProc(proc : ProcNode);
var
    code : CodeNode;
    counted : boolean;
$if pascal then
    vn  : VarNode;
    fvn : FileVarNode;
$end

begin
    currFile := proc^.fileName;
    currLine := proc^.lineNumber;
    counted := false;
    InitTemps;
    GenProcEntry(proc);
    returned := false;
    exited := false;
    exitLevel := 1000;
    code := proc^.code^.first;
    while code # nil do
	if not counted and (code^.stmts # nil) then
	    if code^.stmts^.first # nil then
		GenCounter(proc^.lineNumber,code^.stmts);
		counted := true;
	    end;
	end;
	GenStmtList(code^.stmts);
	code := code^.next;
    end;
    GenProcExit(proc);
end GenProc;

procedure GenModule (theModule : ModuleNode);
var
    proc : ProcNode;
    submod : ModuleNode;
    code : CodeNode;
begin
    if theModule^.kind # MODDEFINITION then
	if theModule # globalModule then
	    StabModule(theModule);
	    if theModule^.enclosing = globalModule then
		StabGlobalPort;
	    end;
	    StabScope(theModule^.scope);
	end;
	submod := theModule^.modules^.first;
	while submod # nil do
	    GenModule(submod);
	    submod := submod^.next;
	end;
	proc := theModule^.procs^.first;
	while proc # nil do
	    if (proc^.builtin # BIPNOTBIP) or proc^.inlineProc
$if pascal then
		    (* No main prog code for compiles with no PROGRAM decl *)
		    or ((proc = globalProc) and (proc^.name # nil))
$end
	    then
		(* no code generation *)
$if modula2 then
	    elsif (proc = globalProc) and (proc^.enclosingModule^.noinit) then
		(* No code for NOINIT module body *)
		code := proc^.code^.first;
		while code # nil do
		    assert((code^.stmts = nil) or (code^.stmts^.first = nil),
			'Init code for NOINIT module?');
		    code := code^.next;
		end;
		submod := globalModule^.modules^.first;
		while (submod # nil) do
		    if (submod^.kind = MODDEFINITION) then
			assert(submod^.noinit, 'Import init to NOINIT module?');
		    end;
		    submod := submod^.next;
		end;
$end
	    else
		GenProc(proc);
	    end;
	    proc := proc^.next;
	end;
    end;
end GenModule;

procedure GenStmtAssign(stn : StmtNode);
begin
    if stn^.assignSizeCheck # nil then
	(* Check that string fits into dynamic array *)
	GenExpr(stn^.assignSizeCheck, EVALGET);
	(* Throw away top of stack *)
	GenOp(PCSAV); I(0); X; C('d'); EndLine;
    end;
    if stn^.assignOp = TKASSIGN then
	GenExpr(stn^.rhs,EVALGET);
	GenStore(stn^.lhs,stn^.lhsType);
    else
	GenExpr(stn^.rhs^.opnd1^.exprVal,EVALPOINT);
	GenExpr(stn^.rhs^.opnd2,EVALGET);
	case stn^.assignOp of
	| TKPLUS	: GenOp(PCAD2)
	| TKASTERISK    : GenOp(PCMP2)
	| TKMINUS       : GenOp(PCSB2)
	| TKDIV, 
	  TKSLASH       : GenOp(PCDV2)
	end;
	GenT(stn^.lhsType); X; I(SizeOf(stn^.lhsType)); EndLine;
    end;
end GenStmtAssign;

procedure GenParamList(procType : TypeNode; procVariable : ExprNode;
	params : ExprList) : integer;
var
    pn		  : ParamNode;
    pen, den	  : ExprNode;
    numParamWords : cardinal;
begin
    numParamWords := 0;
    if (procType^.paramList = nil) or (procType^.paramList^.first = nil) then
	GenOp(PCMST); I(0); EndLine;
    else
	pn := procType^.paramList^.first;
	while pn # nil do
	    case pn^.kind of
	    | PARAMARRAYVALUE, PARAMARRAYVAR, PARAMARRAYCONST :
		(* One word for array base, plus words for size/stride(s) *)
		inc(numParamWords, pn^.paramType^.descripCount + 1);
	    | PARAMVAR :
		inc(numParamWords);
	    | PARAMVALUE, PARAMCONST :
		inc(numParamWords);
		if pn^.paramType^.kind = DTLONGREAL then
		    inc(numParamWords);
		end;
	    end (* case *);
	    pn := pn^.next;
	end (* while *);
	GenOp(PCMST); I(numParamWords); EndLine;

	(* Now actually evaluate and push the arguments *)
	numParamWords := 0;
	pen := params^.first;
	pn := procType^.paramList^.first;
	while (pn # nil) and (pen # nil) do
	    case pn^.kind of
	    | PARAMARRAYVALUE, PARAMARRAYVAR, PARAMARRAYCONST :
		GenExpr(pen^.descripBase, pen^.descripMode);
		GenOp(PCPAR); I(numParamWords); EndLine;
		inc(numParamWords);
		assert(pn^.paramType^.descripCount = pen^.descripCount,
		    'Bad descripCount match in GenParamList');
		den := pen^.descrips^.first;
		while den # nil do
		    GenExpr(den, EVALGET);
		    GenOp(PCPAR); I(numParamWords); EndLine;
		    inc(numParamWords);
		    den := den^.next;
		end;

	    | PARAMVAR, PARAMVALUE, PARAMCONST :
		if pn^.reference then
		    GenExpr(pen,EVALPOINT);
		    GenOp(PCPAR); I(numParamWords); EndLine;
		else
		    GenExpr(pen,EVALGET);
		    GenOp(PCPAR); I(numParamWords); EndLine;
		    if pn^.paramType^.kind = DTLONGREAL then
			(* longreals are two words *)
			inc(numParamWords);
		    end;
		end;
		inc(numParamWords);
	    end;
	    pen := pen^.next;
	    pn := pn^.next;
	end;
    end;
    if procVariable <> nil then
	(* invocation of procedure variable, make it last parameter *)
	GenExpr(procVariable,EVALGET);
    end;
    return numParamWords;
end GenParamList;

procedure GenCall(internal : boolean; procName : String; procType : TypeNode; 
		    numParams : integer);
begin
    if internal then
	GenOp(PCCUP);
    else
	GenOp(PCCEP);
    end;
    GenT(procType^.funcType);
    X;
    if procType^.funcType # nil then
	I(SizeOf(procType^.funcType));
    else
	I(0);
    end;
    X;
    I(numParams);
    X;
    GenString(procName);
    EndLine;
end GenCall;

procedure GenTailRecursion(proc : ProcNode; params : ExprList);
const
    MAXTAILPARAMS = 50;	(* maximum number of parameters *)
var
    pn        : ParamNode;
    pen, en   : ExprNode;
    pnum      : integer;
    temps     : array [1..MAXTAILPARAMS] of integer;
    tempCount : integer;
    skip      : boolean;
    sameVar   : VarNode;
    atn       : TypeNode;
begin
    if (params # nil) and (proc^.procType^.paramList # nil) then
	(* first evaluate all parameters (except those that are the same) *)
	pn := proc^.procType^.paramList^.first;
	pen := params^.first;
	pnum := 0;
	tempCount := 0;
	while pn # nil do
	    pnum := pnum + 1;
	    case pn^.kind of
	    | PARAMARRAYVALUE, PARAMARRAYVAR, PARAMARRAYCONST :
		ExprError(pen, 'Tail recursion with open array?');
	    
	    | PARAMVAR :
		en := pen;
		if en^.kind = EXPRVAL then
		    (* By reference has EXPRVAL over it, so move down *)
		    en := en^.exprVal;
		end;
		if (en^.kind = EXPRVAR) and (en^.exprVar = pn^.paramVar) then
		    temps[pnum] := 0;
		else
		    GenExpr(pen, EVALPOINT);
		    tempCount := tempCount + 1;
		    temps[pnum] := tempCount;
		    GenOp(PCSAV); I(temps[pnum]); X; C('m'); EndLine;
		end;
	    
	    | PARAMVALUE, PARAMCONST :
		en := pen;
		if en^.kind = EXPRCHECK then
		    en := en^.checkExpr;
		end;
		if (en^.kind = EXPRVAL) and (en^.exprVal^.kind = EXPRVAR) and
			(en^.exprVal^.exprVar = pn^.paramVar) then
		    temps[pnum] := 0;
		else
		    GenExpr(pen, EVALGET);
		    tempCount := tempCount + 1;
		    temps[pnum] := tempCount;
		    GenOp(PCSAV); I(temps[pnum]); X; C('m'); EndLine;
		end;
		
	    end;
	    pen := pen^.next;
	    pn := pn^.next;
	end;
	(* now store them into the parameter list *)
	pn := proc^.procType^.paramList^.first;
	pen := params^.first;
	pnum := 0;
	while pn # nil do
	    pnum := pnum + 1;
	    case pn^.kind of
	    | PARAMARRAYVALUE, PARAMARRAYVAR, PARAMARRAYCONST :
	    
	    | PARAMVAR :
		if temps[pnum] # 0 then
		    GenOp(PCUSE); I(temps[pnum]); X; C('m'); EndLine;
		    GenVarT(pn^.paramVar, addressTypeNode, EVALPUT);
		end;
	    
	    | PARAMVALUE, PARAMCONST :
		if temps[pnum] # 0 then
		    GenOp(PCUSE); I(temps[pnum]); X; C('m'); EndLine;
		    GenVar(pn^.paramVar, EVALPUT);
		end;
	    end (* case *);
	    pen := pen^.next;
	    pn := pn^.next;
	end;
    end;
    GenOp(PCUJP); Lab(proc^.tailRecursionEntry); EndLine;
end GenTailRecursion;

procedure GenFuncProc(procExpr    : ExprNode; 
		      params	  : ExprList; 
		      selected    : boolean;
		      addrOfValue : VarNode;
		      mode        : EvalMode);
var
    proc : ProcNode;
    pn : ParamNode;
    numParams : integer;
begin
    if (procExpr^.kind = EXPRSYM) and (procExpr^.exprSym^.kind = SYMTYPE) then
	GenExpr(params^.first,mode);
	if mode = EVALGET then
	    GenOpTL(PCTYP,procExpr^.exprSym^.symType);
	end;
	return;
    elsif procExpr^.kind # EXPRCONST then
	numParams := GenParamList(procExpr^.exprType,procExpr,params);
	GenOpT(PCCIP,procExpr^.exprType^.funcType); X;
	    if procExpr^.exprType^.funcType # nil then
		I(SizeOf(procExpr^.exprType^.funcType));
	    else
		I(0);
	    end;
	    X;
	    I(numParams); EndLine;
    else
	proc := procExpr^.exprConst^.procVal;
	if proc^.builtin # BIPNOTBIP then
	    GenBuiltin(proc,params);
	else
	    if optimFlag and procExpr^.opt^.tailRecursion then
		GenTailRecursion(proc,params);
	    else
		numParams := GenParamList(proc^.procType,nil,params);
		GenCall (((proc^.extern = GSNORMAL) and internalCallFlag)
		    or ((proc^.internalProc) and not OptNcall),
		    proc^.globalName,proc^.procType,numParams);
	    end;
	end;
    end;
    if selected then
	if addrOfValue = nil then
	    GenOpL(PCADR);  (* Convince xlate we really have a pointer *)
	else
	    (* Store value away, then load pointer to it *)
	    GenVarT(addrOfValue, procExpr^.exprType^.funcType, EVALPUT);
	    GenVarT(addrOfValue, procExpr^.exprType^.funcType, EVALPOINT);
	end;
    end;
end GenFuncProc;

procedure @inline GenStmtProc(const stn : StmtNode);
begin
    GenFuncProc(stn^.proc,stn^.params, false, nil, EVALGET);
end GenStmtProc;


procedure GenStmtIf(stn : StmtNode);
var
    elseLabel, endLabel : LabelNumber;
    elsePresent : boolean;
begin
    elsePresent := stn^.elseList # nil;
    if elsePresent then
	elsePresent := stn^.elseList^.first # nil;
    end;
    endLabel := NewLabel();
    if elsePresent then
	elseLabel := NewLabel();
	GenCondition(stn^.ifCond,NULLLABEL,elseLabel);
	GenCounter(stn^.lineNumber,stn^.thenList);
	GenStmtList(stn^.thenList);
	GenOp(PCUJP); Lab(endLabel); EndLine;
	Lab(elseLabel); GenOpL(PCLAB);
	GenCounter(stn^.lineNumber,stn^.elseList);
	GenStmtList(stn^.elseList);
    else
	GenCondition(stn^.ifCond,NULLLABEL,endLabel);
	GenCounter(stn^.lineNumber,stn^.thenList);
	GenStmtList(stn^.thenList);
    end;
    Lab(endLabel); GenOpL(PCLAB);
end GenStmtIf;

procedure GenCaseTable(tree : CaseTreeNode; minval, maxval : HugeInteger;
	elseLabel : LabelNumber);
var
    i : HugeInteger;
begin
    if tree = nil then
	i := minval;
	while i <= maxval do
	    GenOp(PCCJP); Lab(elseLabel); EndLine;
	    i := i + 1.0;
	end;
    else
	GenCaseTable(tree^.lower,minval,tree^.first-1.0,elseLabel);
	i := tree^.first;
	while i <= tree^.last do
	    GenOp(PCCJP); Lab(tree^.caseNode^.pcodeLabel); EndLine;
	    i := i + 1.0;
	end;
	GenCaseTable(tree^.higher,tree^.last+1.0,maxval,elseLabel);
    end;
end GenCaseTable;

procedure GenStmtCase(stn : StmtNode);
var
    caseNode : CaseNode;
    top, elseLabel, table, bottom : LabelNumber;
    node : CaseTreeNode;
    minval, maxval : HugeInteger;
begin
    if stn^.cases = nil then
	(* Just generate selector, throw it away, then execute PCCHK or
	   the else statement. *)
	GenExpr(stn^.caseSel, EVALGET);
	(* Throw away top of stack *)
	GenOp(PCSAV); I(0); X; C('d'); EndLine;
	if stn^.caseElse = nil then
	    if stn^.caseSel^.doCheck then
		GenOp(PCCHK); C('c'); EndLine;
	    end;
	else
	    GenCounter(stn^.lineNumber,stn^.caseElse);
	    GenStmtList(stn^.caseElse);
	end;

    else (* Normal situation, non-empty case *)
	top := NewLabel();
	elseLabel := NewLabel();
	table := NewLabel();
	bottom := NewLabel();
	GenOp(PCUJP); Lab(top); EndLine;
	caseNode := stn^.cases^.first;
	while caseNode # nil do
	    caseNode^.pcodeLabel := NewLabel();
	    Lab(caseNode^.pcodeLabel); GenOpL(PCLAB);
	    GenCounter(stn^.lineNumber,caseNode^.stmts);
	    GenStmtList(caseNode^.stmts);
	    GenOp(PCUJP); Lab(bottom); EndLine;
	    caseNode := caseNode^.next;
	end;
	Lab(elseLabel); GenOpL(PCLAB);
	if stn^.caseElse = nil then
	    if stn^.caseSel^.doCheck then
		GenOp(PCCHK); C('c'); EndLine;
	    else
		elseLabel := bottom;
	    end;
	else
	    GenCounter(stn^.lineNumber,stn^.caseElse);
	    GenStmtList(stn^.caseElse);
	    GenOp(PCUJP); Lab(bottom); EndLine;
	end;
	node := stn^.caseTree;
	repeat
	    minval := node^.first;
	    node := node^.lower;
	until node = nil;
	node := stn^.caseTree;
	repeat
	    maxval := node^.last;
	    node := node^.higher;
	until node = nil;
	Lab(top); GenOpL(PCLAB);
	GenExpr(stn^.caseSel,EVALGET);
	GenOp(PCXJP); Lab(table); X; Lab(elseLabel); X;
	    I(trunc(minval)); X; I(trunc(maxval)); EndLine;
	Lab(table); GenOpL(PCLAB);
	GenCaseTable(stn^.caseTree,minval,maxval,elseLabel);
	Lab(bottom); GenOpL(PCLAB);
    end;
end GenStmtCase;

procedure GenPrePostEval(el : ExprList;state : EvalState);
var
    en : ExprNode;
begin
    if el = nil then
	(* do nothing *)
    else
	en := el^.first;
	while en # nil do
	    OptGenExpr(en,EVALGET,state);
	    en := en^.next;
	end;
    end;
end GenPrePostEval;

procedure GenStmtWhile(stn : StmtNode);
var
    top, bottom : LabelNumber;
    save : ExitInfo;
begin
    top := NewLabel();
    bottom := NewLabel();
    save := whileExit;
    whileExit.exitLabel := bottom;
    whileExit.loopLevel := loopNestLevel;

    GenCondition(stn^.whileCond,NULLLABEL,bottom);
    GenPrePostEval(stn^.whilePreEval,EVALPRE);
Lab(top); GenOp(PCLAB); C('l'); EndLine;

    GenCounter(stn^.lineNumber,stn^.whileBody);

    loopNestLevel := loopNestLevel + 1;
    GenStmtList(stn^.whileBody);
    loopNestLevel := loopNestLevel - 1;

    GenCondition(stn^.whileCond,top,NULLLABEL);

Lab(bottom); GenOpL(PCLAB);

    GenPrePostEval(stn^.whilePreEval,EVALPOST);
    whileExit := save;
    if exitLevel > loopNestLevel then
	exited := false;
	exitLevel := 1000;
    end;
end GenStmtWhile;


procedure GenStmtRepeat(stn : StmtNode);
var
    top, bottom : LabelNumber;
    save : ExitInfo;
begin
    top := NewLabel();
    bottom := NewLabel();
    save := repeatExit;
    repeatExit.exitLabel := bottom;
    repeatExit.loopLevel := loopNestLevel;

    GenPrePostEval(stn^.repeatPreEval,EVALPRE);

Lab(top); GenOp(PCLAB); C('l'); EndLine;

    GenCounter(stn^.lineNumber,stn^.repeatBody);

    loopNestLevel := loopNestLevel + 1;
    GenStmtList(stn^.repeatBody);
    loopNestLevel := loopNestLevel - 1;

    GenCondition(stn^.repeatCond,NULLLABEL,top);

    Lab(bottom);
    GenOpL(PCLAB);

    GenPrePostEval(stn^.repeatPreEval,EVALPOST);

    repeatExit := save;
    if exitLevel > loopNestLevel then
	exited := false;
	exitLevel := 1000;
    end;
end GenStmtRepeat;


procedure GenStmtLoop(stn : StmtNode);
var
    top, bottom : LabelNumber;
    save : ExitInfo;
begin
    top := NewLabel();
    bottom := NewLabel();
    save := loopExit;
    loopExit.exitLabel := bottom;
    loopExit.loopLevel := loopNestLevel;

    GenPrePostEval(stn^.loopPreEval,EVALPRE);
Lab(top); GenOp(PCLAB); C('l'); EndLine;

    GenCounter(stn^.lineNumber,stn^.loopBody);

    loopNestLevel := loopNestLevel + 1;
    GenStmtList(stn^.loopBody);
    loopNestLevel := loopNestLevel - 1;

    GenOp(PCUJP);
    Lab(top);
    EndLine;

    Lab(bottom);
    GenOpL(PCLAB);

    GenPrePostEval(stn^.loopPreEval,EVALPOST);
    
    loopExit := save;
    if exitLevel > loopNestLevel then
	exited := false;
	exitLevel := 1000;
    end;
end GenStmtLoop;


procedure GenStmtStmts(stn : StmtNode);
begin
    GenStmtList(stn^.stmts);
end GenStmtStmts;


procedure GenStmtFor(stn : StmtNode);
var
    top, bottom : LabelNumber;
    increment   : integer;
    limit       : HugeInteger;
    compareop   : PCodeOp;
    cn		: ConstNode;
    tn		: TypeNode;
    variableInc, variableLimit : boolean;
    savedFrom   : boolean;
    save : ExitInfo;
begin
    top := NewLabel();
    bottom := NewLabel();
    save := forExit;
    forExit.exitLabel := bottom;
    forExit.loopLevel := loopNestLevel;

    if stn^.forBy^.kind = EXPRCONST then
	increment := trunc(OrdOf(stn^.forBy^.exprConst));
	variableInc := false;
    else
	variableInc := true;
    end;

    if (stn^.forTo^.kind = EXPRCONST) then
	limit := OrdOf(stn^.forTo^.exprConst);
	variableLimit := false;
    else
	variableLimit := true;
    end;

    tn := stn^.forIndexType;

    if variableLimit then
	GenExpr(stn^.forTo,EVALGET);
	GenVarT(stn^.forLimitVar,tn,EVALPUT);
    end;
    
    GenExpr(stn^.forFrom,EVALGET);
    savedFrom := false;
    if not stn^.forWillExecute then
	if IsRegister(stn^.forIndexVar^.address) then
		(* Don't bother, index var will be in register *)
	elsif stn^.forFrom^.kind = EXPRCONST then
	    (* Do save it...xlate won't register shuffle *)
	    savedFrom := true;
	elsif stn^.forIndexVar^.address.kind = MEMGLOBAL then
	    (* Don't bother with register, xlate shuffles too much *)
	else
	    savedFrom := true;
	end;
	if savedFrom then
	    GenOp(PCSAV); I(0); X; C('c'); EndLine;
	end;
    end;
    GenVarT(stn^.forIndexVar,tn,EVALPUT);

    if variableInc then
	GenExpr(stn^.forBy,EVALGET);
	GenVarT(stn^.forIncVar,tn,EVALPUT);
    elsif increment > 0 then
	compareop := PCLEQ;
    else
	compareop := PCGEQ;
    end;


    if not stn^.forWillExecute then
	(* see if loop ever executed *)
	if savedFrom then
	    GenOp(PCUSE); I(0); X; C('m'); EndLine;
	else
	    GenVarT(stn^.forIndexVar, tn, EVALGET);
	end;
	if variableLimit then
	    GenVarT(stn^.forLimitVar,tn,EVALGET);
	elsif tn^.kind = DTPOINTER then
	    GenOpT(PCLDC,tn); X; I(WORDSIZE); X;
		I(trunc(limit)); EndLine;
	else
	    GenOpT(PCLDC,cardIntTypeNode); X; I(WORDSIZE); X;
		I(trunc(limit)); EndLine;
	end;
	if variableInc then
	    GenOpTL(PCSUB,integerTypeNode);
	    GenVarT(stn^.forIncVar,tn,EVALGET);
	    GenOp(PCBIT); C('x'); EndLine; (* xor *)
	    GenConstInteger(0);
	    GenOpTL(PCLES,integerTypeNode); (* check for sign = 1 *)
	elsif tn^.kind = DTCARDINAL then
	    GenOpTL(compareop,integerTypeNode);
	else
	    GenOpTL(compareop,tn);
	end;
	GenOp(PCFJP);  Lab(bottom); EndLine;
    
	(* If here, for will execute at least once.  Range-check upper bound *)
	if stn^.forLimitCheck # nil then
	    GenExpr(stn^.forLimitCheck, EVALGET);
	    (* Throw away top of stack *)
	    GenOp(PCSAV); I(0); X; C('d'); EndLine;
	end;
    end;

    (* evaluate invariants *)
    GenPrePostEval(stn^.forPreEval,EVALPRE);

    Lab(top); GenOp(PCLAB); C('l'); EndLine;

    GenCounter(stn^.lineNumber,stn^.forBody);

    loopNestLevel := loopNestLevel + 1;
    GenStmtList(stn^.forBody);
    loopNestLevel := loopNestLevel - 1;

    GenVarT(stn^.forIndexVar,tn,EVALPOINT);
    if variableInc then
	GenVarT(stn^.forIncVar,tn,EVALGET);
    elsif tn^.kind = DTPOINTER then
	GenOpT(PCLDC,tn); X; I(WORDSIZE); X;
	    I(increment); EndLine;
    else
	GenOpT(PCLDC,cardIntTypeNode); X; I(WORDSIZE); X;
	    I(increment); EndLine;
    end;
    if variableLimit then
	GenVarT(stn^.forLimitVar,tn,EVALGET);
    elsif tn^.kind = DTPOINTER then
	GenOpT(PCLDC,tn); X; I(WORDSIZE); X;
	    I(trunc(limit)); EndLine;
    else
	GenOpT(PCLDC,cardIntTypeNode); X; I(WORDSIZE); X;
	    I(trunc(limit)); EndLine;
    end;
    GenOpT(PCFOR,tn); X; I(SizeOf(tn));
	    X; Lab(top); EndLine;

Lab(bottom); GenOpL(PCLAB);

    GenPrePostEval(stn^.forPreEval,EVALPOST);

    forExit := save;
    if exitLevel > loopNestLevel then
	exited := false;
	exitLevel := 1000;
    end;
end GenStmtFor;


procedure GenStmtWith(stn : StmtNode);
begin
    GenExpr(stn^.withQual,EVALGET);
    GenVar(stn^.withPtrVar,EVALPUT);
    GenStmtList(stn^.withBody);
end GenStmtWith;


procedure GenStmtReturn(stn : StmtNode);
var
    on       : OptNode;
    doreturn : boolean;
    tn       : TypeNode;
begin
    doreturn := true;
    returned := true;
    tn := genProcNode^.procType^.funcType;
    if stn^.inlineExpr # nil then
	GenExpr(stn^.returnVal, EVALGET);
	GenStore(stn^.inlineVarExpr, stn^.returnVal^.exprType);
        GenOp(PCUJP); Lab(stn^.inlineExpr^.inlineReturn); EndLine;
        doreturn := false;
    elsif stn^.inlineStmt # nil then
        GenOp(PCUJP); Lab(stn^.inlineStmt^.inlineReturn); EndLine;
        doreturn := false;
    elsif stn^.returnVal # nil then
        if ReferenceByPoint(tn) then
            GenExpr(stn^.returnVal,EVALPOINT);
            tn := addressTypeNode;
        else
            GenExpr(stn^.returnVal,EVALGET);
        end;
	(* watch for tail recursion *)
	if optimFlag then
	    if stn^.returnVal^.kind = EXPRFUNC then
		on := stn^.returnVal^.func^.opt;
		if on^.tailRecursion then
		    doreturn := false;
		end;
	    end;
	end;
    end;
    if doreturn then
	GenOpTL(PCRET, tn);
    end;
end GenStmtReturn;

procedure GenStmtExit(stn : StmtNode);
begin
    exited := true;
    case stn^.exitKind of
    | STMTLOOP:
	GenOp(PCUJP); Lab(loopExit.exitLabel); EndLine;
	if loopExit.loopLevel < exitLevel then
	    exitLevel := loopExit.loopLevel;
	end;
    
    | STMTFOR:
	GenOp(PCUJP); Lab(forExit.exitLabel); EndLine;
	if forExit.loopLevel < exitLevel then
	    exitLevel := forExit.loopLevel;
	end;
    
    | STMTREPEAT:
	GenOp(PCUJP); Lab(repeatExit.exitLabel); EndLine;
	if repeatExit.loopLevel < exitLevel then
	    exitLevel := repeatExit.loopLevel;
	end;
    
    | STMTWHILE:
	GenOp(PCUJP); Lab(whileExit.exitLabel); EndLine;
	if whileExit.loopLevel < exitLevel then
	    exitLevel := whileExit.loopLevel;
	end;
	
    end;
end GenStmtExit;


$if pascal then
procedure GenStmtGoto(stn : StmtNode);
begin
    if stn^.OOB then
	GenOp(PCLJP);
	C('l'); GenString(stn^.targetLabel^.labelName); 
	X;
	I(currLevel - stn^.targetLabel^.proc^.displayLevel);
	X;
	I(stn^.targetLabel^.proc^.block);
	EndLine;
    else
	GenOp(PCUJP);	
	C('l');	
	GenString(stn^.targetLabel^.labelName);
	EndLine();
    end;
end GenStmtGoto;

procedure GenStmtLabel(stn : StmtNode);
begin
    C('l');
    GenString(stn^.label^.labelName);
    GenOpL(PCLAB);
    if stn^.label^.OOBIndex > 0 then
	GenOpL(PCRST);
    end;
end GenStmtLabel;
$end


procedure DoInline(inlineParams  : ExprList; 
		   inlineFormals : InlineParamList; 
		   paramList     : ParamList);
var
    pen, den: ExprNode;
    pn      : ParamNode;
    ipn     : InlineParamNode;
    atn     : TypeNode;
    i       : integer;
    rowSize : MemoryOffset;
begin
    if (inlineParams # nil) and (inlineFormals # nil) and (paramList # nil) then
        pen := inlineParams^.first;
        pn := paramList^.first;
        ipn := inlineFormals^.first;
        while (pn # nil) and (pen # nil) do
	    if ipn^.kind = IPPARAM then
		case pn^.kind of
		| PARAMARRAYVALUE, PARAMARRAYVAR, PARAMARRAYCONST :
		    GenExpr(pen^.descripBase, pen^.descripMode);
		    GenVarT(ipn^.formal,addressTypeNode,EVALPUT);
		    (* Generate descriptor count/strides *)
		    den := pen^.descrips^.first;
		    for i := 1 to pn^.paramType^.descripCount do
			GenExpr(den,EVALGET);
			GenVarT(ipn^.formal,addressTypeNode,EVALPOINT);
			GenOpT(PCINC,addressTypeNode); X; I(i*WORDSIZE);
			    EndLine;
			GenIndirectVar(integerTypeNode,EVALPUT);
			den := den^.next;
		    end (* for *);
		    if pn^.kind = PARAMARRAYVALUE then
			(* copy value parameter *)
			(* point to parameter address *)
			GenVarT(ipn^.formal,addressTypeNode,EVALPOINT);
    
			(* save variable address *)
			GenOp(PCSAV); I(0); X; C('m'); EndLine;
			(* get number of elements *)
			atn := pn^.paramType;
			for i := 1 to pn^.paramType^.descripCount do
			    (* get address of pointer variable *)
			    GenOp(PCUSE); I(0); X; C('c'); EndLine;
			    GenOpT(PCINC,addressTypeNode); X; I(i*WORDSIZE);
				EndLine;
			    (* get count *)
			    GenIndirectVar(cardIntTypeNode,EVALGET);
			    if i > 1 then
				(* multiply by previous values *)
				GenOpT(PCMUP,cardIntTypeNode); X;
				    I(WORDSIZE); EndLine;
			    end;
			    rowSize := atn^.elementSize;
			    atn := atn^.elementType;
			end;
			GenConstAddress(rowSize);
			(* multiply by element size *)
			GenOpT(PCMUP,addressTypeNode); X; I(WORDSIZE); EndLine;
			(* save size *)
			GenOp(PCSAV); I(1); X; C('m'); EndLine;
			(* get address, size *)
			GenOp(PCUSE); I(0); X; C('m'); EndLine;
			GenOp(PCUSE); I(1); X; C('m'); EndLine;
			(* allocate space on stack, copy parameter value *)
			GenOp(PCSAC); EndLine;
		    end;

		| PARAMVAR, PARAMVALUE, PARAMCONST :
		    if pn^.reference then
			GenExpr(pen,EVALPOINT);
			GenVarT(ipn^.formal,addressTypeNode,EVALPUT);
		    else
			GenExpr(pen,EVALGET);
			GenVar(ipn^.formal,EVALPUT);
		    end;
		    if (pn^.kind = PARAMVALUE) and pn^.reference then
			GenVarT(ipn^.formal,addressTypeNode,EVALPOINT);
			GenConstAddress(WordSizeOf(pn^.paramType));
			(* allocate space on stack, copy parameter value *)
			GenOp(PCSAC); EndLine;
		    end;
		end (* case *);
	    end (* if ipn^.kind = IPPARAM *);
	    pen := pen^.next;
            pn := pn^.next;
            ipn := ipn^.next;
        end (* while *);
    end (* if *);
end DoInline;

procedure GenStmtInline(stn : StmtNode);
begin
    DoInline(stn^.inlineParams, stn^.inlineFormals,
	    stn^.inlineProc^.procType^.paramList);
    stn^.inlineReturn := NewLabel();
    GenStmtList(stn^.inlineBody);
    Lab(stn^.inlineReturn); GenOpL(PCLAB);
end GenStmtInline;


procedure GenStmt(stn : StmtNode);
begin
    if DEBUG and TraceGenpc then
	Writef(codeFile,'# statement %n\n', stn^.kind);
    end;
    if stn^.bad then
	StmtError(stn,'GenStmt: bad statement?');
    else
	currFile := stn^.fileName;
	currLine := stn^.lineNumber;
	StabLine(currFile,currLine);
	case stn^.kind of
$if pascal then
	| STMTLABEL     :   GenStmtLabel(stn);
	| STMTGOTO      :   GenStmtGoto(stn);
$end
	| STMTNONE      :   (* nothing *);
	| STMTASSIGN    :   GenStmtAssign(stn);
	| STMTPROC      :   GenStmtProc(stn);
	| STMTIF	:   GenStmtIf(stn);
	| STMTCASE      :   GenStmtCase(stn);
	| STMTWHILE	:   GenStmtWhile(stn);
	| STMTREPEAT    :   GenStmtRepeat(stn);
	| STMTLOOP      :   GenStmtLoop(stn);
	| STMTFOR       :   GenStmtFor(stn);
	| STMTWITH      :   GenStmtWith(stn);
	| STMTRETURN    :   GenStmtReturn(stn);
	| STMTEXIT      :   GenStmtExit(stn);
	| STMTINLINE    :   GenStmtInline(stn);
	| STMTSTMTS     :   GenStmtStmts(stn);
	end;
	UpdateTemps;
    end;
end GenStmt;

procedure GenIndirectVar(varType : TypeNode; mode : EvalMode);
begin
    if mode in EvalModeSet{EVALGET, EVALPUT} then
	if mode = EVALGET then
	    GenOp(PCIND);
	else
	    GenOp(PCSTO);
	end;
	GenT(varType); X; I(SizeOf(varType)); EndLine;
    end;
end GenIndirectVar;

procedure GenExprString(en : ExprNode; mode : EvalMode);
begin
    if mode = EVALPUT then
	ExprError(en,'GenExprString: Cannot store into a constant?');
    elsif en^.exprConst^.kind # DTSTRING then
	ExprError(en,'GenExprString: not a string?');
    else
	if mode = EVALGET then
	    GenOpT(PCLDC,stringTypeNode);
	else
	    GenOpT(PCLCA,stringTypeNode);
	end;
	X; I(SizeOf(en^.constType)); X; C('"');
	WriteStringConst(codeFile,en^.exprConst^.strVal);
	C('"'); EndLine;
    end;
end GenExprString;

procedure GenExprConst(en : ExprNode; mode : EvalMode);
var
    cn : ConstNode;
    op : PCodeOp;
begin
    cn := en^.exprConst;
    if mode = EVALGET then
	op := PCLDC;
    elsif mode = EVALPOINT then
	op := PCLCA;
    else
	ExprError(en,'Store into a constant?');
    end;
    case cn^.kind of
    | DTINTEGER, DTCARDINAL :
	if en^.exprType^.kind = DTPOINTER then
	    GenOpT(op,addressTypeNode);
	elsif cn^.cardVal < 0.0 then
	    GenOpT(op,integerTypeNode);
	else
	    GenOpT(op,cardinalTypeNode);
	end;
	X; I(WORDSIZE); X; I(trunc(cn^.cardVal)); EndLine;

    | DTCHAR :
	if en^.exprType^.kind = DTSTRING then
	    GenOpT(op,stringTypeNode);
	else
	    GenOpT(op,charTypeNode);
	end;
	X; I(SizeOf(en^.exprType)); X; C('"');
	if cn^.charVal in CharSet{' '..'~'} then
	    if cn^.charVal in CharSet{'"','\\'} then
		C('\\');
	    end;
	    C(cn^.charVal);
	else
	    C('\\');
	    I((ord(cn^.charVal) div 64) mod 8);
	    I((ord(cn^.charVal) div 8) mod 8);
	    I(ord(cn^.charVal) mod 8);
	end;
	C('"'); EndLine;
    
    | DTBOOLEAN :
	GenOpT(op,booleanTypeNode); X; I(WORDSIZE); X; I(ord(cn^.boolVal));
	EndLine;
    
    | DTREAL,
    DTLONGREAL :
	GenOpT(op,en^.exprType); X; I(SizeOf(en^.exprType)); X;
	GenReal(cn^.realVal); EndLine;
    
    | DTSTRING :
	GenExprString(en,mode);
    
    | DTENUMERATION :
	GenOpT(op,integerTypeNode); X; I(WORDSIZE); X; I(cn^.enumVal^.enumOrd);
	EndLine;
    
    | DTPROC :
	GenOpT(op,procTypeNode); X; I(WORDSIZE); X;
	GenString(cn^.procVal^.globalName); EndLine;
    
    | DTPOINTER :
	GenOp(op); C('n'); X; I(SizeOf(en^.exprType)); EndLine;
    
    | DTSET :
	GenOpT(op,bitsetTypeNode); X; GenSet(cn^.setVal); EndLine;
    end;
end GenExprConst;

procedure GenExprUnOp(en : ExprNode; mode : EvalMode);
begin
    assert(mode = EVALGET, 'Mode not EVALGET in GenExprUnOp?');
    GenExpr(en^.opnd,EVALGET);
    if en^.exprUnOp = TKMINUS then
	GenOpTL(PCNEG,en^.unOperType);	(* unary subtract *)
    elsif en^.exprUnOp = TKPLUS then
	(* unary plus *)
    else
	GenOpTL(operPcode[en^.exprUnOp],en^.unOperType);
    end;
end GenExprUnOp;

procedure GenExprBinSetOp(en : ExprNode);
var
    binOp : Token;
    size : integer;
begin
    GenExpr(en^.opnd1,EVALGET);
    GenExpr(en^.opnd2,EVALGET);
    binOp := en^.exprBinOp;
    size := trunc(NumberOf(en^.operType^.setRange));
    case binOp of
	| TKPLUS :
	    GenOpT(PCUNI,en^.operType); X; I(size); EndLine;
	
	| TKMINUS :
	    GenOpT(PCDIF,en^.operType); X; I(size); EndLine;
	
	| TKASTERISK :
	    GenOpT(PCINT,en^.operType); X; I(size); EndLine;
	
	| TKSLASH :
	    GenOpT(PCSDF,en^.operType); X; I(size); EndLine;
	
	| TKLSEQUAL :
	    GenOpT(PCDIF,en^.operType); X; I(size); EndLine;
	    GenOpT(PCLDC,bitsetTypeNode); X; I(size); X;
		I(0); EndLine;
	    GenOpT(PCEQU,en^.operType); X; I(size); EndLine;
	
$if pascal then
	| TKLESS :
	    Error('Set <, > not implemented yet');
$end

	| TKEQUALS, TKSHARP, TKNOTEQUAL:
	    GenOpT(operPcode[binOp],en^.operType); X; I(size);
	    EndLine;

	| TKIN :
	    GenOpTL(PCINN,en^.operType);
    end;
end GenExprBinSetOp;

procedure GenExprBinOp(en : ExprNode; mode : EvalMode);
var
    trueLabel, bothLabel : LabelNumber;
    temp : TempNumber;
    minElement : HugeInteger;
begin
    if en^.operType^.kind = DTSET then
	GenExprBinSetOp(en);
    elsif en^.exprBinOp in TokenSet{TKAND, TKAMPERSAND, TKOR} then
	trueLabel := NewLabel();
	bothLabel := NewLabel();
	temp := AllocTemp();
	GenCondition(en,trueLabel,NULLLABEL);
	GenConstBoolean(false);
	GenOp(PCSAV); I(temp); X; C('t'); EndLine;
	GenOp(PCUJP); Lab(bothLabel); EndLine;
	Lab(trueLabel); GenOpL(PCLAB);
	GenConstBoolean(true);
	GenOp(PCSAV); I(temp); X; C('r'); EndLine;
	Lab(bothLabel); GenOpL(PCLAB);
	GenOp(PCUSE); I(temp); X; C('m'); EndLine;
	FreeTemp(temp);
    else
	GenExpr(en^.opnd1,EVALGET);
	GenExpr(en^.opnd2,EVALGET);
	GenOpT(operPcode[en^.exprBinOp],en^.operType);
	    X; I(SizeOf(en^.operType));
	EndLine;
    end;
end GenExprBinOp;

procedure GenExprVal(en : ExprNode; mode : EvalMode);
begin
    if en^.exprVal^.kind = EXPRVAR then
	GenVarT(en^.exprVal^.exprVar,en^.exprType,EVALGET);
    else
	GenExpr(en^.exprVal,EVALPOINT);
	GenIndirectVar(en^.exprType,EVALGET);
    end;
end GenExprVal;

procedure GenExprSave(en : ExprNode; mode : EvalMode);
begin
    if mode = EVALGET then
	GenExpr(en^.exprSave,EVALGET);
    else
	GenExpr(en^.exprSave,EVALPOINT);
    end;
    GenOpT(PCSTN,en^.exprSaveVar^.varType); X;
	I(SizeOf(en^.exprSaveVar^.varType)); X;
	I(0); X; GenAddress(en^.exprSaveVar); EndLine;
end GenExprSave;

procedure @inline GenExprFunc(const en : ExprNode; const mode : EvalMode);
begin
    GenFuncProc(en^.func,en^.params, en^.selected, en^.addrOfValue, mode);
end GenExprFunc;

procedure GenExprInline(en : ExprNode; mode : EvalMode);
var
    pen     : ExprNode;
    pn      : ParamNode;
    ipn     : InlineParamNode;
    atn     : TypeNode;
    i       : integer;
begin
    assert(mode # EVALPUT, 'Mode EVALPUT in GenExprInline?');
    DoInline(en^.inlineParams, en^.inlineFormals,
	en^.inlineProc^.procType^.paramList);
    en^.inlineReturn := NewLabel();
    GenStmtList(en^.inlineBody);
    Lab(en^.inlineReturn); GenOpL(PCLAB);
    GenExpr(en^.inlineResult, mode);
end GenExprInline;

procedure GenExprSet(en : ExprNode; mode : EvalMode);
var
    esn : ExprSetNode;
    cn : ConstNode;
    numOpnds : integer;
begin
    cn := ConstSet(en^.setConst,en^.setType);
    if (mode = EVALGET) or (en^.setExpr # nil) then
	GenOpT(PCLDC,bitsetTypeNode);
    else
	GenOpT(PCLCA,bitsetTypeNode);
    end;
    X; GenSet(cn^.setVal); EndLine;

    if en^.setExpr # nil then
	esn := en^.setExpr^.first;
	while esn # nil do
	    GenExpr(esn^.lower,EVALGET);
	    if esn^.upper = nil then
		numOpnds := 1;
	    else
		GenExpr(esn^.upper,EVALGET);
		numOpnds := 2;
	    end;
	    GenOpT(PCSET,en^.setType); X;
		I(SizeOf(en^.setType)); X; 
		I(numOpnds);
		EndLine;
	    esn := esn^.next;
	end;
    end;
end GenExprSet;

procedure DoGenExpr(en : ExprNode; mode : EvalMode);
begin
    case en^.kind of
    | EXPRVAR       :   if mode = EVALPUT then
			    GenVar(en^.exprVar, EVALPUT);
			else
			    GenVar(en^.exprVar, EVALPOINT);
			end;
    | EXPRCONST     :   GenExprConst(en, mode);
    | EXPRBINOP     :   GenExprBinOp(en, mode);
    | EXPRVAL       :   GenExprVal(en, mode);
    | EXPRFUNC      :   GenExprFunc(en, mode);
    | EXPRUNOP      :   GenExprUnOp(en, mode);
    | EXPRCHECK     :   GenExprCheck(en, mode);
    | EXPRSAVE      :   GenExprSave(en, mode);
    | EXPRSET       :   GenExprSet(en, mode);
    | EXPRINLINE    :   GenExprInline(en, mode);
    end;
end DoGenExpr;

procedure OptGenExpr(en : ExprNode; mode : EvalMode; state : EvalState);
var
    on, ron : OptNode;
begin
    on := en^.opt;
    ron := on^.rootEqual;
    if (on^.usage # OUSEINDIVIDUAL) or
	    ((on^.usage = OUSEINDIVIDUAL) and (state # EVALNORMAL)) then
	if DEBUG and TraceOptim then
	    Writef(output,'OptGenExpr: usage=%n, mode=%n, state=%n, en=',
		    on^.usage, mode, state);
	    WriteExpr(en);
	    Writec(output, '\n');
	end;
    end;
    case on^.usage of
    | OUSEINDIVIDUAL :
	(* not subexpression, do normal evaluate *)
	if state = EVALNORMAL then
	    DoGenExpr(en,mode);
	end;
    
    | OUSEGENERATE :
	(* either generation or discard of a value, depending on state *)
	if state = EVALPRE then
	    (* calculate a value for later use *)
	    DoGenExpr(en,mode);
	    GenTemp(PCSTR,ron,en);
	end;
    
    | OUSEFIRST :
	(* first use of a value, evaluate and copy it *)
	DoGenExpr(en,mode);
	GenTemp(PCSTN,ron,en);
    
    | OUSEAFTERFIRST :
	(* reuse saved value *)
	GenTemp(PCLOD,ron,en);
    
    | OUSEINDUCTION :
	if state = EVALNORMAL then
	    (* use of for index *)
	    GenVarT(ron^.inductionVar,en^.exprType,EVALGET);
	end;
    
    end;
end OptGenExpr;

procedure GenExpr(en : ExprNode; mode : EvalMode);
begin
    if en = nil then
	Error('Generated nil expression?');
    else
	if DEBUG and TraceGenpc then
	    Writef(codeFile,'# expression %n %n\n', en^.kind, mode);
	end;
	if en^.exprType = nil then
	    ExprError(en,'GenExpr: no type on expression');
	end;
	if optimFlag then
	    OptGenExpr(en,mode,EVALNORMAL);
	else
	    DoGenExpr(en,mode);
	end;
    end;
end GenExpr;

procedure GenCondition(en : ExprNode; trueLabel,falseLabel : LabelNumber);
var
    done : boolean;
    fallThrough : LabelNumber;
begin
    done := false;
    if en^.kind = EXPRUNOP then
	if en^.exprUnOp = TKNOT then
	    GenCondition(en^.opnd,falseLabel,trueLabel);
	    done := true;
	end;
    elsif en^.kind = EXPRBINOP then
	fallThrough := NULLLABEL;
	if en^.exprBinOp in TokenSet{TKAND, TKAMPERSAND} then
	    if falseLabel = NULLLABEL then
		fallThrough := NewLabel();
		falseLabel := fallThrough;
	    end;
	    GenCondition(en^.opnd1,NULLLABEL,falseLabel);
	    GenCondition(en^.opnd2,trueLabel,falseLabel);
	    if fallThrough # NULLLABEL then
		Lab(fallThrough); GenOpL(PCLAB);
	    end;
	    done := true;
	elsif en^.exprBinOp = TKOR then
	    if trueLabel = NULLLABEL then
		fallThrough := NewLabel();
		trueLabel := fallThrough;
	    end;
	    GenCondition(en^.opnd1,trueLabel,NULLLABEL);
	    GenCondition(en^.opnd2,trueLabel,falseLabel);
	    if fallThrough # NULLLABEL then
		Lab(fallThrough); GenOpL(PCLAB);
	    end;
	    done := true;
	end;
    end;
    if not done then
	GenExpr(en,EVALGET);
	if trueLabel # NULLLABEL then
	    GenOp(PCTJP); Lab(trueLabel); EndLine;
	    if falseLabel # NULLLABEL then
		GenOp(PCUJP); Lab(falseLabel); EndLine;
	    end;
	else
	    GenOp(PCFJP); Lab(falseLabel); EndLine;
	end;
    end;
end GenCondition;

procedure GenCode;
begin
    if DEBUG and TraceGenpc then
	Writef(output,'Beginning code generation\n');
    end;
    InitPcode;
    loopExit.exitLabel := NULLLABEL;
    loopExit.loopLevel := 0;
    forExit := loopExit;
    whileExit := loopExit;
    repeatExit := loopExit;
    currCounter := 0;
    loopNestLevel := 0;
    new(dummyVar);	(* For use by GenAddress *)
    
    InitStab(mainFileName);

    if globalProc^.globalName = nil then
	(* Why can't these be the same ? ||| *)
	GenString(compileModuleName); GenOp(PCBGN); W('Modula'); X;
	    I(globalProc^.block); X;
	    I(ord(genCountFlag)); 
	    I(ord(gprofFlag));
	EndLine;
    else
	GenString(compileModuleName); GenOp(PCBGN); W('Modula');
	    X; I(0); X; I(ord(genCountFlag));
	    I(ord(gprofFlag));
	EndLine;
    end;

$if pascal then
    StabScope(builtinScope);
$end
    GenComs;
    GenModule(globalModule);
    if genCountFlag then
	GenOp(PCCTS); C('d'); X; I(currCounter); X;
	    GenString(mainFileName); EndLine;
    end;
    GenOpL(PCEND);
    if DEBUG and TraceGenpc then
	Writef(output,'Ending code generation\n');
    end;
end GenCode;

begin
    target := TARGETVAX;
end GenPC.
