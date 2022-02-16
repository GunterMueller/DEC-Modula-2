implementation module SymbolDump;

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


from Machine import
    BYTESIZE;
    
from Strings import
    String, WriteStringConst;

from Globals import
    MODULEINITNAME, compileModuleName, genDebugInfoFlag, internalCallFlag,
    OptNcall;

from Symbols import
    TypeNumber, Scope, PTRMODULA, GSNORMAL, Symbol,
    ConstNode, TypeNode, FieldNode, FieldList, VariantNode, PortNode,
    ModuleNode, ProcNode, EnumNode, ParamNode, SymbolKind,
    MemoryType, DataType, ArrayKind, LabelNumber,
    integerTypeNode, charTypeNode, booleanTypeNode, longrealTypeNode,
    packedCharTypeNode, packedBooleanTypeNode, 
    globalModule, globalProc, globalPortList, FIELDTAG;

$if modula2 then
from Symbols import ArrayKindSet, realTypeNode, cardinalTypeNode, wordTypeNode,
    byteTypeNode, addressTypeNode, cardIntTypeNode, fileTypeNode, 
    processTypeNode, packedByteTypeNode;
$else
from Symbols import PTRPASCAL;
$end

from Errors import
    ErrorName;

from TypeInfo import
    SizeOf, AlignmentOf, BaseType;

from PCodeOps import
    PCSYM, PCLAB;

from PCode import
    GenOp, Lab, GenOpL, NewLabel, W, C, GenReal, GenSet, EndLine, I, GenString;

from GenCode import
    GenAddress, GenProcName, codeFile;

from GenPC import
    IsRegister;

const
    STABNMOD2         =  80;	(* same as N_MOD2 in /usr/include/stab.h *)

    STABSOURCEFILE    = 100;
    STABSYMBOL        = 128;
    STABREGISTER      =  64;
    STABGLOBAL        =  32;
    STABPARAM         = 160;
    STABLINE	      =  68;
    STABPROC	      =  36;
    STABTOKENSPERLINE =  10;

(* Builtin type numbers...dbx knows about all these. *)
$if modula2 then
    INTEGERNUMBER   =  1;
    CHARNUMBER      =  2;
    BOOLEANNUMBER   =  3;
    CARDINALNUMBER  =  4;
    REALNUMBER      =  5;
    LONGREALNUMBER  =  6;
    WORDNUMBER      =  7;
    BYTENUMBER      =  8;
    ADDRESSNUMBER   =  9;
    FILENUMBER      = 10;
    PROCESSNUMBER   = 11;
    CARDINTNUMBER   = 12;
$else (* pascal *)
    BOOLEANNUMBER   =  1;
    CHARNUMBER      =  2;
    INTEGERNUMBER   =  3;
    LONGREALNUMBER  =  4;
$end
    MAXBUILTINTYPES = 20;

var
    generateTypeNumber : TypeNumber;
    stabFileName    : String;
    stabLineNumber  : integer;
    stabTokenCount  : integer;
    inTypeDef       : boolean;


procedure NewTypeNumber (): TypeNumber;
begin
    generateTypeNumber := generateTypeNumber + 1;
    return generateTypeNumber;
end NewTypeNumber;

procedure @inline StabEndLine;
begin
    EndLine;
    stabTokenCount := 0;
end StabEndLine;

procedure @inline StabComma;
begin
    C(',');
end StabComma;

procedure @inline StabSemicolon;
begin
    C(';');
end StabSemicolon;

procedure StartTypeDef;
begin
    GenOp(PCSYM); C('t'); StabComma; C('"');
end StartTypeDef;

procedure EndTypeDef;
begin
    C('"'); StabComma; I(STABSYMBOL); StabComma; I(0);
	StabComma; I(0); StabComma; I(0); StabEndLine;
end EndTypeDef;

procedure StabCommaX;
begin
    C(',');
    stabTokenCount := stabTokenCount + 1;
    if inTypeDef and (stabTokenCount > STABTOKENSPERLINE) then
	C('?');
	EndTypeDef;
	StartTypeDef;
    end;
end StabCommaX;

procedure StabSemicolonX;
begin
    StabSemicolon;
    stabTokenCount := stabTokenCount + 1;
    if inTypeDef and (stabTokenCount > STABTOKENSPERLINE) then
	C('?');
	EndTypeDef;
	StartTypeDef;
    end;
end StabSemicolonX;

procedure NamedType(tn : TypeNode) : TypeNode;
begin
    if tn # nil then
	while   (tn^.opaqueName = nil) and (tn^.theModule = nil) and
		(tn^.kind = DTRENAME) and
		(tn^.alignment = -1) and (tn^.size = -1) and
		(tn^.renameType # nil) do
	    tn := tn^.renameType;
	end;
    end;
    return tn;
end NamedType;

procedure InitStab(const mainFileName : String);
var
    fileLabel : LabelNumber;
    tn : TypeNode;
begin
    stabTokenCount := 0;

$if modula2 then
    integerTypeNode^.number	  := INTEGERNUMBER;
    charTypeNode^.number	  := CHARNUMBER;
    packedCharTypeNode^.number    := CHARNUMBER;
    booleanTypeNode^.number	  := BOOLEANNUMBER;
    packedBooleanTypeNode^.number := BOOLEANNUMBER;
    cardinalTypeNode^.number	  := CARDINALNUMBER;
    realTypeNode^.number	  := REALNUMBER;
    longrealTypeNode^.number	  := LONGREALNUMBER;
    wordTypeNode^.number	  := WORDNUMBER;
    byteTypeNode^.number	  := BYTENUMBER;
    packedByteTypeNode^.number    := BYTENUMBER;
    addressTypeNode^.number	  := ADDRESSNUMBER;
    fileTypeNode^.number	  := FILENUMBER;
    processTypeNode^.number	  := PROCESSNUMBER;
    cardIntTypeNode^.number	  := CARDINTNUMBER;
$else
    booleanTypeNode^.number	  := BOOLEANNUMBER;
    packedBooleanTypeNode^.number := BOOLEANNUMBER;
    charTypeNode^.number	  := CHARNUMBER;
    packedCharTypeNode^.number    := CHARNUMBER;
    integerTypeNode^.number	  := INTEGERNUMBER;
    longrealTypeNode^.number	  := LONGREALNUMBER;
$end

    generateTypeNumber := MAXBUILTINTYPES;
    inTypeDef := false;

    fileLabel := NewLabel();
    GenOp(PCSYM); C('s'); StabComma; C('"'); GenString(mainFileName); C('"');
	StabComma; I(STABSOURCEFILE); StabComma; I(0); StabComma; I(0);
	StabComma; Lab(fileLabel); 
    StabEndLine;
    Lab(fileLabel); GenOpL(PCLAB);

end InitStab;


procedure StabFieldList(fl : FieldList);
var
    fn : FieldNode;
    vn : VariantNode;
begin
    fn := fl^.first;
    while fn # nil do
	if fn^.name # nil then
	    GenString(fn^.name);
	    C(':');
	    StabTypeNumber(fn^.fieldType);
	    StabComma;
	    I(fn^.offset);
	    StabComma;
	    I(SizeOf(fn^.fieldType));
	    StabSemicolonX;
	end;
	if fn^.kind = FIELDTAG then
	    vn := fn^.variantList^.first;
	    while vn # nil do
		StabFieldList(vn^.fieldList);
		vn := vn^.next;
	    end;
	end;
	fn := fn^.next;
    end;
end StabFieldList;

procedure StabProcType(tn : TypeNode);
var
    param : ParamNode;
    ptn : TypeNode;
begin
    if tn^.funcType # nil then
	StabTypeNumber(tn^.funcType); StabComma;
    end;
    I(tn^.numParams); StabSemicolon;
    if tn^.paramList # nil then
	param := tn^.paramList^.first;
	while param # nil do
	    if param^.name # nil then
		GenString(param^.name); C(':');
	    end;
	    ptn := param^.paramType;
	    StabTypeNumber(ptn);
	    StabComma;
	    I(ord(param^.kind)); StabSemicolon;
	    param := param^.next;
	end;
    end;
    StabSemicolon;
end StabProcType;

procedure StabTypeDef(tn : TypeNode);
var
    enum : EnumNode;
    atn : TypeNode;
    i : integer;
begin
    tn := NamedType(tn);
    if tn^.opaqueName # nil then
	C('o');
	GenString(tn^.opaqueName);
	if tn^.kind # DTOPAQUE then
	    StabComma;
	end;
    elsif tn^.theModule # nil then
	(* indirect type name *)
	C('i');
	GenString(tn^.theModule^.name);
	C(':');
	GenString(tn^.name);
	StabComma;
    end;
    case tn^.kind of
    | DTINTEGER     : I(INTEGERNUMBER);
    | DTCHAR	    : I(CHARNUMBER);
    | DTBOOLEAN     : I(BOOLEANNUMBER);
    | DTLONGREAL    : I(LONGREALNUMBER);
    | DTRENAME      : (* watch for size and alignment *)
	if tn^.renameType # nil then
	    if (tn^.size # -1) and (tn^.size # tn^.renameType^.size)
	    then
		C('@'); C('s'); I(tn^.size);
		StabSemicolon;
	    end;
	    if (tn^.alignment # -1) and
		(tn^.alignment # tn^.renameType^.alignment)
	    then
		C('@'); C('a'); I(tn^.alignment);
		StabSemicolon;
	    end;
	end;
	StabTypeNumber(tn^.renameType);

$if modula2 then
    | DTCARDINAL    : I(CARDINALNUMBER);
    | DTREAL	    : I(REALNUMBER);
    | DTWORD	    : I(WORDNUMBER);
    | DTBYTE	    : I(BYTENUMBER);
    | DTOPAQUE      : (* handled above *)
    | DTDYNARRAY    :
	atn := tn^.dynArrayType;
	i := 0;
	while (atn^.kind = DTARRAY) and
		(atn^.arrayKind in ArrayKindSet{ARRAYOPEN,ARRAYNOCOUNT}) do
	    i := i + 1;
	    atn := atn^.elementType;
	end;
	if tn^.dynArrayKind # PTRMODULA then
	    C('@'); C('p'); I(ord(tn^.dynArrayKind));
	    StabSemicolon;
	end;
	(* ||| Needs elementSize modifications, else dbx will lie about contents
	   if you index into the array *)
	C('D'); I(i); StabComma; StabTypeNumber(atn);
	    StabSemicolon;

$else (* pascal *)
    | DTFILE :
	C('@'); C('p'); I(ord(PTRPASCAL));
	    StabSemicolon;
	C('*');
	StabTypeNumber(tn^.fileType);
$end

    | DTPOINTER :
	if tn^.ptrKind # PTRMODULA then
	    C('@'); C('p'); I(ord(tn^.ptrKind)); StabSemicolon;
	end;
	C('*');
	StabTypeNumber(tn^.toType);
    
    | DTPROC :
	if tn^.funcType = nil then
	    C('p');
	else
	    C('f');
	end;
	StabProcType(tn);
    
    | DTSET :
	C('S');
	StabTypeNumber(tn^.setRange);
    
    | DTSUBRANGE :
	C('r');
	StabTypeNumber(tn^.baseType);
	StabSemicolon;
	I(trunc(tn^.subMinOrd));
	StabSemicolon;
	I(trunc(tn^.subMaxOrd));
    
    | DTRECORD :
	C('s');
	I(SizeOf(tn) div BYTESIZE);
	StabComma;
	I(AlignmentOf(tn));
	StabComma;
	StabFieldList(tn^.fieldList);
	StabSemicolon;
    
    | DTARRAY :
	if tn^.arrayKind = ARRAYNORMAL then
	    C('a');
	    StabTypeNumber(tn^.indexType);
	    StabSemicolon;
	    StabTypeNumber(tn^.elementType);
	    (* ||| Need elementSize modifications *)
	else
	    atn := tn;
	    i := 0;
	    while (atn^.kind = DTARRAY) and (atn^.arrayKind # ARRAYNORMAL) do
		atn := atn^.elementType;
		i := i + 1;
	    end;
	    if tn^.arrayKind = ARRAYSUBARRAY then
		C('E');
	    else
		C('O');
	    end;
	    I(i); StabComma; StabTypeNumber(atn); StabSemicolon;
	    (* ||| Need elementSize modifications *)
	end;

    | DTENUMERATION :
	C('e');
	enum := tn^.enumList^.first;
	while enum # nil do
	    GenString(enum^.name);
	    C(':');
	    I(enum^.enumOrd);
	    StabCommaX;
	    enum := enum^.next;
	end;
	StabSemicolon;

    end (* case *);
    if tn^.theModule # nil then
	StabSemicolon;
    elsif tn^.opaqueName # nil then
	StabSemicolon;
    end;
end StabTypeDef;

procedure StabCheckType(tn : TypeNode);
begin
    if tn = nil then
	(* do nothing *)
    elsif tn^.number = 0 then
	StabNamedType(tn^.name,tn);
    elsif tn^.kind = DTRENAME then
	StabCheckType(tn^.renameType);
    end;
end StabCheckType;

procedure StabCheckFieldList(fl : FieldList);
var
    fn : FieldNode;
    vn : VariantNode;
begin
    fn := fl^.first;
    while fn # nil do
	StabCheckType(fn^.fieldType);
	if fn^.kind = FIELDTAG then
	    vn := fn^.variantList^.first;
	    while vn # nil do
		StabCheckFieldList(vn^.fieldList);
		vn := vn^.next;
	    end;
	end;
	fn := fn^.next;
    end;
end StabCheckFieldList;

procedure StabCheckProcType(tn : TypeNode);
var
    pn : ParamNode;
begin
    StabCheckType(tn^.funcType);
    if tn^.paramList # nil then
	pn := tn^.paramList^.first;
	while pn # nil do
	    StabCheckType(pn^.paramType);
	    pn := pn^.next;
	end;
    end;
end StabCheckProcType;


(* try to make sure dependent types are output before this one is *)
procedure StabNamedType(name : String; tn : TypeNode);
var
    enum : EnumNode;
begin
    tn := NamedType(tn);
    if name = nil then
	name := tn^.name;
    end;
    if tn^.number = 0 then
	tn^.number := NewTypeNumber();
	case tn^.kind of
	| DTINTEGER, DTCHAR, DTBOOLEAN, DTREAL, DTLONGREAL,
	  DTCARDINAL, DTSTRING, DTANY, DTWORD, DTBYTE :
	    (* these are already defined *)
	| DTOPAQUE, DTENUMERATION :
	    (* nothing to do *)
	| DTRENAME :
	    StabCheckType(tn^.renameType);	
	| DTPOINTER :
	    StabCheckType(tn^.toType);
	| DTPROC :
	    StabCheckProcType(tn);
	| DTSET :
	    StabCheckType(tn^.setRange);
	| DTSUBRANGE :
	    StabCheckType(tn^.baseType);
	| DTRECORD :
	    StabCheckFieldList(tn^.fieldList);
	| DTDYNARRAY :
	    StabCheckType(tn^.dynArrayType);
	| DTARRAY :
	    if tn^.indexType # nil then
		StabCheckType(tn^.indexType);
	    end;
	    (* ||| Need elementSize modifications *)
	    StabCheckType(tn^.elementType);
$if pascal then
	| DTFILE :
	    StabCheckType(tn^.fileType);
$end
	end (* case *);
	StartTypeDef;
	    GenString(name); C(':'); C('t');
	    I(tn^.number);
	    C('=');
	    inTypeDef := true;
	    StabTypeDef(tn);
	    inTypeDef := false;
	EndTypeDef;
    end;
end StabNamedType;

procedure StabTypeNumber(tn : TypeNode);
begin
    tn := NamedType(tn);
    if tn = nil then
	I(0);
    elsif tn^.number # 0 then
	I(tn^.number);
    else
	tn^.number := NewTypeNumber();
	I(tn^.number);
	C('=');
	StabTypeDef(tn);
    end;
end StabTypeNumber;

procedure StabQualifiers(theModule : ModuleNode; 
			 proc      : ProcNode; 
			 last      : boolean);
begin
    if (theModule = nil) or
	((theModule = globalModule) and ((proc = globalProc) or (proc = nil)))
    then
	(* do nothing *)
    elsif proc = nil then
	(* global thing, just module qualifiers *)
	StabQualifiers(theModule^.enclosing,proc,false);
	GenString(theModule^.name);
	if not last then
	    C(':');
	end;
    elsif theModule^.enclosingProc = proc then
	(* next level is a module *)
	StabQualifiers(theModule^.enclosing,proc,false);
	GenString(theModule^.name);
	if not last then
	    C(':');
	end;
    elsif proc^.enclosingModule = theModule then
	(* next level is a proc *)
	StabQualifiers(theModule,proc^.enclosing,false);
	GenString(proc^.name);
	if not last then
	    C(':');
	end;
    else
	ErrorName(theModule^.name,'Module/proc list for $ confused');
	ErrorName(proc^.name,'Module/proc list for $ confused');
    end;
end StabQualifiers;

procedure StabProc(proc : ProcNode);
var
    tn : TypeNode;
    pn : ParamNode;
begin
    if genDebugInfoFlag then
	tn := proc^.procType^.funcType;
	if tn # nil then
	    StabCheckType(tn);
	end;
	GenOp(PCSYM); C('F'); StabComma; C('"');
	    if proc^.name = nil then
		W(MODULEINITNAME);
	    else
		GenString(proc^.name);
	    end;
	    C(':');
	    if tn # nil then
    		if (proc # globalProc) and (
		   ((proc^.extern = GSNORMAL) and internalCallFlag)
		    or (proc^.internalProc) and not OptNcall)
		then
		    C('J');
		else
		    C('F');
		end;
		StabTypeNumber(tn);
	    else
    		if (proc # globalProc) and (
		    ((proc^.extern = GSNORMAL) and internalCallFlag)
		    or (proc^.internalProc) and not OptNcall)
		then
		    C('I');
		else
		    C('P');
		end;
	    end;
	    StabComma;
	    GenProcName(proc);
	    StabComma;
	    if proc^.name = nil then
		GenString(proc^.enclosingModule^.name);
	    else
		StabQualifiers(proc^.enclosingModule,proc^.enclosing,true);
	    end;
	    C('"'); StabComma; 
	    I(STABPROC); StabComma; I(0); StabComma;
	    if tn # nil then
		I(SizeOf(tn));
	    else
		I(0);
	    end;
	    StabComma; 
	    GenProcName(proc);
	StabEndLine;
	if proc^.procType^.paramList # nil then
	    pn := proc^.procType^.paramList^.first;
	    while pn # nil do
		GenOp(PCSYM); C('p'); StabComma; C('"');
		    GenString(pn^.name); C(':');
		    if pn^.reference then
			C('v');
		    else
			C('p');
		    end;
		    StabTypeNumber(pn^.paramType); C('"'); StabComma;
		    I(STABPARAM); StabComma; I(0); StabComma; 
		    I(SizeOf(pn^.paramType)); StabComma;
		    GenAddress(pn^.paramVar);
		StabEndLine;
		pn := pn^.next;
	    end;
	end;
    end;
end StabProc;

procedure StabModule(theModule : ModuleNode);
begin
    if genDebugInfoFlag then
	GenOp(PCSYM); C('m'); StabComma; C('"');
	    GenString(theModule^.name); C(':'); C('m'); StabComma;
	    StabQualifiers(theModule^.enclosing,theModule^.enclosingProc,true);
	    C('"'); StabComma; I(STABSYMBOL); StabComma; I(0); 
	    StabComma; I(0); StabComma; I(0);
	StabEndLine;
    end;
end StabModule;

procedure StabConst(cn : ConstNode);
begin
    C('c'); C('=');
    case cn^.kind of
    | DTREAL, DTLONGREAL :
	C('r');
	GenReal(cn^.realVal);

    | DTINTEGER, DTCARDINAL :
	C('i');
	I(trunc(cn^.cardVal));

    | DTBOOLEAN :
	C('b');
	I(ord(cn^.boolVal));

    | DTCHAR :
	C('c');
	I(ord(cn^.charVal));

    | DTSTRING :
	C('s');
	C('''');
	WriteStringConst(codeFile,cn^.strVal);
	C('''');

    | DTENUMERATION :
	C('e');
	StabTypeNumber(cn^.enumVal^.enumType); StabComma;
	I(cn^.enumVal^.enumOrd);

    | DTSET :
	C('S');
	StabTypeNumber(cn^.setVal^.setType); StabComma;
	GenSet(cn^.setVal);

    | DTPOINTER :
	C('i');     (* Can only be nil right now *)
	I(0);
    end;
    StabSemicolon;
end StabConst;

procedure StabConstDef(name : String; cn : ConstNode);
var
    tn : TypeNode;
begin
    if cn^.kind = DTENUMERATION then
	tn := cn^.enumVal^.enumType;
	StabCheckType(tn);
    elsif cn^.kind = DTSET then
	tn := cn^.setVal^.setType;
	StabCheckType(tn);
    end;
    StartTypeDef;
    GenString(name); C(':');
    StabConst(cn);
    EndTypeDef;
end StabConstDef;

procedure StabScope(scope : Scope);
var
    sym      : Symbol;
    tn       : TypeNode;
begin
    if genDebugInfoFlag then
	sym := scope^.symbols^.first;
	while sym # nil do
	    if sym^.kind = SYMVAR then
		tn := NamedType(sym^.symVar^.varType);
		StabCheckType(tn);
		case sym^.symVar^.address.kind of
		| MEMGLOBAL :
		    GenOp(PCSYM); C('G'); StabComma; C('"');
			GenString(sym^.name); C(':');
			if sym^.symVar^.address.gvn^.extern = GSNORMAL
			then
			    C('V');
			else
			    C('G');
			end;
			StabTypeNumber(tn); StabComma;
			GenString(sym^.symVar^.address.gvn^.globalName);
			C('"'); StabComma; I(STABGLOBAL); StabComma;
			I(0); StabComma; I(SizeOf(tn));
			StabComma; I(0);
		    StabEndLine;
		
		| MEMNORMAL :
		    GenOp(PCSYM); C('v'); StabComma; C('"');
			GenString(sym^.name); C(':');
			StabTypeNumber(tn); C('"'); StabComma;
			I(STABSYMBOL); StabComma; I(0); StabComma;
			I(SizeOf(tn)); StabComma;
			GenAddress(sym^.symVar);
		    StabEndLine;
		
		| MEMFAST :
		    if IsRegister(sym^.symVar^.address) then
		        GenOp(PCSYM); C('r'); StabComma; C('"');
			    GenString(sym^.name); C(':'); C('r');
			    StabTypeNumber(BaseType(tn)); C('"'); StabComma;
			    I(STABREGISTER); StabComma; I(0); StabComma;
			    I(SizeOf(tn)); StabComma;
		    else
			GenOp(PCSYM); C('v'); StabComma; C('"');
			    GenString(sym^.name); C(':');
			    StabTypeNumber(tn); C('"'); StabComma;
			    I(STABSYMBOL); StabComma; I(0); StabComma;
			    I(SizeOf(tn)); StabComma;
		    end;
		    GenAddress(sym^.symVar);
		    StabEndLine;

		| MEMPARAM :
		end;
	    elsif sym^.kind = SYMTYPE then
		StabNamedType(sym^.name,sym^.symType);
	    elsif sym^.kind = SYMCONST then
		StabConstDef(sym^.name,sym^.symConst);
	    end;
	    sym := sym^.next;
	end;
    end;
end StabScope;

procedure StabGlobalPort;
var
    pn : PortNode;
    tn : TypeNode;
    cn : ConstNode;
    i : integer;
    proc : ProcNode;
begin
    GenOp(PCSYM); C('X'); StabComma; C('"');
	GenString(compileModuleName); C(':'); C('X'); I(0); I(0); I(0);
	C('m'); C('"'); StabComma; I(STABNMOD2); StabComma; I(0); StabComma;
	I(0); StabComma; I(0);
    StabEndLine;
    pn := globalPortList^.first;
    while pn # nil do
	if pn^.sym^.kind = SYMVAR then
	    tn := pn^.sym^.symVar^.varType;
	    StabCheckType(tn);
	    GenOp(PCSYM); C('X'); StabComma; C('"');
		GenString(pn^.theModule^.name); C('.');
		GenString(pn^.sym^.name); C(':'); C('X');
		I(ord(pn^.isQualified));
		I(ord(pn^.isExport));
		I(ord(pn^.extern));
		C('v'); StabTypeNumber(tn); C('"'); StabComma;
		I(STABNMOD2); StabComma; I(0); StabComma;
		I(0); StabComma; I(0);
	    StabEndLine;
	elsif pn^.sym^.kind = SYMPROC then
	    proc := pn^.sym^.symProc;
	    tn := proc^.procType;
	    StabCheckProcType(tn);
	    GenOp(PCSYM); C('X'); StabComma; C('"');
		GenString(pn^.theModule^.name); C('.');
		GenString(pn^.sym^.name); C(':'); C('X');
		I(ord(pn^.isQualified));
		I(ord(pn^.isExport));
		I(ord(pn^.extern));
		if tn^.funcType = nil then
		    C('p');
		else
		    C('f');
		end;
		if proc^.inlineProc then
		    I(proc^.time); StabComma;
		else
		    I(0); StabComma;
		end;
		StabProcType(tn); C('"'); StabComma;
		I(STABNMOD2); StabComma; I(0); StabComma;
		I(0); StabComma; I(0);
	    StabEndLine;
	elsif pn^.sym^.kind = SYMTYPE then
	    tn := pn^.sym^.symType;
	    StabCheckType(tn);
	    GenOp(PCSYM); C('X'); StabComma; C('"');
		GenString(pn^.theModule^.name); C('.');
		GenString(pn^.sym^.name); C(':'); C('X');
		I(ord(pn^.isQualified));
		I(ord(pn^.isExport));
		I(ord(pn^.extern));
		C('t'); StabTypeNumber(tn); C('"'); StabComma;
		I(STABNMOD2); StabComma; I(0); StabComma;
		I(0); StabComma; I(0);
	    StabEndLine;
	elsif pn^.sym^.kind = SYMCONST then
	    cn := pn^.sym^.symConst;
	    if cn^.kind = DTENUMERATION then
		tn := cn^.enumVal^.enumType;
		StabCheckType(tn);
	    elsif cn^.kind = DTSET then
		tn := cn^.setVal^.setType;
		StabCheckType(tn);
	    end;
	    GenOp(PCSYM); C('X'); StabComma; C('"');
		GenString(pn^.theModule^.name); C('.');
		GenString(pn^.sym^.name); C(':'); C('X');
		I(ord(pn^.isQualified));
		I(ord(pn^.isExport));
		I(ord(pn^.extern));
		StabConst(cn);
		C('"'); StabComma;
		I(STABNMOD2); StabComma; I(0); StabComma;
		I(0); StabComma; I(0);
	    StabEndLine;
	end;
	pn := pn^.next;
    end;
    GenOp(PCSYM); C('X'); StabComma; C('"');
	GenString(compileModuleName); C(':'); C('X');
	I(0); I(0); I(0);
	C('z'); C('"'); StabComma;
	I(STABNMOD2); StabComma; I(0); StabComma;
	I(0); StabComma; I(0);
    StabEndLine;
end StabGlobalPort;

procedure StabLine(fileName:String; lineNumber : integer);
begin
    if genDebugInfoFlag then
	if fileName # stabFileName then
	    (* stab file name *)
	    stabLineNumber := -1;
	end;
	if lineNumber # stabLineNumber then
	    GenOp(PCSYM); C('l'); StabComma; I(STABLINE); StabComma; I(0);
	    StabComma;
	    I(lineNumber);
	    StabEndLine;
	    stabLineNumber := lineNumber;
	end;
    end;
end StabLine;

end SymbolDump.
