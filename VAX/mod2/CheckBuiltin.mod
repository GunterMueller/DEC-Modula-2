implementation module CheckBuiltin;

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
    SWritef;

from MemLib import
    ALLOCATE;

from Machine import
    HugeInteger, WORDSIZE, BYTESIZE, HALFSIZE, UNITSIZE,
    MAXINT, MININT, MAXCHAR;

from Strings import
    String, NonHashString, AddChar, AddText, AddString;

from Tokens import
    Token;

from Globals import
    target, TargetMachine;

$if modula2 then
from Machine import CharSet;
from Strings import GetChar, NonHashText;
from Globals import standardKeywordFlag;
$end

from Symbols import
    EvalMode, PointerKind, GSNORMAL, GSPRIVATE, ConstNode, TypeNode,
    MEMFAST, MEMNORMAL, FieldNode, FieldList, FIELDTAG, VariantNode,
    ConstSetNode, VarNode, ProcNode, ExprNode, ExprList, EnumNode,
    DataType, DataTypeSet, ArrayKind, BuiltinProcType, ExprKind,
    ParamKind, FormerNode, FEENUMNAMETABLE, 
    ConstParamKind, integerTypeNode, realConstTypeNode,
    charTypeNode, cardinalTypeNode, booleanTypeNode, addressTypeNode,
    cardIntTypeNode, arrayOfCharTypeNode, longrealTypeNode,
    indexableTypes, MemoryOffset,
    StmtNode, globalProc, ExprSetList, ExprSetNode
$if modula2 then
    , IdentList, Symbol, ParamNode, ConstParamNode, ConstParamList,
    SYMTYPE, SYMPROC, ArrayKindSet, CHECKRANGE, CHECKDYNARRAY, realTypeNode,
    fileTypeNode, processTypeNode, ioStringTypeNode, 
    currScope, LookUpSymbol, QualifiedName
$else
    , stringDataType, alfaTypeNode, ExprKindSet,
    BuiltinProcTypeSet, STMTSTMTS,
    getProcNode, putProcNode, unitProcNode, 
    fnilProcNode, read4ProcNode, read8ProcNode, readeProcNode,
    readcProcNode, writelnProcNode,
    writecProcNode, fputcProcNode, writefProcNode, fprintfProcNode,
    writesProcNode, fwriteProcNode, maxProcNode, minProcNode, namProcNode
$end
    ;

from Errors import
    ErrorName, ErrorNumber, ExprError, ExprErrorName, 
    ExprErrorNumber, ExprErrorNameNumber, totalErrors;

from TypeInfo import
    NewTypeNode, BaseType, ActualType, NumberOf, LowerBoundOf, SizeOf;

from Decls import
    DefineVar, DefineVarInProc;

from Consts import
    OrdOf, CardinalConst;

from BuildExpr import
    AddToExprList, AddToExprSetList;

from Compatible import
    Assignable, Passable;

from CheckExpr import
    IsBadExpr, IsAddressableExpr, IsAddressableType, CheckExpr, 
    RefOpenArray, MakeExprVal, MakeExprVar, MakeExprConst, 
    MakeExprConstString, MakeExprOffset, ValueOrAddr;
    
from InitBuiltin import
     builtinFunctions;
    
$if modula2 then
from Errors import
    Error, ErrorNameNumber;
from Consts import
    DefaultExitStatus;
from TypeInfo import
    UpperBoundOf, ElementType;
from BuildExpr import
    BuildExprConst, SameExprLine;
from Compatible import
    Compatible;
from CheckExpr import
    CheckExprType, MakeExprBinChain, AssignExprNode, 
    MakeExprCheck, MakeExprSave, ScaleBySize; 
from InitBuiltin import
    allocateString, deallocateString, allocateProc, deallocateProc,
    maxreal, minreal, maxlongreal, minlongreal, constFunctions;
$else

from BuildExpr import
    NewExprNode, PrependExprList;
from BuildStmt import
    NewStmtNode, AddToStmtList, BuildStmtProc, SameStmtLine;
from CheckExpr import
    IntegerToReal, MakeExprProc, MakeExprFunc, SetSetType,
    CheckExprWithSubExprs;
from CheckStmt import
    MakeStmtAssign; from InitBuiltin import argcVarNode,
    inputVarNode, outputVarNode, errorVarNode, seedVarNode;
$end

(* For creating names for anonymous enumerated types 
    (see AllocateEnumNameTable) *)
var
    enumTableNumber : integer;

(* Offset of Unix file field in Pascal runtime file structure *)
const
    FILEOFFSET = longfloat(12 * BYTESIZE);

procedure CheckBuiltin(      stn	  : StmtNode; 
		       const procValExpr  : ExprNode;
		       const procNameExpr : ExprNode;
		       var   proc	  : ProcNode; 
		       var   params	  : ExprList; 
		       var   retType	  : TypeNode) : boolean; 
type
    ErrorKind = (ERRNONE, ERRNUMP);
var
    p, pnext, p1, p2, p3, p4, p2newen, counten, sizeen, adden, conen,
	aen: ExprNode;
    pt, pt1, pt2, pt3, pt4, pt1b, counttn, atn : TypeNode;
    nump, pnum, i : integer;
    fi, subnum : cardinal;
    error : ErrorKind;
    value, lowerBound, numDimensions : HugeInteger;
    size, offset : MemoryOffset;
    cn : ConstNode;
    ptrKind : PointerKind;
    bip : BuiltinProcType;
    saveTotalErrors : integer;
    arrayDesc : VarNode;


procedure CheckVarParam(const en : ExprNode; 
			const tn : TypeNode;
			const currParam : cardinal);
begin
    if not IsAddressableExpr(en) then
	ExprErrorNameNumber(en, proc^.name,
	    'Procedure $, parameter # % must be a variable', currParam);
    elsif not IsAddressableType(tn) then
	ExprErrorNameNumber(en, proc^.name, 
	 'Procedure $, parameter # % must be sized and aligned to address unit',
	    currParam);
    end;
end CheckVarParam;

procedure AllocateEnumNameTable(const enumType: TypeNode);
    var
	enum	      : EnumNode;
	numEntries    : cardinal;
	size	      : MemoryOffset;
	tableTypeNode : TypeNode;
	formerNode    : FormerNode;
	tableNumber   : array [0..11] of char;
	name	      : String;
begin
    enum := enumType^.enumList^.first;
    numEntries := enumType^.enumList^.last^.enumOrd - enum^.enumOrd + 1 
		    + 2 (* for # of names entry, and extra null name at end *);
    (* Size of pointers to names *)
    if target = TARGETVAX then
	(* For compatibility with existing Pascal library *)
	size := HALFSIZE * numEntries;
    else
	size := WORDSIZE * numEntries;
    end;
    (* Size of names *)
    repeat
	size := size + BYTESIZE * MemoryOffset(enum^.name^.length + 1);
	enum := enum^.next;
    until enum = nil;
    
    (* Create type for this guy *)
    tableTypeNode := NewTypeNode(DTARRAY);
    tableTypeNode^.size := size;
    tableTypeNode^.indexType := nil;
    tableTypeNode^.elementType := nil;
    tableTypeNode^.arrayKind := ARRAYNORMAL;

    (* Create variable name *)
    case target of
    | TARGETVAX :
	AddChar('$');
    | TARGETTITAN, TARGETTITANM :
	AddChar("'");
    end;
    AddText('EnumNames_');
    if enumType^.name # nil then
	AddString(enumType^.name);
	AddChar('_');
    end;
    inc(enumTableNumber);
    SWritef(tableNumber, '%d', enumTableNumber);
    AddText(tableNumber);
    name := NonHashString();

    (* Create FormerNode for name table *)
    new(formerNode);
    formerNode^.next := nil;
    formerNode^.formerType := enumType;
    formerNode^.formerTypeName := nil;
    formerNode^.value := nil;
    formerNode^.kind := FEENUMNAMETABLE;

    (* Define variable at global level *)
    enumType^.nameTable :=
	DefineVarInProc(name, tableTypeNode, MEMNORMAL, GSPRIVATE,
	    formerNode, globalProc);
end AllocateEnumNameTable;
 
$if modula2 then
procedure CheckWritef(const en        : ExprNode; 
		      const format    : String; 
		            pen       : ExprNode;
		      var   newFormat : String);
const
    FMTCHAR = '%';
var
    long     : boolean;
    pentn    : TypeNode;
    penbtn   : TypeNode;
    currPos, formatSize : integer;
    currChar : char;
    newFormatText  : array [0..1023] of char;
    newFormatIndex : [0..1024];

    procedure Advance;
    begin
	if currPos >= formatSize then
	    if currChar # chr(0) then
		ExprErrorNumber(en, 'Premature end of format, char # %',
		    currPos);
	    end;
	    currChar := chr(0);
	else
	    if currPos > 0 then
		newFormatText[newFormatIndex] := currChar;
		inc(newFormatIndex);
	    end;
	    currChar := GetChar(format,currPos);
	    currPos := currPos + 1;
	end;
    end Advance;


    procedure CheckWidth;
    var
	lasttime : boolean;
	tn : TypeNode;
    begin
	if currChar in CharSet{'-', '+', ' '} then
	    Advance;
	end;
	if currChar = '#' then
	    Advance;
	end;
	lasttime := true;
	repeat
	    if currChar = '*' then
		Advance;
		if pen = nil then
		    ExprErrorNumber(en,
			'No parameter for field width, char # %', currPos);
		else
		    tn := BaseType(CheckExpr(pen,EVALGET));
		    if IsBadExpr(pen) or (tn^.kind = DTANY) then
		    elsif (tn^.kind in DataTypeSet{DTCARDINAL,DTINTEGER}) then
		    else
			ExprErrorNumber(en, 
			    'Field width not integer or cardinal, char # %', 
			    currPos);
		    end;
		    pen := pen^.next;
		end;
	    else
		while currChar in CharSet{'0'..'9'} do
		    Advance;
		end;
	    end;
	    if lasttime and (currChar = '.') then
		Advance;
		lasttime := false;
	    else
		lasttime := true;
	    end;
	until lasttime;
    end CheckWidth;

begin
    currPos := 0;
    currChar := chr(0);
    newFormatIndex := 0;
    formatSize := format^.length;
    while (currPos < formatSize) do
	Advance;
	if currChar = FMTCHAR then
	    Advance;
	    if currChar # FMTCHAR then  (* %% means one % *)
		CheckWidth;
		if (pen = nil) then
		    if (currChar # 0C) then
			ExprErrorNumber(en,
			    'No parameter for format, char # %', currPos);
			currPos := formatSize;  (* Skip rest of format *)
		    end;
		else
		    if currChar = '#' then
			Advance;
		    end;
		    long := currChar = 'l';
		    if long then
			Advance;
		    end;

		    case currChar of
		    | 'd','o','x','u':		(* integer or cardinal *)
			pentn := CheckExpr(pen,EVALGET);
			if IsBadExpr(pen) then
			elsif not (Passable(integerTypeNode,PARAMCONST,
					    pentn,pen) or
				   Passable(addressTypeNode,PARAMCONST,
					    pentn,pen)) then
			    ExprErrorNumber(en,
			       'Format requires integer or cardinal, char # %', 
				currPos);
			end;
			pen := pen^.next;

		    | 'f','e','g','F','E','G':  (* real or longreal *)
			if long then
			    (* 'l' does not apply to floating point in printf *)
			    dec(newFormatIndex);
			end;
			if currChar in CharSet{'F','E','G'} then
			    currChar := chr(ord(currChar)-ord('A')+ord('a'));
			    long := true;
			end;
			pentn := CheckExpr(pen,EVALGET);
			if IsBadExpr(pen) then
			elsif long then
			    if not Passable(longrealTypeNode,PARAMCONST,
				    pentn,pen) then
				ExprErrorNumber(en,
				    'Format requires longreal, char # %', 
				    currPos);
			    end;
			else
			    if not Passable(realTypeNode,PARAMCONST,
				    pentn,pen) then
				ExprErrorNumber(en,
				    'Format requires real, char # %', currPos);
			    end;

			end;
			pen := pen^.next;

		    | 'c':			(* character *)
			if long then
			    ExprErrorNumber(en, 
				'Format "lc" should be "c", char # %', currPos);
			end;
			pentn := CheckExpr(pen,EVALGET);
			if IsBadExpr(pen) then
			elsif not Passable(charTypeNode,PARAMCONST,pentn,pen)
			then
			    ExprErrorNumber(en,
				'Format requires char, char # %', currPos);
			end;
			pen := pen^.next;
		    
		    | 's':			(* string *)
			if long then
			    ExprErrorNumber(en, 
				'Format "ls" should be "s", char # %', currPos);
			end;
			pentn := CheckExpr(pen,EVALPOINT);
			if IsBadExpr(pen) then
			elsif not Passable(arrayOfCharTypeNode,PARAMARRAYCONST,
					    pentn,pen) then
			    ExprErrorNumber(en,
				'Format requires string, char # %', currPos);
			else
			    RefOpenArray(pen,pentn);
			    if pen^.exprType^.kind = DTPOINTER then
                                pen^.exprType := ioStringTypeNode;
                            end;
			end;
			pen := pen^.next;
		    
		    | 'n':			(* enumeration *)
			if long then
			    ExprErrorNumber(en, 
				'Format "ln" should be "n", char # %', currPos);
			end;
			pentn := CheckExpr(pen,EVALGET);
			penbtn := BaseType(pentn);
			if IsBadExpr(pen) then
			elsif (penbtn^.kind # DTBOOLEAN) and
				(penbtn^.kind # DTENUMERATION) then
			    ExprErrorNumber(en,
				    'Format requires enumeration, char # %',
				    currPos);
			elsif penbtn^.nameTable = nil then
			    AllocateEnumNameTable(penbtn);
			end;
			currChar := 's';
			pen := pen^.next;
 
		    | 0C: (* already said format string too short *)
		    
		    else
			ExprErrorNumber(en,
			    'Invalid format character, char # %', currPos);
			(* Probably should have been %%, so don't eat param *)
		    end (* case currChar *);
		end (* if pen = nil then ... else ... *)
	    end (* if currChar # FMTCHAR *);
	end (* if currChar = FMTCHAR *);
    end (* while *);
    if (pen # nil) then
	ExprError(en,'Too many parameters for format string');
    end;
    
    newFormatText[newFormatIndex] := currChar;
    newFormatText[newFormatIndex+1] := 0C;
    newFormat := NonHashText(newFormatText);
end CheckWritef;

procedure CheckReadf(const en     : ExprNode; 
		     const format : String; 
		           pen    : ExprNode);
const
    FMTCHAR = '%';
    SUPPRESSCHAR = '*';
    LBRACK = '[';
    RBRACK = ']';
var
    suppress, long  : boolean;
    pentn	    : TypeNode;
    currPos, formatSize : integer;
    currChar	    : char;
    currParam       : cardinal;
    saveTotalErrors : cardinal;

    procedure Advance;
    begin
	if currPos >= formatSize then
	    if currChar # chr(0) then
		ExprErrorNumber(en,
		    'Premature end of format, char # %', currPos);
	    end;
	    currChar := chr(0);
	else
	    currChar := GetChar(format,currPos);
	    currPos := currPos + 1;
	end;
    end Advance;


    procedure CheckWidth;
    begin
	while currChar in CharSet{'0'..'9'} do
	    Advance;
	end;
    end CheckWidth;

begin (* CheckReadf *)
    formatSize := format^.length;
    currPos := 0;
    currParam := 0;
    saveTotalErrors := totalErrors;

    while (currPos < formatSize) do
	Advance;
	if currChar = FMTCHAR then
	    Advance;
	    if currChar # FMTCHAR then  (* %% means one % *)
		suppress := currChar = SUPPRESSCHAR;
		if suppress then
		    Advance;
		end;
		CheckWidth;
		if not suppress and (pen = nil) then
		    if (currChar # 0C) then
			ExprErrorNumber(en,
			    'No parameter for format, char # %', currPos);
			currPos := formatSize;  (* Skip rest of format *)
		    end;
		else
		    long := currChar = 'l';
		    if long then
			Advance;
		    end;

		    case currChar of
		    | 'd','o','x','u':		(* integer or cardinal *)
			if not suppress then
			    currParam := currParam + 1;
			    pentn := CheckExpr(pen, EVALPUT);
			    CheckVarParam(pen, pentn, currParam);
			    if IsBadExpr(pen) then
			    elsif Assignable(integerTypeNode,pentn,pen) = nil
			    then
				ExprErrorNumber(en,
				'Format requires integer or cardinal, char # %',
				    currPos);
			    end;
			    pen := pen^.next;
			end;

		    | 'f','e','g','F','E','G':  (* real or longreal *)
			if not suppress then
			    if currChar in CharSet{'F','E','G'} then
				long := true;
			    end;
			    currParam := currParam + 1;
			    pentn := CheckExpr(pen, EVALPUT);
			    CheckVarParam(pen, pentn, currParam);
			    if IsBadExpr(pen) then
			    elsif long then
				if not Passable(longrealTypeNode,PARAMVAR,
					pentn,pen) then
				    ExprErrorNumber(en,
					'Format requires longreal, char # %', 
					currPos);
				end;
			    else
				if not Passable(realTypeNode,PARAMVAR,pentn,pen)
				then
				    ExprErrorNumber(en,
					'Format requires real, char # %', 
					currPos);
				end;
			    end;
			    pen := pen^.next;
			end;

		    | 'c':			(* character *)
			if not suppress then
			    currParam := currParam + 1;
			    pentn := CheckExpr(pen, EVALPUT);
			    CheckVarParam(pen, pentn, currParam);
			    if IsBadExpr(pen) then
			    elsif not Passable(charTypeNode,PARAMVAR,pentn,pen)
			    then
				ExprErrorNumber(en,
				    'Format requires char, char # %', currPos);
			    end;
			    pen := pen^.next;
			end;
		    
		    | 's':			(* string *)
			if not suppress then
			    currParam := currParam + 1;
			    pentn := CheckExpr(pen, EVALPUT);
			    if IsBadExpr(pen) then
			    elsif not Passable(arrayOfCharTypeNode,
				    PARAMARRAYVAR, pentn, pen) then
				ExprErrorNumber(en,
				   'Format requires string, char # %', currPos);
			    else
				RefOpenArray(pen,pentn);
				CheckVarParam(pen, pentn, currParam);
				if pen^.exprType^.kind = DTPOINTER then
				    pen^.exprType := ioStringTypeNode;
				end;
			    end;
			    pen := pen^.next;
			end;

		    | LBRACK : (* string (allowable chars in brackets) *)
			while (currChar # chr(0)) and (currChar # RBRACK) do
			    Advance;
			end;
			if currChar # RBRACK then
			    ExprErrorNumber(en,'Missing ], char # %', currPos);
			end;
			if not suppress then
			    (* need a string *)
			    currParam := currParam + 1;
			    pentn := CheckExpr(pen, EVALPUT);
			    if IsBadExpr(pen) then
			    elsif not Passable(arrayOfCharTypeNode,
				    PARAMARRAYVAR, pentn, pen) then
				ExprErrorNumber(en,
				    'Format requires string, char # %',currPos);
			    else
				RefOpenArray(pen,pentn);
				CheckVarParam(pen, pentn, currParam);
				if pen^.exprType^.kind = DTPOINTER then
				    pen^.exprType := ioStringTypeNode;
				end;
			    end;
			    pen := pen^.next;
			end;
		    else
			ExprErrorNumber(en,
			    'Invalid format character, char # %', currPos);
		    end (* case currChar *);
		end (* if not suppress and (pen = nil) then ... else... *);
	    end (* if currChar # FMTCHAR *);
	end (* if currChar = FMTCHAR *);
    end (* while *);
    if (pen # nil) then
	ExprError(en,'Too many parameters for format string');
    end;

end CheckReadf;

procedure PointerOrAddress(const en : ExprNode; const tn : TypeNode);
begin
    if tn^.kind = DTPOINTER then
	ValueOrAddr(en,addressTypeNode,EVALGET);
	ExprError(en,'Read/write of pointer instead of data');
    else
	RefOpenArray(en,tn);
    end;
end PointerOrAddress;

(* UserOrBuiltin: look up whichName in current scope.  If found, use it     *)
(* instead of whichProc builtin procedure.  Set procExpr to ExprNode	    *)
(* containing correct procedure or nil for error.  Note: this only really   *)
(* works for allocate and deallocate, since it checks that the parameter    *)
(* list is an address and a cardinal. *)
procedure UserOrBuiltin(const whichProc : ProcNode; 
			const whichName : String; 
			const erroren   : ExprNode) : ExprNode;
var
    sym : Symbol;
    newProc : ProcNode;
    procExpr : ExprNode;
    procExprTn : TypeNode;
    pn1, pn2 : ParamNode;
    cn : ConstNode;
begin
    newProc := whichProc;
    sym := LookUpSymbol(whichName,currScope);
    if sym = nil then
	if standardKeywordFlag then
	    ExprErrorName(erroren,whichName,
		'Procedure $ not found for new/dispose substitution');
	    return nil;
	end;
	(* no error of non-standard, use the default memory module *)
    elsif sym^.kind # SYMPROC then
	newProc := nil;		(* wrong kind of symbol *)
    elsif sym^.symProc = whichProc then
	(* found the default *)
    else
	newProc := sym^.symProc;
	if newProc^.procType^.paramList = nil then
	    newProc := nil;	(* no parameter list *)
	else
	    pn1 := newProc^.procType^.paramList^.first;
	    if (pn1 = nil) or (pn1^.next = nil) then
		newProc := nil;	(* no parameter list, or just one param *)
	    else
		pn2 := pn1^.next;
		if (pn1^.paramType # addressTypeNode) or
		    not (pn2^.paramType^.kind in 
			DataTypeSet{DTCARDINAL,DTINTEGER}) or
		    (pn2^.next # nil) then
		    newProc := nil;	(* wrong types or extra parameters *)
		end;
	    end;
	end;
    end;
    if newProc = nil then
	ExprErrorName(erroren,whichName,
	    'Procedure $ has the wrong type for new/dispose substitution');
	return nil;
    else
	new(cn);
	cn^.kind := DTPROC;
	cn^.procVal := newProc;
	procExpr := BuildExprConst(cn);
	procExprTn := CheckExpr(procExpr,EVALPOINT);
	assert(not IsBadExpr(procExpr));
	return procExpr;
    end;
end UserOrBuiltin;

$end (* if modula2 *)


procedure RecordTypeSize(const recordType : TypeNode; 
			       tagExpr    : ExprNode;
			 const procName   : String) : MemoryOffset;
(* Compute size of record given list of constants for tag fields.  Only tag
   fields that are at the very end of the record count. *)
    var tagType   : TypeNode;
	fieldList : FieldList;
	tagField  : FieldNode;
	variant   : VariantNode;
	tagValue  : HugeInteger;
	variantSet: ConstSetNode;
	found     : boolean;
begin
    fieldList := recordType^.fieldList;
    while tagExpr # nil do
	tagField := fieldList^.last;
	if (tagField = nil) or (tagField^.kind # FIELDTAG) then
	    ExprErrorName(tagExpr, procName, 
			    'Procedure $: too many tags for record');
	    exit while;
	end;
	tagType := CheckExpr(tagExpr, EVALGET);
	if IsBadExpr(tagExpr) then
	    exit while;
	end;
	if tagExpr^.kind # EXPRCONST then
	    ExprErrorName(tagExpr, procName,
			    'Procedure $: tag value must be a constant');
	    exit while;
	end;
	if Assignable(tagField^.fieldType, tagType, tagExpr) = nil then
	    ExprErrorName(tagExpr, tagField^.name,
			    'Tag type not compatible with tag field $');
	    exit while;
	end;
	
	(* Find variant part to match the specified tag *)
	tagValue := OrdOf(tagExpr^.exprConst);
        variant := tagField^.variantList^.first;
	while (variant # nil) and not variant^.elsePart do
	    variantSet := variant^.tag^.first;
	    found := false;
	    while variantSet # nil do
		if (OrdOf(variantSet^.lower) <= tagValue) and
		   (tagValue <= OrdOf(variantSet^.upper)) then
		    found := true;
		    variantSet := nil;
	        else 
		    variantSet := variantSet^.next;
		end;
	    end (* while variantSet # nil *);
	    if found then
	        exit while;
	    end;
	    variant := variant^.next;
	end (* while variant # nil *);
	assert(variant # nil, "Oops...else case didn't catch unspeced tag");
	
	(* Okay, we found it.  Move down to the fieldList for the variant,
	   and move to the next tag specified. *)
        fieldList := variant^.fieldList;
	tagExpr := tagExpr^.next;
    end (* while tagExpr # nil *);

    return fieldList^.size;
end RecordTypeSize;

(* CopyExpr: duplicate the expression tree *)
procedure CopyExpr(en : ExprNode) : ExprNode;
var
    nen	      : ExprNode;
    pen, npen : ExprNode;
    npl       : ExprList;
    nesl      : ExprSetList;
    esn, nesn : ExprSetNode;
begin
    new(nen);
    nen^ := en^;	(* copy miscellaneous fields *)
    case en^.kind of
    | EXPRCONST, EXPRVAR :
	(* Leaves; no need for further copying *)

    | EXPRVAL :
	nen^.exprVal := CopyExpr(en^.exprVal);
    
    | EXPRNAME,
      EXPRSYM :
	ExprError(en,'CopyExpr: unexpected expr')

    | EXPRUNOP :
	nen^.opnd := CopyExpr(en^.opnd);
    
    | EXPRBINOP :
	nen^.opnd1 := CopyExpr(en^.opnd1);
	nen^.opnd2 := CopyExpr(en^.opnd2);
    
    | EXPRFUNC :
	nen^.func := CopyExpr(en^.func);
	npl := AddToExprList(nil,nil);
	pen := en^.params^.first;
	while pen # nil do
	    npen := CopyExpr(pen);
	    npl := AddToExprList(npl,npen);
	    pen := pen^.next;
	end;
	nen^.params := npl;
    
    | EXPRCHECK :
	nen^.checkExpr := CopyExpr(en^.checkExpr);
    
    | EXPRSET :
	nesl := nil;
	if en^.setExpr # nil then
	    esn := en^.setExpr^.first;
	    while esn # nil do
		new(nesn);
		nesn^.lower := CopyExpr(esn^.lower);
		if esn^.upper = nil then
		    nesn^.upper := nil;
		else
		    nesn^.upper := CopyExpr(esn^.upper);
		end;
		nesl := AddToExprSetList(nesl,nesn);
		esn := esn^.next;
	    end;
	end;
	nen^.setExpr := nesl;
    
    | EXPRSAVE :
	nen^.exprSave := CopyExpr(en^.exprSave);
    
    | EXPRINLINE :
	ExprError(en,'CopyExpr: INLINE expr?');
    end;
    return nen;
end CopyExpr;


$if pascal then
procedure MakeSimpleFileExpr(var fileExpr     : ExprNode;
			     var fileEvalMode : HugeInteger);
var
    fileVarNode : VarNode;     (* Temporary var for address of file *)
    nstn	: StmtNode;    (* temp := complicated file expression *)
begin  
    (* Make sure we have stn^.kind = STMTSTMTS *)
    if stn = nil then
	stn := NewStmtNode(STMTSTMTS);
    else
	stn^.kind := STMTSTMTS;
    end;
    stn^.stmts := nil;
    (* If file expression is complicated, calculate it once and save it *)
    if p1^.kind = EXPRVAR then
	fileExpr := p1;
	fileEvalMode := longfloat(ord(EVALPOINT));
    else
	fileVarNode := DefineVar(nil, addressTypeNode, MEMFAST, GSNORMAL, nil);
	nstn := MakeStmtAssign(
	    MakeExprVar(fileVarNode, p1), addressTypeNode, p1);
	stn^.stmts := AddToStmtList(stn^.stmts, nstn);
	fileExpr := MakeExprVal(addressTypeNode, MakeExprVar(fileVarNode, p1));
	fileEvalMode := longfloat(ord(EVALGET));
    end;
end MakeSimpleFileExpr;

procedure AddCall(const stn    : StmtNode;      (* stn^.kind = STMTSTMTS *)
		  const pn     : ProcNode;
		  const infoen : ExprNode;      (* For file, line info *)
		  const params : ExprList);
var
    nstn : StmtNode;
begin
    nstn := BuildStmtProc(MakeExprProc(pn, infoen), params);
    SameStmtLine(nstn, infoen);
    stn^.stmts := AddToStmtList(stn^.stmts, nstn);
end AddCall;

procedure CheckWriteBinaryParams();
(* Convert to sequence of statements "f^ := p; put(f)" for each param p *)
var
    fileExpr     : ExprNode;    (* Expression for file address *)
    fileEvalMode : HugeInteger; (* Mode to use on fileExpr in put, fnil calls *)
    nstn	 : StmtNode;
    p            : ExprNode;
    pt		 : TypeNode;
    ft		 : TypeNode;    (* pt1^.fileType *)
    pnum	 : cardinal;
    writeParams  : ExprList;    (* Param list for put(f), fnil(f) *)
    unitParams   : ExprList;	(* Param list for unit(f) *)
    fuparrowExpr : ExprNode;    (* f^ is EXPRFUNC(fnil) *)
    needUnit     : boolean;     (* Need UNIT call ? *)
begin
    (* Compensate for CheckExpr(p1, EVALGET) when 1st param is file var *)
    if p1^.kind = EXPRVAL then
	p1^ := p1^.exprVal^;
	p1^.exprType := addressTypeNode;
    end;
    
    MakeSimpleFileExpr(fileExpr, fileEvalMode);
    p := p1^.next;
    if p = nil then
	ExprError(p1, 'write(f) needs arguments');
	return;
    end;
    ft := pt1^.fileType;
    pnum := 2;
    needUnit := true;
    repeat
	pt := CheckExpr(p, EVALGET);
	if IsBadExpr(p) or (pt^.kind = DTANY) then
	else
	    (* Create f^ := p assignment *)
	    writeParams := AddToExprList(nil, CopyExpr(fileExpr));
	    writeParams := AddToExprList(writeParams, 
		MakeExprConst(cardinalTypeNode, fileEvalMode));
	    fuparrowExpr := MakeExprFunc(fnilProcNode, p, writeParams, ft);
	    fuparrowExpr^.baseVar := p1^.baseVar;
	    if Assignable(ft, pt, p) = nil then
		ExprErrorNumber(p, 
		    'write: parameter # % is not assignable to file type',pnum);
	    else
		nstn := MakeStmtAssign(fuparrowExpr, ft, p);
		stn^.stmts := AddToStmtList(stn^.stmts, nstn);
	    end;
	    (* Create put(unit(f)) or put(f) call *)
	    if needUnit then
		unitParams := AddToExprList(nil, CopyExpr(fileExpr));
		unitParams := AddToExprList(unitParams,
		    MakeExprConst(cardinalTypeNode, fileEvalMode));
		writeParams := AddToExprList(nil,
		    MakeExprFunc(unitProcNode, p, unitParams, addressTypeNode));
		writeParams := AddToExprList(writeParams,
			    MakeExprConst(cardinalTypeNode,
				longfloat(ord(EVALGET))));
		needUnit := false;
	    else
		writeParams := AddToExprList(nil, CopyExpr(fileExpr));
		writeParams := AddToExprList(writeParams, 
				MakeExprConst(cardinalTypeNode, fileEvalMode));
	    end;
	    AddCall(stn, putProcNode, p, writeParams);
	 end;
	p := p^.next;
	inc(pnum);
    until p = nil;
end CheckWriteBinaryParams;

procedure CheckReadBinaryParams();
(* Convert to sequence of statements "p := f^; get(f)" for each param p *)
var
    fileExpr     : ExprNode;    (* Expression for file address *)
    fileEvalMode : HugeInteger; (* Mode to use on fileExpr in get, fnil calls *)
    nstn	 : StmtNode;
    p            : ExprNode;
    pt		 : TypeNode;
    pnum	 : cardinal;
    readParams   : ExprList;    (* Param list for get(f), fnil(f) *)
    unitParams   : ExprList;    (* Param list for unit(f) *)
    fuparrowExpr : ExprNode;    (* f^ is EXPRVAL of EXPRFUNC(fnil) *)
    needUnit     : boolean;     (* Need UNIT call ? *)
begin
    MakeSimpleFileExpr(fileExpr, fileEvalMode);

    p := p1^.next;
    if p = nil then
	ExprError(p1, 'read(f) needs arguments');
	return;
    end;
    pnum := 2;
    needUnit := true;
    repeat
	pt := CheckExpr(p, EVALPUT);
	if IsBadExpr(p) or (pt^.kind = DTANY) then
	elsif not IsAddressableExpr(p) then
	    ExprErrorNumber(p, 'read: cannot assign to parameter # %', pnum);
	else
	    (* Create p := f^ assignment *)
	    readParams := AddToExprList(nil, CopyExpr(fileExpr));
	    readParams := AddToExprList(readParams, 
			    MakeExprConst(cardinalTypeNode, fileEvalMode));
	    fuparrowExpr := 
		MakeExprVal(pt1^.fileType, 
		    MakeExprFunc(fnilProcNode, p, readParams, addressTypeNode));
	    if Assignable(pt, pt1^.fileType, fuparrowExpr) = nil then
		ExprErrorNumber(p, 
		    'read: file type is not assignable to parameter # %',pnum);
	    else
		nstn := MakeStmtAssign(p, pt, fuparrowExpr);
		stn^.stmts := AddToStmtList(stn^.stmts, nstn);
	    end;
	    (* Create get(unit(f)) or get(f) call *)
	    if needUnit then
		unitParams := AddToExprList(nil, CopyExpr(fileExpr));
		unitParams := AddToExprList(unitParams,
		    MakeExprConst(cardinalTypeNode, fileEvalMode));
		readParams := AddToExprList(nil,
		    MakeExprFunc(unitProcNode, p, unitParams, addressTypeNode));
		readParams := AddToExprList(readParams,
		    MakeExprConst(cardinalTypeNode, longfloat(ord(EVALGET))));
		needUnit := false;
	    else
		readParams := AddToExprList(nil, CopyExpr(fileExpr));
		readParams := AddToExprList(readParams, 
			    MakeExprConst(cardinalTypeNode, fileEvalMode));
	    end;
	    AddCall(stn, getProcNode, p, readParams);
	 end;
	p := p^.next;
	inc(pnum);
    until p = nil;
end CheckReadBinaryParams;

procedure CheckReadParams(pt : TypeNode);
(* The mapping of different types to the runtime library is :
i	    i := READ4(f);
r	    r := READ8(f);
c	    c := READC(f);
e	    e := READE(f, NameTable(e)); *)

var 
    fileExpr     : ExprNode;    (* Expression for file address *)
    fileEvalMode : HugeInteger; (* Mode to use on fileExpr in get, fnil calls *)
    nstn	 : StmtNode;
    readExpr	 : ExprNode;    (* READx(f) call *)
    readParams   : ExprList;    (* params (f, EVALx) for READx call *)
    p		 : ExprNode;
    pbt          : TypeNode;
    pnum	 : cardinal;

begin
    MakeSimpleFileExpr(fileExpr, fileEvalMode);

    p := p1^.next;
    if p = nil then 
	ExprError(p1, 'read(f) needs arguments');
	return;
    end;
    if pt = nil then
	(* CheckExpr done on file to read from *)
	pt := CheckExpr(p, EVALPUT);
	pnum := 2;
    else
	(* CheckExpr done on first param to read into *)
	pnum := 1;
    end;
    loop (* through all args *)
	if IsBadExpr(p) or (pt^.kind = DTANY) then
	elsif not IsAddressableExpr(p) then
	    ExprErrorNumber(p, 'read: cannot assign to parameter # %', pnum);
	else
	    (* Create p := READx(f) assignment *)
	    readParams := AddToExprList(nil, CopyExpr(fileExpr));
	    readParams := AddToExprList(readParams, 
			    MakeExprConst(cardinalTypeNode, fileEvalMode));
	    pbt := BaseType(pt);
	    case pbt^.kind of
	    | DTINTEGER :
		readExpr := MakeExprFunc(read4ProcNode, p, readParams, pbt);
	    | DTLONGREAL :
		readExpr := MakeExprFunc(read8ProcNode, p, readParams, pbt);
	    | DTCHAR :
		readExpr := MakeExprFunc(readcProcNode, p, readParams, pbt);
	    | DTENUMERATION, DTBOOLEAN :
		if pbt^.nameTable = nil then
		    AllocateEnumNameTable(pbt);
		end;
		readParams := 
		    AddToExprList(readParams, MakeExprVar(pbt^.nameTable, p));
		readExpr := MakeExprFunc(readeProcNode, p, readParams, pbt);
	    | else
	        ExprErrorNameNumber(p, stringDataType[pt^.kind],
		    'Procedure read, parameter #%: cannot read type $', pnum);
		readExpr := nil;
	    end (* case *);
	    nstn := MakeStmtAssign(p, pt, readExpr);
	    stn^.stmts := AddToStmtList(stn^.stmts, nstn);
	end;
	p := p^.next;
	if p = nil then
	    exit;
	end;
	inc(pnum);
	pt := CheckExpr(p, EVALPUT);
    end (* loop *);
    if proc^.builtin = BIPreadln then
	readParams := AddToExprList(nil, CopyExpr(fileExpr));
	readParams := AddToExprList(readParams, 
			MakeExprConst(cardinalTypeNode, fileEvalMode));
	AddCall(stn, proc, p1, readParams);
    end;
end CheckReadParams;

procedure MakeWriteln(const p1 : ExprNode);
var
    fputcParams : ExprList;
    cFileExpr   : ExprNode;
begin
    if p1^.doCheck then
	params := AddToExprList(params,
	    MakeExprConst(integerTypeNode, longfloat(ord(EVALPOINT))));
	proc := writelnProcNode;
    else
	(* fputc(chr(10), f^.cFile) *)
	fputcParams := AddToExprList(nil, 
	    MakeExprConst(integerTypeNode, 10.0));
	cFileExpr :=
	    MakeExprOffset(addressTypeNode, TKPLUS, p1, FILEOFFSET);
	ValueOrAddr(cFileExpr, addressTypeNode, EVALGET);
	fputcParams := AddToExprList(fputcParams, cFileExpr);
	proc := fputcProcNode;
	params := fputcParams;
    end;
end MakeWriteln;

procedure CheckWriteParams(const firstArgEvaluated : boolean);

(* The mapping of different types to Unix interface code are :
s	    WRITES(f, s, len(s), 1, 12(f))
s:n>len(s)  WRITEF(f, 12(f), '%ds', '') (* d = n - len(s) *)
	    WRITES(f, s, len(s), 1, 12(f))
s:n<=len(s) WRITES(f, s, n, 1, 12(f))
s:n?	    WRITEF(f, 12(f), '%*s', MAX(n,len(s),0), '');
	    WRITES(f, s, min(n, len(s)), 1, 12(f))
i	    WRITEF(f, 12(f), '%10D', i)
i:n	    WRITEF(f, 12(f), '%nD', i)
i:n?	    WRITEF(f, 12(f), '%*D', n, i)
r	    WRITEF(f, 12(f), '%21.14e')
r:n	    WRITEF(f, 12(f), '%n.me') where m = max(1, n-7)
r:n:m       WRITEF(f, 12(f), '%n.mf', r)
r:n?	    WRITEF(f, 12(f), '%*.*e', n, MAX(n, 7, 1))
r:n:m?      WRITEF(f, 12(f), '%n.*f', m, r)
r:n?:m      WRITEF(f, 12(f), '%*.mf', n, r)
r:n?:m?     WRITEF(f, 12(f), '%*.*f', n, m, r)
c	    WRITEC(f, c, 12(f))
c:n<=1      WRITEC(f, c, 12(f))
c:n>1       WRITEF(f, 12(f), '%nc', c)
c:n?	    WRITEF(f, 12(f), '%*c', n, c)
e	    e:1
e:n	    WRITEF(f, 12(f), '%ns', NAM(ord(e), NameTable(e)))
e:n?	    WRITEF(f, 12(f), '%*s', n, NAM(ord(e), NameTable(e))

The NameTable for an enumerated type looks like
NameTable   .WORD16     # elements (n)
	    .WORD16     Name0 - NameTable - 2
	    .WORD16     Name1 - NameTable - 2
	    ...
	    .WORD16     Namen - NameTable - 2
Name0	    .BYTE       name0 string followed by null
Name1	    .BYTE       name1 string followed by null
	    ...
Namen			nothing at all...just a pointer past end of table

If runtime-checking is off, the f parameter at the beginning of each routine
is omitted, and the following routine mapping should be used:

WRITES  fwrite
WRITEF  fprintf
WRITEC  fputc

*)
 
var 
    fileExpr     : ExprNode;    (* Expression for file address *)
    fileEvalMode : HugeInteger; (* Mode to use on fileExpr in write calls *)
    cFileExpr    : ExprNode;    (* Expression for UNIX file *)
    p		 : ExprNode;
    nextp        : ExprNode;    (* p gets linked into a new param list *)
    pt, pbt	 : TypeNode;
    pnum	 : cardinal;
    wtn		 : TypeNode;
    newParams    : ExprList;

    procedure AddWritefCall(const formatString : String;
			    const widthExpr    : ExprNode;
			    const decimalExpr  : ExprNode;
			    const p	       : ExprNode);
    var
	newParams : ExprList;
    begin
	if p1^.doCheck then
	    newParams := AddToExprList(nil, CopyExpr(fileExpr));
	    newParams := AddToExprList(newParams, 
			    MakeExprConst(cardinalTypeNode, fileEvalMode));
	    newParams := AddToExprList(newParams, CopyExpr(cFileExpr));
	else
	    newParams := AddToExprList(nil, CopyExpr(cFileExpr));
	end;
	newParams := AddToExprList(newParams, 
	    MakeExprConstString(formatString));
	if widthExpr # nil then
	    newParams := AddToExprList(newParams, widthExpr);
	end;
	if decimalExpr # nil then
	    newParams := AddToExprList(newParams, decimalExpr);
	end;
	newParams := AddToExprList(newParams, p);
	if p1^.doCheck then
	    AddCall(stn, writefProcNode, p, newParams);
	else
	    AddCall(stn, fprintfProcNode, p, newParams);
	end;
    end AddWritefCall;
    
    procedure WidthFormat(const p	     : ExprNode; 
			  const defaultWidth : cardinal; 
			  const fType        : char);
    (* Map integer, char, string, and enumerations into the right
       low-level output calls (WRITES or fwrite; WRITEF or fprintf;
       WRITEC or fputc *)
	var fs		: array [0..15] of char;
	    wtn		: TypeNode;
	    width       : integer;
	    constWidth  : boolean;
	    formatString: String;
	    newParams   : ExprList;
	    minParams   : ExprList;
	    maxParams   : ExprList;
	    namParams   : ExprList;
    begin
	if p^.decimalExpr # nil then
	    if p^.decimalExpr^.kind in ExprKindSet{EXPROCT,EXPRHEX} then
		ExprError(p, 'hex and octal format allowed only on integers');
	    else
		ExprError(p, 'e:m:n format allowed only on reals');
	    end;
	end;
	constWidth := true;
	width := defaultWidth;
	if p^.widthExpr # nil then   (* width was specified *)
	    wtn := BaseType(CheckExpr(p^.widthExpr, EVALGET));
	    if IsBadExpr(p^.widthExpr) or (wtn^.kind = DTANY) then
	    elsif wtn^.kind # DTINTEGER then
		ExprError(p, 'Procedure write: width must be an integer'); 
	    elsif p^.widthExpr^.kind = EXPRCONST then
		width := trunc(OrdOf(p^.widthExpr^.exprConst));
	    else
		constWidth := false;
	    end;
	end;
	case fType of
	| 'c' :     (* character *)
	    if (constWidth) and (width <= 1) then     
		(* fputc(c, f^.cFile)  or  WRITEC(f, c, f^.cFile) *)
		if p1^.doCheck then
		    newParams := AddToExprList(nil, CopyExpr(fileExpr));
		    newParams := AddToExprList(newParams, 
			MakeExprConst(cardinalTypeNode, fileEvalMode));
		    newParams := AddToExprList(newParams, p);
		    newParams := AddToExprList(newParams, CopyExpr(cFileExpr));
		    AddCall(stn, writecProcNode, p, newParams);
		else
		    newParams := AddToExprList(nil, p);
		    newParams := AddToExprList(newParams, CopyExpr(cFileExpr));
		    AddCall(stn, fputcProcNode, p, newParams);
		end;
	    else
		(* WRITEF(f, f^.cFile, '%nc', [n, ] c) or
		   fprintf(f^.cFile, '%nc', [n, ] c) *)
		if constWidth then
		    Swritef(fs, '%%%dc', width);
		    AddText(fs);
		    AddChar(0C);
		    AddWritefCall(NonHashString(), nil, nil, p);
		else
		    AddText('%*c');
		    AddChar(0C);
		    AddWritefCall(NonHashString(), p^.widthExpr, nil, p);
		end;
	    end;
    
	| 's' :
	    if (not constWidth) or (width > defaultWidth) then
		(* WRITEF(f, f^.cFile, '%*s', MAX(n,len(s),0), '') or
		   fprintf(f^.cFile, '%*s', MAX(n,len(s),0), '') *)
		if constWidth then
		    Swritef(fs, '%%%ds', width - defaultWidth);
		    AddText(fs);
		    AddChar(0C);
		    formatString := NonHashString();
		    width := defaultWidth;  (* For WRITES call *)
		    AddChar(0C);
		    AddWritefCall(formatString, nil, nil,
			MakeExprConstString(NonHashString()));
		else
		    AddText('%*s');
		    AddChar(0C);
		    formatString := NonHashString();
		    maxParams := AddToExprList(nil, CopyExpr(p^.widthExpr));
		    maxParams := AddToExprList(maxParams,
			MakeExprConst(integerTypeNode, 
			    longfloat(defaultWidth)));
		    maxParams := AddToExprList(maxParams,
			    MakeExprConst(integerTypeNode, 0.0));
		    AddChar(0C);
		    AddWritefCall(formatString,
			MakeExprFunc(maxProcNode, p, maxParams, integerTypeNode),
			nil,
			MakeExprConstString(NonHashString()));
		end;
	    end;
    
	    (* WRITES(f, adr(s), min(n,len(s)), 1, f^.cFile) or
	       fwrite(adr(s), min(n,len(s)), 1, f^.cFile) *)
	    if p1^.doCheck then
		newParams := AddToExprList(nil, CopyExpr(fileExpr));
		newParams := AddToExprList(newParams, 
		    MakeExprConst(cardinalTypeNode, fileEvalMode));
		newParams := AddToExprList(newParams, p);
	    else
		newParams := AddToExprList(nil, p);
	    end;
	    if constWidth then
		newParams := AddToExprList(newParams,
		    MakeExprConst(integerTypeNode, longfloat(width)));
	    else
		minParams := AddToExprList(nil, CopyExpr(p^.widthExpr));
		minParams := AddToExprList(minParams,
		    MakeExprConst(integerTypeNode, longfloat(defaultWidth)));
		newParams := AddToExprList(newParams,
		    MakeExprFunc(minProcNode, p, minParams, integerTypeNode));
	    end;
	    newParams := AddToExprList(newParams,
		MakeExprConst(integerTypeNode, 1.0));
	    newParams := AddToExprList(newParams, CopyExpr(cFileExpr));
	    if p1^.doCheck then
		AddCall(stn, writesProcNode, p, newParams);
	    else
		AddCall(stn, fwriteProcNode, p, newParams);
	    end;
    
	| 'd', 'o', 'x' :
	    (* WRITEF(f, f^.cFile, '%nd', [n, ] i) or
	       fprintf(f^.cFile, '%nd', [n, ] i) *)
	    if constWidth then
		Swritef(fs, '%%%d%c', width, fType);
		AddText(fs);
		AddChar(0C);
		AddWritefCall(NonHashString(), nil, nil, p);
	    else
		AddText('%*');
		AddChar(fType);
		AddChar(0C);
		AddWritefCall(NonHashString(), p^.widthExpr, nil, p);
	    end;
	    
	| 'n' :
	    (* WRITEF(f, f^.cFile, '%ns', [n, ] NAM(p, NameTable(p))) or
	       fprintf(f^.cFile, '%ns', [n, ] NAM(p, NameTable(p))) *)
	    namParams := AddToExprList(nil, p);
	    namParams := 
		AddToExprList(namParams, MakeExprVar(pt^.nameTable, p));
	    if constWidth then
		Swritef(fs, '%%%ds', width);
		AddText(fs);
		AddChar(0C);
		AddWritefCall(NonHashString(), nil, nil,
		    MakeExprFunc(namProcNode, p, namParams, addressTypeNode));
	     else
		AddText('%*s');
		AddChar(0C);
		AddWritefCall(NonHashString(), p^.widthExpr, nil,
		    MakeExprFunc(namProcNode, p, namParams, addressTypeNode));
	    end;
	end (* case *);
    end WidthFormat;

    procedure WidthDecimalFormat(p : ExprNode);
    (* Map a longreal write specification to WRITEF or fprintf *)
	var fs		: array [0..15] of char;
	    wtn		: TypeNode;
	    width       : integer;
	    maxParams   : ExprList;
    begin
    if p^.widthExpr = nil then
	AddText('%21.14e');
    else
        wtn := BaseType(CheckExpr(p^.widthExpr, EVALGET));
	if IsBadExpr(p^.widthExpr) or (wtn^.kind = DTANY) then
	elsif wtn^.kind # DTINTEGER then
	    ExprError(p, 
		'Procedure write: width specification must be type integer'); 
        elsif p^.widthExpr^.kind = EXPRCONST then
	    width := trunc(OrdOf(p^.widthExpr^.exprConst));
	    SWritef(fs, '%%%d', width);
	    AddText(fs);
	    p^.widthExpr := nil;
	else
	    AddText('%*');
	end;
	if p^.decimalExpr = nil then
	    if p^.widthExpr = nil then   (* Constant width *)
		if width < 8 then
		    AddText('.1e');
		else
		    SWritef(fs, '.%de', width - 7);
		    AddText(fs);
		end;
	    else			(* Variable width, run thru MAX *)
		AddText('.*e');
		maxParams := AddToExprList(nil, CopyExpr(p^.widthExpr));
		maxParams := AddToExprList(maxParams,
		    MakeExprConst(integerTypeNode, 7.0));
		maxParams := AddToExprList(maxParams,
		    MakeExprConst(integerTypeNode, 1.0));
		p^.decimalExpr := 
		    MakeExprFunc(maxProcNode, p, maxParams, addressTypeNode);
	    end;
	else
	    wtn := BaseType(CheckExpr(p^.decimalExpr, EVALGET));
	    if IsBadExpr(p^.decimalExpr) or (wtn^.kind = DTANY) then
	    elsif wtn^.kind # DTINTEGER then
		ExprError(p, 'write: decimal places must be an integer'); 
	    elsif p^.decimalExpr^.kind = EXPRCONST then
		SWritef(fs, '.%df', trunc(OrdOf(p^.decimalExpr^.exprConst)));
		AddText(fs);
		p^.decimalExpr := nil;
	    else
		AddText('.*f');
	    end;
	end;
    end;
    AddChar(0C);
    AddWritefCall(NonHashString(), p^.widthExpr, p^.decimalExpr, p);
    end WidthDecimalFormat;

begin (* CheckWriteParams *)
    MakeSimpleFileExpr(fileExpr, fileEvalMode);

    (* Create expr for UNIX file descriptor *)
    cFileExpr := MakeExprOffset(addressTypeNode, TKPLUS, fileExpr, FILEOFFSET);
    ValueOrAddr(cFileExpr, addressTypeNode, EVALGET);

    p := p1^.next;
    if p = nil then
	ExprError(p1, 'write(f) needs arguments');
        return;
    end;
    if firstArgEvaluated then
	pt := BaseType(p^.exprType);
	pnum := 1;
    else
	pt := BaseType(CheckExprWithSubExprs(p, EVALGET));
	pnum := 2;
    end;
    loop (* through all args *)
	pnext := p^.next;   (* p^.next gets smashed by AddToExprList *)
	if not IsBadExpr(p) then
	    case pt^.kind of
	    | DTANY : (* nothing *)

	    | DTINTEGER : 
		if p^.decimalExpr = nil then
		    WidthFormat(p, 10, 'd');
		elsif p^.decimalExpr^.kind = EXPROCT then
		    p^.decimalExpr := nil;
		    WidthFormat(p, 11, 'o');
		elsif p^.decimalExpr^.kind = EXPRHEX then
		    p^.decimalExpr := nil;
		    WidthFormat(p, 8, 'x');
		else
		    IntegerToReal(p);
		    WidthDecimalFormat(p);
		end;

	    | DTSTRING :
		WidthFormat(p, pt^.stringLength, 's');

	    | DTARRAY :
		(* Get rid of EXPRVAL node (if any), change exprType from
		   addressTypeNode to the array type (yucho) *)
		if p^.kind = EXPRVAL then
		    p^.exprVal^.widthExpr := p^.widthExpr;
		    p^.exprVal^.decimalExpr := p^.decimalExpr;
		    p^ := p^.exprVal^;
		end;
		if p^.kind = EXPRVAR then
		    p^.exprType := pt;
		end;
		pbt := BaseType(pt^.elementType);
		if not (pbt^.kind in DataTypeSet{DTANY, DTCHAR}) then
		    ExprError(p, 
			    'Procedure write: only array of char allowed'); 
		else
		    WidthFormat(p, trunc(NumberOf(pt^.indexType)), 's');
		end;

	    | DTLONGREAL :
		WidthDecimalFormat(p);

	    | DTCHAR :
		WidthFormat(p, 1, 'c');

	    | DTENUMERATION, DTBOOLEAN :
		if pt^.nameTable = nil then
		    AllocateEnumNameTable(pt);
		end;
		WidthFormat(p, 1, 'n');
		
	    else
		ExprErrorNameNumber(p, stringDataType[pt^.kind], 
		    'write(parameter #%): cannot write type $', pnum);
	    end (* case *);
	end (* if *);
	if pnext = nil then
	    exit;
	end;
	p := pnext;
	inc(pnum);
	pt := BaseType(CheckExprWithSubExprs(p, EVALGET));
     end (* loop *);
    if proc^.builtin in BuiltinProcTypeSet{BIPwriteln, BIPmessage} then
	if p1^.doCheck then
	    newParams := AddToExprList(nil, CopyExpr(fileExpr));
	    newParams := AddToExprList(newParams,
		MakeExprConst(integerTypeNode, fileEvalMode));
	    AddCall(stn, writelnProcNode, p1, newParams);
	else
	    newParams := AddToExprList(nil, 
		MakeExprConst(integerTypeNode, 10.0));
	    newParams := AddToExprList(newParams, CopyExpr(cFileExpr));
	    AddCall(stn, fputcProcNode, p1, newParams);
	end;
    end;
end CheckWriteParams;

$end (* pascal *)

var
    procNode   : ProcNode;
    unitParams : ExprList;

begin (* CheckBuiltin *)
    error := ERRNONE;
    saveTotalErrors := totalErrors;
    retType := nil;
    p1 := nil;
    p2 := nil;
    pt1 := nil;
    pt2 := nil;
    nump := 0;
    if (params # nil) then	(* count params *)
	p1 := params^.first;
        p := p1;
	while p # nil do
	    nump := nump + 1;
	    p := p^.next;
	end;
	if nump > 1 then
	    p2 := p1^.next;
	end;
$if pascal then
	(* Disallow e:m:n format specifier except in write, writeln *)
	if not (proc^.builtin in BuiltinProcTypeSet{BIPwrite, BIPwriteln}) then
	    p := p1;
	    while p # nil do
		if p^.decimalExpr # nil then
		    ExprErrorName(p, proc^.name,
		    'Procedure $: "e:m:n" parameters allowed only in write');
		elsif p^.widthExpr # nil then
		    ExprErrorName(p, proc^.name,
		    'Procedure $: "e:m" parameters allowed only in write');
		end;
		p := p^.next;
	    end (* while *);
	end (* if not write, writeln *);
$end
    end;

$if modula2 then
    if (proc^.builtin = BIPMIN) and (nump = 1) then
	bip := BIPFIRST;
    elsif (proc^.builtin = BIPMAX) and (nump = 1) then
	bip := BIPLAST;
    else
	bip := proc^.builtin;
    end;
$else (* pascal *)
    bip := proc^.builtin;
$end

    if bip in builtinFunctions then
	if stn # nil then
	    ErrorName(proc^.name,'Function $ used as a procedure');
	end;
    else
	if stn = nil then
	    ErrorName(proc^.name,'Procedure $ used as a function');
	end;
    end;
    if (nump < proc^.minParams) or (proc^.maxParams < nump) then
	error := ERRNUMP;
    end;

    if error # ERRNUMP then     (* No further checking if # params is wrong *)
	case bip of
	| BIPABS :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in DataTypeSet{DTINTEGER, DTREAL, DTLONGREAL})
	    then
$if modula2 then
		ExprError(p1, 'ABS(x): x must be INTEGER, REAL, or LONGREAL');
$else (* pascal *)
		ExprError(p1, 'abs(x): x must be integer or real');
$end
	    elsif p1^.kind = EXPRCONST then
		procValExpr^.kind := EXPRCONST;
		procValExpr^.exprConst^.kind := DTREAL;
		procValExpr^.exprConst^.realVal := abs(p1^.exprConst^.realVal);
		procValExpr^.constType := realConstTypeNode;
		procValExpr^.exprType := realConstTypeNode;
	    end;
	    retType := pt1;
    
	| BIPASSERT :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTBOOLEAN then
		ExprError(p1, 'assert(b, s): b must be boolean');
	    else
		if nump = 1 then
		    AddChar(0C);
		    p2 := MakeExprConstString(NonHashString());
		    params := AddToExprList(params,p2);
		else
		    pt2 := CheckExpr(p2,EVALPOINT);
		    if IsBadExpr(p2) or (pt2^.kind = DTANY) then
		    elsif not Passable(arrayOfCharTypeNode,
			    PARAMARRAYCONST,pt2,p2) then
$if modula2 then
			ExprError(p2, 'ASSERT(b, s): s must be ARRAY OF CHAR');
$else (* pascal *)
			ExprError(p2, 'assert(b, s): s must be array of char');
$end
			nump := 1;
		    else
			RefOpenArray(p2,pt2);
		    end;
		end;
	    end;
    
	| BIPCHR :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in 
			DataTypeSet{DTCARDINAL,DTINTEGER,DTWORD,DTBYTE}) then
		ExprError(p1,
$if modula2 then
		    'CHR(x): x must be INTEGER, UNSIGNED, WORD, or BYTE');
$else (* pascal *)
		    'chr(x): x must be integer');
$end
	    else
		if p1^.kind = EXPRCONST then
		    value := OrdOf(p1^.exprConst);
		    if (value < 0.0) or (value > MAXCHAR) then
			ErrorNumber('CHR(x): x out of range [0..%]', 
			    trunc(MAXCHAR));
		    end;
		    procValExpr^.kind := EXPRCONST;
		    new(cn);
		    cn^.kind := DTCHAR;
		    cn^.charVal := chr(trunc(value));
		    procValExpr^.exprConst := cn;
		    procValExpr^.exprType := charTypeNode;
		    procValExpr^.constType := charTypeNode;
(* |||		else
		    params^.first := MakeExprCheck(p1, CHECKRANGE, nil, nil, 
			    pt1, 0, trunc(MAXCHAR), nil);
		    params^.last := params^.first;
*)
		end;
		retType := charTypeNode;
	    end;
 
	| BIPDISPOSE :
	    (* parameter list is address and possible tags *)
	    (* build parameter list that is addr, check, proc, count, size *)
	    (* for dynamic array, size is element size *)
	    pt1 := BaseType(CheckExpr(p1, EVALPUT));
	    CheckVarParam(p1, pt1, 1);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind = DTPOINTER then
		(* get size *)
		atn := ActualType(pt1^.toType);
		ptrKind := pt1^.ptrKind;
		if nump = 1 then
		    size := (atn^.size + UNITSIZE-1) div UNITSIZE;
		elsif atn^.kind = DTRECORD then
		    size := (RecordTypeSize(atn, p2, proc^.name) + UNITSIZE-1) 
				    div UNITSIZE;
		else
		    ExprError(p1,
			'Variant specifications allowed on records only');
                end;

		numDimensions := 0.0;
$if modula2 then
	    elsif pt1^.kind = DTDYNARRAY then
		(* Save type for code generation *)
		p1^.exprType := pt1;
		ptrKind := pt1^.dynArrayKind;
		numDimensions := 0.0;
		atn := pt1^.dynArrayType;
		counten := p2;
		pnum := 2;
		while (atn^.kind = DTARRAY) and
		    (atn^.arrayKind in ArrayKindSet{ARRAYOPEN,ARRAYNOCOUNT}) do
		    if atn^.arrayKind = ARRAYNOCOUNT then
			(* Better be a size specified *)
			if counten = nil then
			    ExprError(p1, 'DISPOSE(dynarray): too few sizes');
			else
			    counttn := CheckExpr(counten, EVALGET);
			    if IsBadExpr(counten) or
				   (Assignable(cardIntTypeNode,counttn,counten)
				    = nil) then
				ExprErrorNumber(p1,
				  'DISPOSE(dynarray): bad size, parameter # %',
				    pnum);
			    end;
			    counten := counten^.next;
			    inc(pnum);
			end;
		    end (* if ARRAYNOCOUNT *);
		    size := atn^.elementSize;
		    atn := atn^.elementType;
		    numDimensions := numDimensions + 1.0;
		end (* while *);
		if counten # nil then
		    ExprErrorNumber(p1,
			'DISPOSE(dynarray): too many sizes, parameter # %', 
			pnum);
		end;
		size := size div UNITSIZE;
$end (* modula2 *)
	    else
$if modula2 then
		ExprError(p1, 'DISPOSE(p): p must be POINTER or DYNARRAY');
$else (* pascal *)
		ExprError(p1, 'dispose(p): p must be pointer');
$end
	    end;
	    if (error # ERRNONE) or (totalErrors # saveTotalErrors) then
	    elsif ptrKind = PTRLOCAL then
		ExprError(p1,'DISPOSE does not work on @local pointer');
	    else
		(* Detach size specifications *)
		p1^.next := nil;
		params^.last := p1;
		(* add check as second parameter *)
		conen := MakeExprConst(cardIntTypeNode,longfloat(ord(ptrKind)));
		params := AddToExprList(params,conen);
$if modula2 then
		(* add procedure as third parameter *)
		conen := UserOrBuiltin(deallocateProc,deallocateString,p1);
		procNode := conen^.exprConst^.procVal;
$else (* pascal *)
		if atn^.containsFiles then      (* Any files being disposed? *)
		    conen := MakeExprConst(integerTypeNode,1.0);
		else
		    conen := MakeExprConst(integerTypeNode,0.0);
		end;
$end
		params := AddToExprList(params,conen);
		(* add number of dimensions as fourth parameter *)
		conen := MakeExprConst(cardIntTypeNode,numDimensions);
		params := AddToExprList(params,conen);
		(* add size as fifth parameter *)
		conen := MakeExprConst(cardIntTypeNode, longfloat(size));
		params := AddToExprList(params,conen);
$if modula2 then
		(* add sizes to list *)
		if numDimensions > 0.0 then
		    p := p2;
		    while p # nil do
			pnext := p^.next;
			params := AddToExprList(params,p);
			p := pnext;
		    end;
		end;
$end
	    end;
    
	| BIPNEW
$if modula2 then
	    , BIPLOCAL
$end
		    :
	    (* Parameter list is address followed by possible tags (record *)
	    (* with variants) or sizes (dynamic array).			   *)
	    (* Build a parameter list address, check, proc, count, size,   *)
	    (* sizes; where count is the number of dimensions (if any),    *)
	    (* size is the record or element size, sizes is a variable     *)
	    (* number of sizes (none for anything but dynarrays)	   *)
	    pt1 := BaseType(CheckExpr(p1, EVALPUT));
	    CheckVarParam(p1, pt1, 1);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind = DTPOINTER then
		(* get size *)
		atn := ActualType(pt1^.toType);
		ptrKind := pt1^.ptrKind;
		if nump = 1 then
		    size := (atn^.size + UNITSIZE-1) div UNITSIZE;
		elsif atn^.kind = DTRECORD then
		    size := (RecordTypeSize(atn, p2, proc^.name) + UNITSIZE-1)
		    		div UNITSIZE;
		else
		    ExprError(p1,
		      'Variant specifications allowed on records only');
                end;
		numDimensions := 0.0;
$if modula2 then
	    elsif pt1^.kind = DTDYNARRAY then
		(* Save type for code generation *)
		p1^.exprType := pt1;
		ptrKind := pt1^.dynArrayKind;
		numDimensions := 0.0;
		atn := pt1^.dynArrayType;
		counten := p2;
		pnum := 2;
		while (counten # nil) do
		    counttn := CheckExpr(counten,EVALGET);
		    if IsBadExpr(counten) or 
			(Assignable(cardIntTypeNode,counttn,counten)=nil) then
			ExprErrorNumber(p1,
			    'NEW(dynarray): bad size, parameter # %', pnum);
		    end;
		    if (atn^.kind = DTARRAY) and (atn^.arrayKind in 
			    ArrayKindSet{ARRAYNOCOUNT,ARRAYOPEN}) then
			size := atn^.elementSize;
			atn := atn^.elementType;
 			numDimensions := numDimensions + 1.0;
		    else
			ExprErrorNumber(p1,
			  'NEW(dynarray): too many sizes, parameter # %',pnum);
		    end;
		    counten := counten^.next;
		    inc(pnum);
		end;
		if totalErrors = saveTotalErrors then
		    if (atn^.kind = DTARRAY) and (atn^.arrayKind in 
			    ArrayKindSet{ARRAYNOCOUNT,ARRAYOPEN}) then
			ExprError(p1, 'NEW(dynarray): too few sizes');
		    else
			size := size div UNITSIZE;
		    end;
		end;
$end (* modula2 *)
	    else
$if modula2 then
		ExprErrorName(p1, proc^.name,
		    '$(p): p must be POINTER or DYNARRAY');
$else (* pascal *)
		ExprError(p1, 'new(p): p must be pointer');
$end
	    end;
	    if (error # ERRNONE) or (totalErrors # saveTotalErrors) then
$if modula2 then
	    elsif (ptrKind # PTRLOCAL) and (bip = BIPLOCAL) then
		ExprError(p1,'LOCAL operation requires @local pointer');
	    elsif (ptrKind = PTRLOCAL) and (bip # BIPLOCAL) then
		ExprError(p1,'NEW does not work on @local pointer');
$end
	    else
		(* detach the sizes from the list *)
		p1^.next := nil;
		params^.last := p1;
		(* add check as second parameter *)
		conen := MakeExprConst(cardIntTypeNode,longfloat(ord(ptrKind)));
		params := AddToExprList(params,conen);
$if modula2 then
		(* add proc as third parameter *)
		conen := UserOrBuiltin(allocateProc,allocateString,p1);
		procNode := conen^.exprConst^.procVal;
		params := AddToExprList(params,conen);
$end
		(* add number of dimensions as fourth parameter *)
		conen := MakeExprConst(cardIntTypeNode,numDimensions);
		params := AddToExprList(params,conen);
		(* add size as fifth parameter *)
		conen := MakeExprConst(cardIntTypeNode, longfloat(size));
		params := AddToExprList(params,conen);
$if modula2 then
		(* add sizes to list *)
		if numDimensions > 0.0 then
		    p := p2;
		    while p # nil do
			pnext := p^.next;
			params := AddToExprList(params,p);
			p := pnext;
		    end;
		end;
$end
	    end;
    
	| BIPODD :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
$if modula2 then
		ExprError(p1, 'ODD(x): x must be INTEGER or UNSIGNED');
$else (* pascal *)
		ExprError(p1, 'odd(x): x must be integer');
$end
	    else
		retType := booleanTypeNode;
	    end;
    
	| BIPORD :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in (indexableTypes+DataTypeSet{DTPOINTER}))
	    then
$if modula2 then
		ExprError(p1,
		    'ORD(x): x must be discrete scalar or POINTER');
$else (* pascal *)
		ExprError(p1,
		    'ord(x): x must be discrete scalar or pointer');
$end
	    else
		if p1^.kind = EXPRCONST then
		    value := OrdOf(p1^.exprConst);
		    procValExpr^.kind := EXPRCONST;
		    procValExpr^.exprConst := CardinalConst(value);
		    if value < 0.0 then
			retType := integerTypeNode;
		    else
			retType := cardIntTypeNode;
		    end;
		    procValExpr^.exprType := retType;
		    procValExpr^.constType := retType;
		else (* not a constant *)
		    retType := cardIntTypeNode;
		end;
	    end;
	
	| BIPTRUNC :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in DataTypeSet{DTREAL,DTLONGREAL}) then
$if modula2 then
		ExprError(p1, 'TRUNC(x): x must be REAL or LONGREAL');
$else (* pascal *)
		ExprError(p1, 'trunc(x): x must be real');
$end
	    else
		if p1^.kind = EXPRCONST then
		    value := p1^.exprConst^.realVal;
		    if (value >= MAXINT + 1.0) or (value <= MININT - 1.0) then
			ExprError(p1, 
				'Truncation would exceed integer limits');
			value := 0.0;
		    else 
			value := longfloat(trunc(value));
		    end;
		    procValExpr^.kind := EXPRCONST;
		    procValExpr^.exprConst := CardinalConst(value);
		    procValExpr^.exprConst^.kind := DTINTEGER;
		    procValExpr^.exprType := integerTypeNode;
		    procValExpr^.constType := integerTypeNode;
		end;
		retType := integerTypeNode;
	    end;
    
		    (* CED - 8/13/87 *)
	| BIPXFC :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTINTEGER then
		ExprError(p1, 'XFC(x): x must be INTEGER');
	    else
		retType := integerTypeNode;
	    end;
    
$if modula2 then
	| BIPMIN, BIPMAX :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    else
		retType := Compatible(pt1,p1, pt2,p2);
		if retType = nil then
		    ErrorName(proc^.name,'Parameters of $ are not compatible');
		elsif not (retType^.kind in DataTypeSet{DTCARDINAL, DTINTEGER, 
					DTREAL, DTLONGREAL}) then
		    ExprErrorName(p1, proc^.name,
			'$(x, y): x and y must be numbers');
		end;
	    end;
    
	| BIPCAP :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTCHAR then
		ExprError(p1, 'CAP(c): c must be CHAR');
	    else
		retType := charTypeNode;
	    end;
    
	| BIPDEC, BIPINC :
	    pt1 := CheckExpr(p1, EVALPUT);
	    if not IsAddressableExpr(p1) then
		ExprErrorName(p1, proc^.name,
		    'Procedure $, parameter # 1 must be a variable');
	    end;
	    (* Non-aligned types get special code generated in BuiltinPC *)
	    pt1b := BaseType(pt1);
	    if IsBadExpr(p1) or (pt1b^.kind = DTANY) then
	    elsif not ((pt1b^.kind in indexableTypes) or 
		    (pt1b = addressTypeNode)) then
		ExprErrorName(p1, proc^.name,
		    '$(x): x must be discrete scalar or address');
	    elsif nump = 1 then
		p2 := MakeExprConst(cardIntTypeNode, 1.0);
		params := AddToExprList(params,p2);
	    else
		pt2 := BaseType(CheckExpr(p2,EVALGET));
		if IsBadExpr(p2) or (pt2^.kind = DTANY) then
		elsif not (pt2^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
		    ExprErrorName(p1, proc^.name,
			'$(x, i): i must be INTEGER or UNSIGNED');
		end;
	    end;
	    if (pt1b^.kind = DTENUMERATION) and (not pt1b^.enumContiguous) then
		ExprErrorName(p1, proc^.name,
		    '$(e): e must be a contiguous enumeration');
	    end;
	    p3 := MakeExprConst(pt1,0.0);	(* put var type on param list *)
	    params := AddToExprList(params,p3);
    
	| BIPALLOCATE, BIPDEALLOCATE:
	    pt1 := BaseType(CheckExpr(p1, EVALPUT));
	    CheckVarParam(p1, pt1, 1);
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1^.kind # DTPOINTER then
		ExprErrorName(p1, proc^.name, '$(p, i): p must be POINTER');
	    elsif not Passable(cardinalTypeNode,PARAMCONST,pt2,p2) then
		ExprErrorName(p1, proc^.name,
		    '$(p, i): i must be INTEGER or UNSIGNED');
	    end;
    
	| BIPEXCL, BIPINCL :
	    pt1 := CheckExpr(p1, EVALPUT);
	    CheckVarParam(p1, pt1, 1);
	    pt1b := BaseType(pt1);
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1b^.kind # DTSET then
		ExprErrorName(p1, proc^.name, '$(s, i): s must be SET');
	    elsif not Passable(pt1b^.setRange,PARAMCONST,pt2,p2) then
		ExprErrorName(p1, proc^.name,
		    '$(s, i): i must be compatible with SET base type');
	    else
		lowerBound := LowerBoundOf(pt1b^.setRange);
		if lowerBound # 0.0 then
		    (* replace p2 with (p2 - lowerBound) *)
		    p2newen := MakeExprOffset(pt2, TKMINUS, p2, lowerBound);
		    (* replace p2 (last in list) with p2newen *)
		    p1^.next := p2newen;
		    params^.last := p2newen;
		    p2newen^.next := nil;
		end;
		p3 := MakeExprConst(pt1,0.0); (* put set type on param list *)
		params := AddToExprList(params,p3);
	    end;
    
	| BIPFLOAT :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in 
		    DataTypeSet{DTINTEGER, DTCARDINAL, DTLONGREAL}) then
		ExprError(p1,
		    'FLOAT(x): x must be INTEGER, UNSIGNED, or LONGREAL');
	    elsif p1^.kind = EXPRCONST then
		procValExpr^.kind := EXPRCONST;
		procValExpr^.exprConst^.kind := DTREAL;
		if pt1^.kind = DTLONGREAL then
		    procValExpr^.exprConst^.realVal := p1^.exprConst^.realVal;
		else
		    procValExpr^.exprConst^.realVal := p1^.exprConst^.cardVal;
		end;
		procValExpr^.constType := realTypeNode;
		procValExpr^.exprType := realTypeNode;
	    elsif pt1^.kind = DTCARDINAL then
		params^.first := MakeExprCheck(p1, CHECKRANGE, nil, nil,
		    pt1, 0, trunc(MAXINT), nil);
		params^.last := params^.first;
	    end;
	    retType := realTypeNode;

	| BIPLONGFLOAT :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in 
		    DataTypeSet{DTINTEGER, DTCARDINAL, DTREAL}) then
		ExprError(p1,
		    'LONGFLOAT(x): x must be INTEGER, UNSIGNED, or REAL');
	    elsif p1^.kind = EXPRCONST then
		procValExpr^.kind := EXPRCONST;
		procValExpr^.exprConst^.kind := DTLONGREAL;
		if pt1^.kind = DTREAL then
		    procValExpr^.exprConst^.realVal := p1^.exprConst^.realVal;
		else
		    procValExpr^.exprConst^.realVal := p1^.exprConst^.cardVal;
		end;
		procValExpr^.constType := longrealTypeNode;
		procValExpr^.exprType := longrealTypeNode;
	    elsif pt1^.kind = DTCARDINAL then
		params^.first := MakeExprCheck(p1, CHECKRANGE, nil, nil,
		    pt1, 0, trunc(MAXINT), nil);
		params^.last := params^.first;
	    end;
	    retType := longrealTypeNode;
		
	| BIPHALT :
	    if nump = 0 then
                conen := MakeExprConst(cardIntTypeNode, DefaultExitStatus);
                params := AddToExprList(params,conen);
	    else
		pt1 := BaseType(CheckExpr(p1,EVALGET));
		if IsBadExpr(p1) or (pt1^.kind = DTANY) then
		elsif not(pt1^.kind in DataTypeSet{DTINTEGER, DTCARDINAL}) then
		    ExprError(p1, 'HALT(x): x must be INTEGER or UNSIGNED');
		end;
	    end;
	    retType := nil;
    
	| BIPDESCRIPTOR :
	    pt1 := ElementType(CheckExprType(p1, EVALGET));
	    pt2 := BaseType(CheckExpr(p2, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif (p1^.kind # EXPRSYM) or (p1^.exprSym^.kind # SYMTYPE) then
		ExprError(p1, 'DESCRIPTOR(t, p): t must be a type identifier');
	    elsif pt2^.kind = DTPOINTER then
		procValExpr^.kind := EXPRDESCRIPTOR;
		procValExpr^.descripBase := p2;
		procValExpr^.descripMode := EVALGET;
		procValExpr^.descripCount := 0;
		procValExpr^.descrips := nil;
		if p2^.kind = EXPRCONST then (* NIL *)
		    if nump > 2 then
		        ExprError(p1,'DESCRIPTOR(t, NIL): too many parameters');
		    end;
		    (* Leave descripCount = 0 so know special NIL case *)
		else (* address variable *)
		    p := p2^.next;
		    while p # nil do
			pnext := p^.next;
			pt := BaseType(CheckExpr(p, EVALGET));
			if IsBadExpr(p) or (pt^.kind = DTANY) then
			else
			    if not (pt^.kind in
				    DataTypeSet{DTINTEGER,DTCARDINAL}) then
				ExprErrorNumber(p1, 
				    'DESCRIPTOR: parameter # % must be number',
				    procValExpr^.descripCount + 3);
			    end;
			    inc(procValExpr^.descripCount);
			    procValExpr^.descrips := AddToExprList(
				    procValExpr^.descrips, p);
			end;
			p := pnext;
		    end (* while *);
		end;
	    else
		ExprError(p1, 'DESCRIPTOR(t, p): p must be a pointer');
	    end;
	    retType := NewTypeNode(DTDESCRIPTOR);
	    retType^.descripType := pt1;

	    
	| BIPHIGH, BIPLOW, BIPNUMBER :
	    pt1 := BaseType(CheckExpr(p1,EVALPOINT));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind = DTDYNARRAY then
		(* accept non-dereferenced dynamic array *)
		pt1 := BaseType(pt1^.dynArrayType);
	    end;
	    if totalErrors # saveTotalErrors then
		(* error already *)
	    elsif pt1^.kind # DTARRAY then
		ExprErrorName(p1, proc^.name, '$(a): a must be ARRAY');
	    else
		subnum := 1;
		if nump = 2 then
		    pt2 := BaseType(CheckExpr(p2, EVALGET));
		    if p2^.kind # EXPRCONST then
			ExprError(p2, 'Dimension number must be constant');
		    elsif not (pt2^.kind in 
			    DataTypeSet{DTINTEGER, DTCARDINAL}) then
			ExprErrorName(p1, proc^.name,
			    '$(a, d): dimension d must be INTEGER or UNSIGNED');
		    else
			subnum := trunc(OrdOf(p2^.exprConst));
			if subnum <= 0 then
			    ExprErrorNameNumber(p1, proc^.name,
				'$(a, %): dimension must be positive', subnum);
			end;
		    end;
		end;
		atn := pt1;
		fi := 1;
		while (fi < subnum) do
		    fi := fi + 1;
		    atn := BaseType(atn^.elementType);
		    if atn^.kind # DTARRAY then
			ExprErrorNameNumber(p1, proc^.name, 
			   "$(a, %): array a doesn't have that many dimensions",
			    subnum);
			exit while;
		    end;
		end;
		if totalErrors # saveTotalErrors then
		elsif atn^.arrayKind = ARRAYNORMAL then
		    (* constant: evaluate now *)
		    if bip = BIPHIGH then
			value := UpperBoundOf(atn^.indexType);
		    elsif bip = BIPLOW then
			value := LowerBoundOf(atn^.indexType);
		    else
			value := NumberOf(atn^.indexType);
		    end;
		    procValExpr^.kind := EXPRCONST;
		    procValExpr^.exprConst := CardinalConst(value);
		    if value >= 0.0 then
			retType := cardIntTypeNode;
		    else
			retType := integerTypeNode;
		    end;
		    procValExpr^.exprType := retType;
		    procValExpr^.constType := retType;
		elsif bip = BIPLOW then
		    (* low of open/dynamic array is always 0 *)
		    conen := MakeExprConst(cardIntTypeNode,0.0);
		    SameExprLine(conen, procValExpr);
		    AssignExprNode(procValExpr,conen);
		    retType := cardIntTypeNode;
		elsif atn^.arrayKind = ARRAYNOCOUNT then
		    ExprErrorName(procValExpr, proc^.name,
			'Cannot take $ of NOCOUNT array');
		else
		    (* make a reference to the descriptor *)
		    if pt1^.arrayKind = ARRAYSUBARRAY then
			value := 
			    longfloat(WORDSIZE + (subnum-1) * 2 * WORDSIZE);
		    else
			value := longfloat(WORDSIZE + (subnum-1) * WORDSIZE);
		    end;
		    adden := MakeExprOffset(addressTypeNode, TKPLUS, p1, value);
		    counten := MakeExprVal(cardIntTypeNode,adden);
		    if bip = BIPHIGH then
			counten :=
			    MakeExprOffset(cardIntTypeNode,TKMINUS,counten,1.0);
		    end;
		    AssignExprNode(procValExpr,counten);
		    retType := cardIntTypeNode;
		end;
	    end;
    
	| BIPVAL :
	    pt1 := BaseType(CheckExprType(p1,EVALGET));
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif (p1^.kind # EXPRSYM) or (p1^.exprSym^.kind # SYMTYPE) then
		ExprError(p1, 'VAL(t, x): t must be a type identifier');
	    elsif not (pt1^.kind in indexableTypes) then
		ExprError(p1, 'VAL(t, x): t must be a discrete scalar type');
	    elsif not (pt2^.kind in DataTypeSet{DTCARDINAL,DTINTEGER}) then
		ExprError(p1, 'VAL(t, x): x must be INTEGER or UNSIGNED');
	    else
(* |||		p1^.next := MakeExprCheck(p2, CHECKRANGE, nil, nil, pt2, 
			trunc(LowerBoundOf(pt1)), trunc(UpperBoundOf(pt1)), 
			nil);
		params^.last := p1^.next;
*)
		retType := pt1;
	    end;
    
	| BIPADR :
	    (* ||| EVALPOINT permits adr(constparam) *)
	    pt1 := CheckExpr(p1, EVALPOINT);
	    CheckVarParam(p1, pt1, 1);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    else
		RefOpenArray(p1,pt1);
		retType := addressTypeNode;
	    end;
    
	| BIPBITSIZE, BIPBYTESIZE, BIPWORDSIZE :
	    pt1 := CheckExpr(p1,EVALPOINT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif (pt1^.kind = DTARRAY) and (pt1^.arrayKind # ARRAYNORMAL) then
		if pt1^.arrayKind = ARRAYSUBARRAY then
		    ExprErrorName(procValExpr,proc^.name,
			'Cannot use $ on subarrays');
		elsif pt1^.arrayKind = ARRAYNOCOUNT then
		    ExprErrorName(procValExpr,proc^.name,
			'Cannot use $ on nocount open arrays');
		else (* ARRAYOPEN *)
		    offset := WORDSIZE;
		    atn := pt1;
		    sizeen := nil;
		    (* Multiply number of elements in each dimension *)
		    repeat
			adden := MakeExprOffset(addressTypeNode, TKPLUS,
			    CopyExpr(p1), longfloat(offset));
			counten := MakeExprVal(cardIntTypeNode, adden);
			sizeen := MakeExprBinChain(cardIntTypeNode, TKASTERISK,
			    sizeen, counten);
			inc(offset, WORDSIZE);
			size := atn^.elementSize;
			atn := atn^.elementType;
		    until (atn^.kind # DTARRAY) or (atn^.arrayKind # ARRAYOPEN);
		    
		    (* Multiply by element size *)
		    if bip = BIPBITSIZE then
			sizeen := ScaleBySize(sizeen, size, 1);
		    elsif bip = BIPBYTESIZE then
			sizeen := ScaleBySize(sizeen, size, BYTESIZE);
		    else (* BIPWORDSIZE *)
			sizeen := ScaleBySize(sizeen, size, WORDSIZE);
		    end;

		    (* And return this expression as the function value *)
		    AssignExprNode(procValExpr, sizeen);
		end;
	    else
		(* constant: evaluate now *)
		size := SizeOf(pt1);
		if bip = BIPBYTESIZE then
		    size := (size+BYTESIZE-1) div BYTESIZE;
		elsif bip = BIPWORDSIZE then
		    size := (size+WORDSIZE-1) div WORDSIZE;
		end;
		procValExpr^.kind := EXPRCONST;
		procValExpr^.exprConst := CardinalConst(longfloat(size));
		procValExpr^.exprType := cardIntTypeNode;
		procValExpr^.constType := cardIntTypeNode;
	    end;
	    retType := cardIntTypeNode;
    
	| BIPFIRST, BIPLAST :
	    pt1 := ActualType(CheckExprType(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif (p1^.kind # EXPRSYM) or (p1^.exprSym^.kind # SYMTYPE) then
		ExprErrorName(p1, proc^.name,
		    '$(t): t must be a type identifier');
	    else
		new(cn);
		cn^.kind := pt1^.kind;
		case pt1^.kind of
		| DTREAL :
		    if bip = BIPFIRST then
			cn^.realVal := minreal;
		    else
			cn^.realVal := maxreal;
		    end;
		| DTLONGREAL :
		    if bip = BIPFIRST then
			cn^.realVal := minlongreal;
		    else
			cn^.realVal := maxlongreal;
		    end;
		| DTCHAR, DTBOOLEAN, DTENUMERATION, DTSUBRANGE, DTINTEGER,
		  DTCARDINAL :
		    if bip = BIPFIRST then
			value := LowerBoundOf(pt1);
		    else
			value := UpperBoundOf(pt1);
		    end;
		    if pt1^.kind = DTCHAR then
			cn^.charVal := chr(trunc(value));
		    elsif pt1^.kind = DTBOOLEAN then
			cn^.boolVal := (value = 1.0);
		    else
			(* enumerations and integers *)
			cn^.kind := DTINTEGER;
			cn^.cardVal := value;
		    end;
		| else
		    ExprErrorName(p1, proc^.name, 
			'$(t): t must be a scalar type');
		end;
		if (error = ERRNONE) and (totalErrors = saveTotalErrors) then
		    retType := pt1;
		    procValExpr^.kind := EXPRCONST;
		    procValExpr^.exprConst := cn;
		    procValExpr^.exprType := retType;
		    procValExpr^.constType := retType;
		end;
	    end;
    
	| BIPTBITSIZE, BIPTBYTESIZE, BIPTWORDSIZE :
	    pt1 := CheckExprType(p1,EVALGET);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif (p1^.kind # EXPRSYM) or (p1^.exprSym^.kind # SYMTYPE) then
		ExprErrorName(p1, proc^.name,
		    '$(t): t must be a type identifier');
	    else
		pt1 := p1^.exprSym^.symType;
		if (pt1^.kind = DTARRAY) and (pt1^.arrayKind # ARRAYNORMAL) then
		    ExprErrorName(procValExpr, proc^.name,
			'Cannot use $ on open arrays');
		end;
		(* constant: evaluate now *)
		atn := ActualType(pt1);
		if nump = 1 then
		    size := SizeOf(ElementType(atn));
		elsif atn^.kind = DTRECORD then
		    size := RecordTypeSize(atn, p2, proc^.name);
		else
		    ExprError(p1,
                    'Variant specifications allowed on non-sized records only');
                end;
		if bip = BIPTBYTESIZE then
		    size := (size+BYTESIZE-1) div BYTESIZE;
		elsif bip = BIPTWORDSIZE then
		    size := (size+WORDSIZE-1) div WORDSIZE;
		end;
		procValExpr^.kind := EXPRCONST;
		procValExpr^.exprConst := CardinalConst(longfloat(size));
		procValExpr^.exprType := cardIntTypeNode;
		procValExpr^.constType := cardIntTypeNode;
		retType := cardIntTypeNode;
	    end;
    
	| BIPCPUTIME :
	    retType := cardIntTypeNode;
    
	| BIPWRITEF :
	    pt1 := CheckExpr(p1,EVALGET);
	    pt2 := CheckExpr(p2,EVALPOINT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'WriteF(f, fs, ...): f must be FILE');
	    elsif p2^.kind # EXPRCONST then
		ExprError(p1,
		    'WriteF(f, fs, ...): fs must be constant ARRAY OF CHAR');
	    elsif p2^.exprConst^.kind = DTCHAR then
		(* one character string is OK *)
		if p2^.next # nil then
		    error := ERRNUMP;
		end;
	    elsif p2^.exprConst^.kind # DTSTRING then
		ExprError(p1,
		    'WriteF(f, fs, ...): fs must be constant ARRAY OF CHAR');
	    else
		(* need to update format string *)
		new(cn);
		cn^.kind := DTSTRING;
		CheckWritef(p2,p2^.exprConst^.strVal,p2^.next, cn^.strVal);
		p2^.exprConst := cn;
	    end;
    
	| BIPREADF :
	    pt1 := CheckExpr(p1,EVALGET);
	    pt2 := CheckExpr(p2,EVALPOINT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'ReadF(f, fs, ...): f must be FILE');
	    elsif p2^.kind # EXPRCONST then
		ExprError(p1,
		    'ReadF(f, fs, ...): fs must be constant ARRAY OF CHAR');
	    elsif p2^.exprConst^.kind = DTCHAR then
		(* one character string is OK *)
		if p2^.next # nil then
		    error := ERRNUMP;
		end;
	    elsif p2^.exprConst^.kind # DTSTRING then
		ExprError(p1,
		    'ReadF(f, fs, ...): fs must be constant ARRAY OF CHAR');
	    else
		CheckReadf(p2,p2^.exprConst^.strVal,p2^.next);
	    end;
	    retType := integerTypeNode;
    
	| BIPSWRITEF :
	    pt1 := CheckExpr(p1,EVALPUT);
	    pt2 := CheckExpr(p2,EVALPOINT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif not Passable(arrayOfCharTypeNode,PARAMARRAYVAR,pt1,p1) then
		ExprError(p1, 'SWriteF(s, fs, ...): s must be ARRAY OF CHAR');
	    elsif p2^.kind # EXPRCONST then
		ExprError(p1,
		    'SWriteF(s, fs, ...): fs must be constant ARRAY OF CHAR');
	    elsif p2^.exprConst^.kind = DTCHAR then
		(* one character string is OK *)
		if p2^.next # nil then
		    error := ERRNUMP;
		end;
	    elsif p2^.exprConst^.kind # DTSTRING then
		ExprError(p1,
		    'WriteF(s, fs, ...): fs must be constant ARRAY OF CHAR');
	    else
		RefOpenArray(p1,pt1);
		(* need to update format string *)
		new(cn);
		cn^.kind := DTSTRING;
		CheckWritef(p2,p2^.exprConst^.strVal,p2^.next, cn^.strVal);
		p2^.exprConst := cn;
	    end;
    
	| BIPSREADF :
	    pt1 := CheckExpr(p1,EVALPOINT);
	    pt2 := CheckExpr(p2,EVALPOINT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif not Passable(arrayOfCharTypeNode,PARAMARRAYCONST,pt1,p1) then
		ExprError(p1, 'SReadF(s, fs, ...): s must be ARRAY OF CHAR');
	    elsif p2^.kind # EXPRCONST then
		ExprError(p1,
		    'SReadF(s, fs, ...): fs must be constant ARRAY OF CHAR');
	    elsif p2^.exprConst^.kind = DTCHAR then
		(* one character string is OK *)
		if p2^.next # nil then
		    error := ERRNUMP;
		end;
	    elsif p2^.exprConst^.kind # DTSTRING then
		ExprError(p1,
		    'SReadF(s, fs, ...): fs must be constant ARRAY OF CHAR');
	    else
		RefOpenArray(p1,pt1);
		CheckReadf(p2,p2^.exprConst^.strVal,p2^.next);
	    end;
	    retType := integerTypeNode;
    
	| BIPWRITES :
	    pt1 := CheckExpr(p1, EVALGET);
	    pt2 := CheckExpr(p2, EVALPOINT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'WriteS(f, s): f must be FILE');
	    elsif not Passable(arrayOfCharTypeNode, PARAMARRAYCONST, pt2, p2) 
	    then
		ExprError(p1, 'WriteS(f, s): s must be ARRAY OF CHAR');
	    else
		RefOpenArray(p2, pt2);
	    end;

	| BIPWRITEC :
	    pt1 := CheckExpr(p1,EVALGET);
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'WriteC(f, c): f must be FILE');
	    elsif pt2^.kind # DTCHAR then
		ExprError(p1, 'WriteC(f, c): c must be CHAR');
	    end;
    
	| BIPREADC :
	    pt1 := CheckExpr(p1,EVALGET);
	    pt2 := CheckExpr(p2, EVALPUT);
	    conen := MakeExprConst(cardIntTypeNode, longfloat(SizeOf(pt2)));
	    params := AddToExprList(params,conen);
	    pt2 := BaseType(pt2);
	    CheckVarParam(p2, pt2, 2);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'ReadC(f, c): f must be FILE');
	    elsif pt2^.kind # DTCHAR then
		ExprError(p1, 'ReadC(f, c): c must be CHAR');
	    else
		retType := integerTypeNode;
	    end;
    
	| BIPWRITEB :
	    pt1 := CheckExpr(p1,EVALGET);
	    pt2 := CheckExpr(p2, EVALPOINT);
	    CheckVarParam(p2, pt2, 2);
	    p3 := p2^.next;
	    pt3 := BaseType(CheckExpr(p3,EVALGET));
	    if IsBadExpr(p1) or IsBadExpr(p2) or IsBadExpr(p3) 
		or (pt1^.kind = DTANY) or (pt2^.kind = DTANY) 
		or (pt3^.kind = DTANY) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'WriteB(f, b, i): f must be FILE');
	    elsif not (pt3^.kind in DataTypeSet{DTINTEGER,DTCARDINAL}) then
		ExprError(p1, 'WriteB(f, b, i): i must be INTEGER or UNSIGNED');
	    else
		PointerOrAddress(p2,pt2);
	    end;
    
	| BIPREADB :
	    pt1 := CheckExpr(p1,EVALGET);
	    pt2 := CheckExpr(p2, EVALPUT);
	    CheckVarParam(p2, pt2, 2);
	    p3 := p2^.next;
	    pt3 := BaseType(CheckExpr(p3,EVALGET));
	    if IsBadExpr(p1) or IsBadExpr(p2) or IsBadExpr(p3) 
	       or (pt1^.kind = DTANY) or (pt2^.kind = DTANY) or 
	       (pt3^.kind = DTANY) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'ReadB(f, b, i): f must be FILE');
	    elsif not (pt3^.kind in DataTypeSet{DTINTEGER,DTCARDINAL}) then
		ExprError(p1, 'ReadB(f, b, i): i must be INTEGER or UNSIGNED');
	    else
		PointerOrAddress(p2,pt2);
		retType := integerTypeNode;
	    end;
    
	| BIPREADS :
	    pt1 := CheckExpr(p1, EVALGET);
	    pt2 := CheckExpr(p2, EVALPUT);
	    CheckVarParam(p2, pt2, 2);
	    if nump = 3 then
		p3 := p2^.next;
		pt3 := CheckExpr(p3, EVALGET);
	    end;
	    if IsBadExpr(p1) or IsBadExpr(p2) or 
		    (pt1^.kind = DTANY) or (pt2^.kind = DTANY) then
	    elsif (nump = 3) and (IsBadExpr(p3) or (pt3^.kind = DTANY)) then
	    elsif pt1 # fileTypeNode then
		ExprError(p1, 'ReadS(f, s): f must be FILE');
	    elsif not Passable(arrayOfCharTypeNode, PARAMARRAYVAR, pt2, p2) then
		ExprError(p1, 'ReadS(f, s): s must be ARRAY OF CHAR');
	    else
		if pt2^.arrayKind = ARRAYNOCOUNT then
		    (* Should be 3rd parameter for max number of chars *)
		    if nump # 3 then
			ExprError(p2,
			    'ReadS(f, s, i): NOCOUNT s requires length i');
		    elsif not (pt3^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
		    then
			ExprError(p2,
			    'ReadS(f, s, i): i must be INTEGER or UNSIGNED');
		    end;
		elsif nump # 2 then
		    ExprError(p2,
			'ReadS(f, s, i): Only NOCOUNT s allows length i');
		else
		    if pt2^.arrayKind = ARRAYNORMAL then
			(* Compile time constant *)
			p3 := MakeExprConst(integerTypeNode,
				    NumberOf(pt2^.indexType));
		    else
			(* Get number elements from descriptor *)
			if p2^.kind = EXPRVAR then
			    aen := MakeExprVar(p2^.exprVar, p2);
			elsif (p2^.kind = EXPRCHECK) and 
				(p2^.exprCheck = CHECKDYNARRAY) and
				(p2^.checkExpr^.kind = EXPRVAR) then
			    aen := MakeExprVar(p2^.checkExpr^.exprVar, p2);
			else
			    arrayDesc := DefineVar(nil,addressTypeNode,MEMFAST,
					GSNORMAL, nil);
			    p2 := MakeExprSave(p2, arrayDesc);
			    p1 ^.next := p2;
			    params^.last := p2;
			    aen := MakeExprVal(addressTypeNode,
					MakeExprVar(arrayDesc, p2));
			end;
			adden := MakeExprOffset(addressTypeNode, TKPLUS,
				    aen, longfloat(WORDSIZE));
			p3 := MakeExprVal(integerTypeNode, adden);
		    end;
		    params := AddToExprList(params, p3);
		end;
		PointerOrAddress(p2,pt2);
		retType := booleanTypeNode;
	    end;
	    
	| BIPOPENF :
	    pt1 := CheckExpr(p1,EVALPOINT);
	    pt2 := CheckExpr(p2,EVALPOINT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif not Passable(arrayOfCharTypeNode,PARAMARRAYCONST,pt1,p1) then
		ExprError(p1, 'Open(fn, s): fn must be ARRAY OF CHAR');
	    elsif not Passable(arrayOfCharTypeNode,PARAMARRAYCONST,pt2,p2) then
		ExprError(p1, 'Open(fn, s): s must be ARRAY OF CHAR');
	    else
		RefOpenArray(p1,pt1);
		RefOpenArray(p2,pt2);
	    end;
	    retType := fileTypeNode;
    
	| BIPCLOSEF :
	    pt1 := CheckExpr(p1,EVALGET);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not Passable(fileTypeNode,PARAMCONST,pt1,p1) then
		ExprError(p1, 'Close(f): f must be FILE');
	    end;
    
	| BIPNEWPROCESS:
	    p3 := p2^.next;
	    p4 := p3^.next;
	    pt1 := CheckExpr(p1,EVALGET);
	    pt2 := CheckExpr(p2,EVALGET);
	    pt3 := CheckExpr(p3,EVALGET);
	    pt4 := CheckExpr(p4, EVALPUT);
	    CheckVarParam(p4, pt4, 4);
	    if IsBadExpr(p1) or IsBadExpr(p2) or IsBadExpr(p3) or
		    IsBadExpr(p4) or (pt1^.kind = DTANY) or (pt2^.kind = DTANY)
		    or (pt3^.kind = DTANY) or (pt4^.kind = DTANY) then
	    elsif pt1^.kind # DTPROC then
		ExprError(p1, 'NEWPROCESS(p, a, i, ps): p must be PROC');
	    elsif not Passable(addressTypeNode,PARAMCONST,pt2,p2) then
		ExprError(p1, 'NEWPROCESS(p, a, i, ps): a must be ADDRESS');
	    elsif not Passable(cardinalTypeNode,PARAMCONST,pt3,p3) then
		ExprError(p1,
		    'NEWPROCESS(p, a, i, ps): i must be INTEGER or UNSIGNED');
	    elsif not Passable(processTypeNode,PARAMVAR,pt4,p4) then
		ExprError(p1, 'NEWPROCESS(p, a, i, ps): ps must be PROCESS');
	    else
		retType := processTypeNode;
	    end;
    
	| BIPTRANSFER:
	    pt1 := CheckExpr(p1, EVALPUT);
	    CheckVarParam(p1, pt1, 1);
	    pt2 := CheckExpr(p2, EVALPUT);
	    CheckVarParam(p2, pt2, 2);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif not Passable(processTypeNode,PARAMVAR,pt1,p1) then
		ExprError(p1, 'TRANSFER(ps1, ps2): ps1 must be PROCESS');
	    elsif not Passable(processTypeNode,PARAMVAR,pt2,p2) then
		ExprError(p1, 'TRANSFER(ps1, ps2): ps2 must be PROCESS');
	    end;
    
	| BIPBITAND, BIPBITOR, BIPBITXOR :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif not (pt1^.kind in 
		    DataTypeSet{DTCARDINAL, DTINTEGER, DTWORD}) then
		ExprErrorName(p1, proc^.name,
		    '$(x, y): x must be INTEGER, UNSIGNED, or WORD');
	    elsif not (pt2^.kind in 
		    DataTypeSet{DTCARDINAL, DTINTEGER, DTWORD}) then
		ExprErrorName(p1, proc^.name,
		    '$(x, y): y must be INTEGER, UNSIGNED, or WORD');
	    else
		retType := pt1;
	    end;
    
	| BIPBITSHIFTLEFT, BIPBITSHIFTRIGHT :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif not (pt1^.kind in 
		    DataTypeSet{DTCARDINAL, DTINTEGER, DTWORD}) then
		ExprErrorName(p1, proc^.name,
		    '$(x, i): x must be INTEGER, UNSIGNED, or WORD');
	    elsif not (pt2^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
		ExprErrorName(p1, proc^.name,
		    '$(x, i): i must be INTEGER or UNSIGNED');
	    else
		if p2^.doCheck then
		    p2 := MakeExprCheck(p2, CHECKRANGE, nil, nil, pt2, 
			0, WORDSIZE-1, nil);
		    p1^.next := p2;
		    params^.last := p2;
		end;
		retType := pt1;
	    end;
    
	| BIPBITNOT :
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in 
		    DataTypeSet{DTCARDINAL, DTINTEGER, DTWORD}) then
		ExprError(p1,
		    'BITNOT(x): x must be INTEGER, UNSIGNED, or WORD');
	    else
		retType := pt1;
	    end;
    
	| BIPBITEXTRACT :
	    p3 := p2^.next;
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    pt3 := BaseType(CheckExpr(p3,EVALGET));
	    if IsBadExpr(p1) or IsBadExpr(p2) or IsBadExpr(p3) or 
		(pt1^.kind = DTANY) or (pt2^.kind = DTANY) or 
		(pt3^.kind = DTANY) then
	    elsif not (pt1^.kind in 
		    DataTypeSet{DTCARDINAL, DTINTEGER, DTWORD}) then
		ExprError(p1,
		   'BITEXTRACT(x, b, i): x must be INTEGER, UNSIGNED, or WORD');
	    elsif not (pt2^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
		ExprError(p1,
		   'BITEXTRACT(x, b, i): b must be INTEGER or UNSIGNED');
	    elsif not (pt3^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
		ExprError(p1,
		   'BITEXTRACT(x, b, i): x must be INTEGER or UNSIGNED');
	    else
		if p2^.doCheck then
		    (* Bit offset must be in [0..WORDSIZE-1] *)
		    p2 := MakeExprCheck(p2, CHECKRANGE, nil, nil, pt2, 
			0, WORDSIZE-1, nil);
		    p1^.next := p2;
		    p2^.next := p3;
		end;
		if p3^.doCheck then
		    (* Bit size must be in [1..WORDSIZE] *)
		    p3 := MakeExprCheck(p3, CHECKRANGE, nil, nil, pt3, 
			1, WORDSIZE, nil);
		    p2^.next := p3;
		    params^.last := p3;
		end;
		retType := pt1;
	    end;
    
	| BIPBITINSERT :
	    p3 := p2^.next;
	    p4 := p3^.next;
	    pt1 := BaseType(CheckExpr(p1,EVALGET));
	    pt2 := BaseType(CheckExpr(p2,EVALGET));
	    pt3 := BaseType(CheckExpr(p3,EVALGET));
	    pt4 := BaseType(CheckExpr(p4,EVALGET));
	    if IsBadExpr(p1) or IsBadExpr(p2) or IsBadExpr(p3) or IsBadExpr(p4)
		    or (pt1^.kind = DTANY) or (pt2^.kind = DTANY)
		    or (pt3^.kind = DTANY) or (pt4^.kind = DTANY) then
	    elsif not (pt1^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
		ExprError(p1,
		    'BITINSERT(x, b, i, y): x must be INTEGER or UNSIGNED');
	    elsif not (pt2^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
		ExprError(p1,
		    'BITINSERT(x, b, i, y): b must be INTEGER or UNSIGNED');
	    elsif not (pt3^.kind in DataTypeSet{DTCARDINAL, DTINTEGER}) then
		ExprError(p1,
		    'BITINSERT(x, b, i, y): i must be INTEGER or UNSIGNED');
	    elsif not (pt4^.kind in 
		    DataTypeSet{DTCARDINAL, DTINTEGER, DTWORD}) then
		ExprError(p1,
		  'BITINSERT(x, b, i, y): y must be INTEGER, UNSIGNED or WORD');
	    else
		if p2^.doCheck then
		    (* Bit offset must be in [0..WORDSIZE-1] *)
		    p2 := MakeExprCheck(p2, CHECKRANGE, nil, nil, pt2, 
			0, WORDSIZE-1, nil);
		    p1^.next := p2;
		    p2^.next := p3;
		end;
		if p3^.doCheck then
		    (* Bit size must be in [1..WORDSIZE] *)
		    p3 := MakeExprCheck(p3, CHECKRANGE, nil, nil, pt3, 
			1, WORDSIZE, nil);
		    p2^.next := p3;
		    p3^.next := p4;
		end;
		retType := pt4;
	    end;
	
$else   (* pascal *)
	| BIPwrite :
	    pt1 := BaseType(CheckExprWithSubExprs(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTFILE then (* insert output *)
		outputVarNode^.address.gvn^.used := true;
		p1 := MakeExprVar(outputVarNode, procNameExpr);
		params := PrependExprList(params, p1);
		CheckWriteParams(true);
	    elsif pt1^.isTextFile then
		if p1^.kind = EXPRVAL then
		    p1^ := p1^.exprVal^;
		    p1^.exprType := addressTypeNode;
		end;
		CheckWriteParams(false);
	    else
		CheckWriteBinaryParams();
	    end;

	| BIPwriteln :
	    if nump = 0 then
		outputVarNode^.address.gvn^.used := true;
		p1 := MakeExprVar(outputVarNode, procNameExpr);
		params := PrependExprList(params, p1);
		MakeWriteln(p1);
	    else
		pt1 := BaseType(CheckExprWithSubExprs(p1, EVALGET));
		if IsBadExpr(p1) or (pt1^.kind = DTANY) then
		elsif pt1^.kind # DTFILE then (* insert output *)
		    outputVarNode^.address.gvn^.used := true;
                    p1 := MakeExprVar(outputVarNode, procNameExpr);
                    params := PrependExprList(params, p1);
 		    CheckWriteParams(true);
		elsif pt1^.isTextFile then
		    if p1^.kind = EXPRVAL then
			p1^ := p1^.exprVal^;
			p1^.exprType := addressTypeNode;
		    end;
		    if nump > 1 then
			CheckWriteParams(false);
		    else
			MakeWriteln(p1);
		    end;
		else
		    ExprError(p1, 'writeln(f): f must be a text file');
		end;
	    end;

	| BIPmessage :
	    errorVarNode^.address.gvn^.used := true;
	    p1 := MakeExprVar(errorVarNode, procNameExpr);
	    params := PrependExprList(params, p1);
	    if nump > 0 then
		CheckWriteParams(false);
	    else
		MakeWriteln(p1);
	    end;

	| BIPread :
	    pt1 := CheckExpr(p1, EVALPUT);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTFILE then (* insert input *)
		inputVarNode^.address.gvn^.used := true;
		p1 := MakeExprVar(inputVarNode, procNameExpr);
		params := PrependExprList(params, p1);
		CheckReadParams(pt1);
	    elsif pt1^.isTextFile then
		CheckReadParams(nil);
	    else 
		CheckReadBinaryParams();
	    end;

	| BIPreadln :
	    if nump = 0 then
		inputVarNode^.address.gvn^.used := true;
		p1 := MakeExprVar(inputVarNode, procNameExpr);
		params := PrependExprList(params, p1);
		params := AddToExprList(params, 
		    MakeExprConst(cardinalTypeNode, longfloat(ord(EVALPOINT))));
	    else
		pt1 := CheckExpr(p1, EVALPUT);
		CheckVarParam(p1, pt1, 1);
		if IsBadExpr(p1) or (pt1^.kind = DTANY) then
		elsif pt1^.kind # DTFILE then (* insert input *)
		    inputVarNode^.address.gvn^.used := true;
		    p1 := MakeExprVar(inputVarNode, procNameExpr);
		    params := PrependExprList(params, p1);
		    CheckReadParams(pt1);
		elsif pt1^.isTextFile then
		    if nump = 1 then
			params := AddToExprList(params, 
			    MakeExprConst(cardinalTypeNode, 
				longfloat(ord(EVALPOINT))));
		    else
			CheckReadParams(nil);
		    end;
		else
		    ExprError(p1, 'readln(f): f must be a text file');
		end;
	    end;

	| BIPput, BIPget :
	    pt1 := CheckExpr(p1, EVALPOINT);
	    CheckVarParam(p1, pt1, 1);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTFILE then
		ExprErrorName(p1, proc^.name, '$(f): f must be a file');
	    end;
	    (* Turn into put(unit(f)) or get(unit(f)) *)
	    unitParams := AddToExprList(nil, p1);
	    unitParams := AddToExprList(unitParams,
		MakeExprConst(cardinalTypeNode, longfloat(ord(EVALPOINT))));
	    params := AddToExprList(nil, 
		MakeExprFunc(unitProcNode, p1, unitParams, addressTypeNode));
	    params := AddToExprList(params,
		MakeExprConst(cardinalTypeNode, longfloat(ord(EVALGET))));

	| BIPflush :
	    (* 0 params means call PFLUSH, rather than FLUSH(f) *)
	    if nump = 1 then
		pt1 := CheckExpr(p1, EVALPUT);
		CheckVarParam(p1, pt1, 1);
		if IsBadExpr(p1) or (pt1^.kind = DTANY) then
		elsif pt1^.kind # DTFILE then
		    ExprError(p1, 'flush(f): f must be a file');
		end;
	    end;

	| BIPpage :
	    pt1 := CheckExpr(p1, EVALPUT);
	    CheckVarParam(p1, pt1, 1);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif (pt1^.kind # DTFILE) or not pt1^.isTextFile then
		ExprError(p1, 'page(f): f must be a text file');
	    end;
	    (* Turn into page(unit(f)) *)
	    unitParams := AddToExprList(nil, p1);
	    unitParams := AddToExprList(unitParams,
		    MakeExprConst(cardinalTypeNode, longfloat(ord(EVALPOINT))));
	    params := AddToExprList(nil, 
		   MakeExprFunc(unitProcNode, p1, unitParams, addressTypeNode));

	| BIPeof :
	    if nump = 1 then
		pt1 := CheckExpr(p1, EVALPOINT);
		If IsBadExpr(p1) or (pt1^.kind = DTANY) then
		elsif (pt1^.kind # DTFILE) then
		    ExprError(p1, 'eof(f): f must be a file');
		end;
	    else (* insert file input for the guy *)
		inputVarNode^.address.gvn^.used := true;
		p1 := MakeExprVar(inputVarNode, procNameExpr);
		params := PrependExprList(params, p1);
	    end;
	    retType := booleanTypeNode;
		
	| BIPeoln :
	    if nump = 1 then
		pt1 := CheckExpr(p1, EVALPOINT);
		If IsBadExpr(p1) or (pt1^.kind = DTANY) then
		elsif (pt1^.kind # DTFILE) or not pt1^.isTextFile then
		    ExprError(p1, 'eoln(f): f must be a text file');
		end;
	    else (* insert file input for the guy *)
		inputVarNode^.address.gvn^.used := true;
		p1 := MakeExprVar(inputVarNode, procNameExpr);
		params := PrependExprList(params, p1);
	    end;
	    retType := booleanTypeNode;

	| BIPlinelimit :
	    pt1 := CheckExpr(p1, EVALPUT);
	    CheckVarParam(p1, pt1, 1);
	    pt2 := BaseType(CheckExpr(p2, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif (pt1^.kind # DTFILE) or not pt1^.isTextFile then
		ExprError(p1, 'linelimit(f,i): f must be a text file');
	    elsif (pt2^.kind # DTINTEGER) then
		ExprError(p1, 'linelimit(f,i): i must be an integer');
	    end;

	| BIPremove :
	    pt1 := CheckExpr(p1, EVALGET);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not Passable(arrayOfCharTypeNode,PARAMARRAYCONST,pt1,p1) then
	       ExprError(p1, 'remove(n): n must be an array of char');
	    else    (* Add max size of name to param list *)
		if pt1^.kind = DTSTRING then
		    p2 := MakeExprConst(integerTypeNode,
			    longfloat(pt1^.stringLength));
		else
		    p2 := MakeExprConst(integerTypeNode,
			    NumberOf(pt1^.indexType));
		end;
		params := AddToExprList(params, p2);
	    end;
	    
	| BIPstlimit :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
            if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif (pt1^.kind # DTINTEGER) then
		ExprError(p1, 'stlimit(i): i must be an integer');
	    end;

	| BIPreset, BIPrewrite:
	    pt1 := CheckExpr(p1, EVALPUT);
	    CheckVarParam(p1, pt1, 1);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTFILE then
		ExprErrorName(p1, proc^.name, '$(f): f must be a file');
	    else 
		if nump = 1 then    (* reset (f) *)
		    (* Add 0, 0 to params *)
		    p2 := MakeExprConst(integerTypeNode, 0.0);
		    params := AddToExprList(params, p2);
		    p3 := MakeExprConst(integerTypeNode, 0.0);
		    params := AddToExprList(params, p3);
		else		    (* reset (f, name) *)
		    pt2 := BaseType(CheckExpr(p2, EVALPOINT));
		    if IsBadExpr(p2) or (pt1^.kind = DTANY) then
		    elsif not Passable(arrayOfCharTypeNode,PARAMARRAYCONST,
			    pt2,p2) then
			ExprErrorName(p1, proc^.name,
					'$(f, n): n must be an array of char');
		    else    (* Add max size of name to param list *)
			if pt2^.kind = DTSTRING then
			    p3 := MakeExprConst(integerTypeNode,
				    longfloat(pt2^.stringLength));
			else    
			    p3 := MakeExprConst(integerTypeNode,
				    NumberOf(pt2^.indexType));
			end;
			params := AddToExprList(params, p3);
		    end;
		end (* if nump = 1 then else *);
		(* Add element size to param list *)
		if pt1^.isTextFile then
		    p := MakeExprConst(integerTypeNode, 0.0);
		else
		    p := MakeExprConst(integerTypeNode,
			    longfloat((SizeOf(pt1^.fileType) + BYTESIZE - 1) div
				    BYTESIZE));
		end;
		params := AddToExprList(params, p);
		(* reset(f) has 2 params, reset(f, name) has 4 params *)
	    end (* if *);

	| BIPpack:
	    p3 := p2^.next;
	    pt1 := CheckExpr(p1, EVALPOINT);
	    pt2 := BaseType(CheckExpr(p2, EVALGET));
	    pt3 := CheckExpr(p3, EVALPUT);
	    CheckVarParam(p3, pt3, 3);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or IsBadExpr(p2) or 
		(pt2^.kind = DTANY) or IsBadExpr(p3) or (pt3^.kind = DTANY) then
	    elsif pt1^.kind # DTARRAY then
	        ExprError(p1, 'pack(a, i, z): a must be an array');
	    elsif pt2^.kind # DTINTEGER then
	        ExprError(p1, 'pack(a, i, z): i must be an integer');
	    elsif pt3^.kind # DTARRAY then
	        ExprError(p1, 'pack(a, i, z): z must be an array');
	    elsif pt1^.elementType # pt3^.elementType then
		ExprError(p1, 
		  'pack(a, i, z): a, z element types must be identical');
	    elsif NumberOf(pt1^.indexType) < NumberOf(pt3^.indexType) then
		ExprError(p1, 
		    'pack(a, i, z): cannot have more elements in a than in z');
	    else
		(* Preserve info that CheckExpr has destroyed
		    var a: array[m..n] of t; z: array[u..v] of t;
		    pack(a,i,z) -> 
			PACK(i, a, z, size(t), m, 
			    number(a)-number(z), size(z)); *)
		params^.first := nil;
		params^.last := nil;
		params := AddToExprList(params, p2);
		params := AddToExprList(params, p1);
		params := AddToExprList(params, p3);
		conen := MakeExprConst(integerTypeNode,
			    longfloat((pt1^.elementSize+UNITSIZE-1) div 
				    UNITSIZE));
		params := AddToExprList(params, conen);
		conen := MakeExprConst(integerTypeNode, 
			    LowerBoundOf(pt1^.indexType));
		params := AddToExprList(params, conen);
		conen := MakeExprConst(integerTypeNode,
			    NumberOf(pt1^.indexType) -
			    NumberOf(pt3^.indexType));
		params := AddToExprList(params, conen);
		conen := MakeExprConst(integerTypeNode, longfloat(
		    ((trunc(NumberOf(pt3^.indexType))*pt3^.elementSize
			    +UNITSIZE-1) div UNITSIZE)));
		params := AddToExprList(params, conen);
	    end;

	| BIPunpack:
	    p3 := p2^.next;
	    pt1 := CheckExpr(p1, EVALPOINT);
	    pt2 := CheckExpr(p2, EVALPUT);
	    CheckVarParam(p2, pt2, 2);
	    pt3 := BaseType(CheckExpr(p3, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or IsBadExpr(p2) or 
		(pt2^.kind = DTANY) or IsBadExpr(p3) or (pt3^.kind = DTANY) then
	    elsif pt1^.kind # DTARRAY then
	        ExprError(p1, 'unpack(z, a, i): z must be an array');
	    elsif pt2^.kind # DTARRAY then
	        ExprError(p1, 'unpack(z, a, i): a must be an array');
	    elsif pt3^.kind # DTINTEGER then
	        ExprError(p1, 'unpack(z, a, i): i must be an integer');
	    elsif pt1^.elementType # pt2^.elementType then
		ExprError(p1,
		    'unpack(z, a, i): z, a element types must be identical');
	    elsif NumberOf(pt2^.indexType) < NumberOf(pt1^.indexType) then
		ExprError(p1, 
		    'unpack(z, a, i) cannot have more elements in a than in z');
	    else
		(* Preserve info that CheckExpr has destroyed
		    var a: array[m..n] of t; z: array[u..v] of t;
		    unpack(z,a,i) -> 
			UNPACK(i, a, z, bytesize(t), m, 
			     number(a)-number(z), bytesize(z)); *)
		params^.first := nil;
		params^.last := nil;
		params := AddToExprList(params, p3);
		params := AddToExprList(params, p2);
		params := AddToExprList(params, p1);
		conen := MakeExprConst(integerTypeNode,
			    longfloat((pt2^.elementSize+UNITSIZE-1) div
				    UNITSIZE));
		params := AddToExprList(params, conen);
		conen := MakeExprConst(integerTypeNode, 
			    LowerBoundOf(pt2^.indexType));
		params := AddToExprList(params, conen);
		conen := MakeExprConst(integerTypeNode,
			    NumberOf(pt2^.indexType) -
			    NumberOf(pt1^.indexType));
		params := AddToExprList(params, conen);
		conen := MakeExprConst(integerTypeNode, longfloat(
		       (trunc(NumberOf(pt1^.indexType))*pt1^.elementSize
			+UNITSIZE-1) div UNITSIZE));
		params := AddToExprList(params, conen);
	    end;

	| BIPargc :
	    (* Create ExprNode for argcVarNode *)
	    argcVarNode^.address.gvn^.used := true;
	    p1 := NewExprNode(EXPRVAR);
	    p1^.exprVar := argcVarNode;
	    pt1 := CheckExpr(p1, EVALGET);
	    params := AddToExprList(params, p1);
	    retType := integerTypeNode;
	    
	| BIPargv :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    pt2 := CheckExpr(p2, EVALPUT);
	    CheckVarParam(p2, pt2, 2);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) or 
	       IsBadExpr(p2) or (pt2^.kind = DTANY) then
	    elsif pt1^.kind # DTINTEGER then
		ExprError(p1, 'argv(n, a): n must be an integer');
	    elsif not Passable(arrayOfCharTypeNode,PARAMARRAYCONST,pt2,p2) then
		ExprError(p1, 'argv(n, a): a must be an array of char');
	    else
		(* Add length of array node *)
		p3 := MakeExprConst(integerTypeNode, NumberOf(pt2^.indexType));
		params := AddToExprList(params, p3);
	    end;

	| BIPsqr:
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in DataTypeSet{DTINTEGER, DTLONGREAL}) then
		ExprError(p1, 'sqr(x): x must be integer or real');
	    end;
	    retType := pt1;
	    
	| BIPsqrt, BIPsin, BIPcos, BIPexp, BIPln, BIParctan :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif Assignable(longrealTypeNode, pt1, p1) = nil then
		ExprErrorName(p1, proc^.name,
		    '$(x): x must be integer or real');
	    end;
	    retType := longrealTypeNode;

	| BIPround :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in DataTypeSet{DTINTEGER, DTLONGREAL}) then
		ExprError(p1, 'round(x): x must be integer or real');
	    end;
	    retType := integerTypeNode;

	| BIPexpo :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif Assignable(longrealTypeNode, pt1, p1) = nil then
		ExprError(p1, 'expo(x): x must be integer or real');
	    end;
	    retType := integerTypeNode;

	| BIPcard :
	    pt1 := BaseType(CheckExpr(p1, EVALPOINT));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTSET then
		ExprError(p1, 'card(s): s must be a set');
	    else
		SetSetType(p1, pt1);
		(* Create size in bytes for second param *)
		p2 := MakeExprConst(integerTypeNode, 
			longfloat(SizeOf(pt1) div UNITSIZE));
		params := AddToExprList(params, p2);
	    end;
	    retType := integerTypeNode;
	    
	| BIPseed :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1^.kind # DTINTEGER then
		ExprError(p1, 'seed(i): i must be an integer');
	    else
		(* create an ExprNode for _seed's address *)
		seedVarNode^.address.gvn^.used := true;
		p2 := MakeExprVar(seedVarNode, p1);
		params := AddToExprList(params, p2);
		(* ...and one for _seed's value *)
		p3 := NewExprNode(EXPRVAR);
		p3^.exprVar := seedVarNode;
		pt3 := CheckExpr(p3, EVALGET);
		params := AddToExprList(params, p3);
		retType := integerTypeNode;
	    end;

	| BIPrandom :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
            if IsBadExpr(p1) or (pt1^.kind = DTANY) then
            elsif not (pt1^.kind in DataTypeSet{DTINTEGER, DTLONGREAL}) then
		ExprError(p1, 'random(x): x must be an integer or real');
	    end;
	    retType := longrealTypeNode;

	| BIPundefined :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in DataTypeSet{DTINTEGER, DTLONGREAL}) then
		ExprError(p1, 'undefined(x): x must be integer or real');
	    end;
	    retType := booleanTypeNode;

	| BIPsucc, BIPpred :
	    pt1 := BaseType(CheckExpr(p1, EVALGET));
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif not (pt1^.kind in indexableTypes) then
		ExprErrorName(p1, proc^.name,
		    '$(x): x must be a discrete scalar');
	    end;
	    retType := pt1;

	| BIPdate, BIPtime :
	    pt1 := CheckExpr(p1, EVALPUT);
	    CheckVarParam(p1, pt1, 1);
	    if IsBadExpr(p1) or (pt1^.kind = DTANY) then
	    elsif pt1 # alfaTypeNode then
		ExprErrorName(p1, proc^.name, '$(a): a must be alfa');
	    end;

	| BIPclock, BIPsysclock, BIPwallclock :
	    retType := integerTypeNode;

	| BIPhalt, BIPnull :
	    (* nothing *);
	
$end
	    
	end (* case *);
    end (* if error # ERRNUMP *);

    case error of
    | ERRNONE :
    | ERRNUMP   : ErrorName(proc^.name,'Incorrect number of parameters for $')
    end;
    return totalErrors = saveTotalErrors;
end CheckBuiltin;


$if modula2 then
procedure ConstBuiltinFunction(const procName : IdentList; 
			       const params   : ConstParamList)	: ConstNode;
type
    ErrorKind = (ERRNONE, ERRNUMP, ERRTYPE1, ERRTYPE2, ERRBADFUNC);
var
    error : ErrorKind;
    p, p1, p2 : ConstParamNode;
    sym : Symbol;
    proc : ProcNode;
    result : ConstNode;
    tn, atn : TypeNode;
    value : HugeInteger;
    subnum : cardinal;
    enum : EnumNode;
    i, nump : integer;
    size : MemoryOffset;
    fi: cardinal;
    found : boolean;
    bip : BuiltinProcType;
    saveTotalErrors : integer;
begin
    error := ERRNONE;
    saveTotalErrors := totalErrors;
    result := CardinalConst(1.0);
    p1 := nil;
    p2 := nil;
    nump := 0;
   if (params # nil) then
	p1 := params^.first;
        p := p1;
	while p # nil do
	    nump := nump + 1;
	    p := p^.next;
	end;
	if nump > 1 then
	    p2 := p1^.next;
	end;
    end;

    sym := QualifiedName(procName);
    if sym = nil then
    elsif procName^.first # nil then
	error := ERRBADFUNC;
    elsif sym^.kind # SYMPROC then
	error := ERRBADFUNC;
    else
	proc := sym^.symProc;
	if proc^.builtin = BIPNOTBIP then
	    error := ERRBADFUNC;
	elsif not (proc^.builtin in constFunctions) then
	    error := ERRBADFUNC;
	else
	    if (proc^.builtin = BIPMIN) and (nump = 1) then
		bip := BIPFIRST;
	    elsif (proc^.builtin = BIPMAX) and (nump = 1) then
		bip := BIPLAST;
	    else
		bip := proc^.builtin;
	    end;
	    if (nump < proc^.minParams) or (proc^.maxParams < nump) then
		error := ERRNUMP
	    end;
	end;
    end;
    if (error = ERRNONE) and (totalErrors = saveTotalErrors) then
	case bip of
	| BIPABS :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif not (p1^.cpConst^.kind in
		    DataTypeSet{DTINTEGER, DTREAL, DTLONGREAL}) then
		error := ERRTYPE1;
	    else
		result^ := p1^.cpConst^;
		if result^.kind = DTINTEGER then
		    result^.cardVal := abs(result^.cardVal);
		else
		    result^.realVal := abs(result^.realVal);
		end;
	    end;
	
	| BIPCAP :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif p1^.cpConst^.kind # DTCHAR then
		error := ERRTYPE1;
	    else
		result^ := p1^.cpConst^;
		result^.charVal := CAP(result^.charVal);
	    end;
	
	| BIPCHR :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif not (p1^.cpConst^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
	    then
		error := ERRTYPE1;
	    elsif (p1^.cpConst^.cardVal < 0.0) or
		    (p1^.cpConst^.cardVal > MAXCHAR) then
		ErrorNumber('CHR: argument out of range [0..%]',trunc(MAXCHAR));
	    else
		result^.kind := DTCHAR;
		result^.charVal := chr(trunc(p1^.cpConst^.cardVal));
	    end;
	
	| BIPFLOAT, BIPLONGFLOAT :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif not (p1^.cpConst^.kind in
		    DataTypeSet{DTREAL, DTINTEGER, DTCARDINAL, DTLONGREAL}) then
		error := ERRTYPE1;
	    else
		if bip = BIPFLOAT then
		    result^.kind := DTREAL;
		else
		    result^.kind := DTLONGREAL;
		end;
		if p1^.cpConst^.kind in DataTypeSet{DTINTEGER, DTCARDINAL} then
		    result^.realVal := p1^.cpConst^.cardVal;
		else
		    result^.realVal := p1^.cpConst^.realVal;
		end;
	    end;
	
	| BIPTRUNC :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif not (p1^.cpConst^.kind in DataTypeSet{DTREAL, DTLONGREAL})
	    then
		error := ERRTYPE1;
	    elsif (p1^.cpConst^.realVal > MAXINT)
		    or (p1^.cpConst^.realVal < MININT) then
		Error('FLOAT: argument out of range for integers');
	    else
		result^.kind := DTINTEGER;
		result^.cardVal := longfloat(trunc(p1^.cpConst^.realVal));
	    end;
	
	| BIPODD :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif not (p1^.cpConst^.kind in DataTypeSet{DTINTEGER, DTCARDINAL})
	    then
		error := ERRTYPE1;
	    else
		result^.kind := DTBOOLEAN;
		result^.boolVal := odd(trunc(p1^.cpConst^.cardVal));
	    end;
	
	| BIPORD :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif not (p1^.cpConst^.kind in indexableTypes) then
		error := ERRTYPE1;
	    else
		result^.kind := DTINTEGER;
		result^.cardVal := OrdOf(p1^.cpConst);
	    end;
	
	| BIPVAL :
	    if p1^.kind # CPTYPE then
		error := ERRTYPE1;
	    elsif p2^.kind # CPCONST then
		error := ERRTYPE2;
	    elsif not (p2^.cpConst^.kind in indexableTypes) then
		error := ERRTYPE2;
	    else
		tn := BaseType(p1^.cpType);
		value := OrdOf(p2^.cpConst);
		if tn^.kind = DTBOOLEAN then
		    if (value < 0.0) or (value > 1.0) then
			Error('VAL: boolean value out of range');
		    else
			result^.kind := DTBOOLEAN;
			result^.boolVal := value # 0.0;
		    end;
		elsif tn^.kind = DTCHAR then
		    if (value < 0.0) or (value > MAXCHAR) then
			ErrorNumber('VAL: char value out of range [0..%]', 
				    trunc(MAXCHAR));
		    else
			result^.kind := DTCHAR;
			result^.charVal := chr(trunc(value));
		    end;
		elsif tn^.kind = DTENUMERATION then
		    if (value < longfloat(tn^.enumMin)) or 
		       (value > longfloat(tn^.enumMax)) then
			Error('VAL: enumeration value out of range');
		    else
			result^.kind := DTENUMERATION;
			enum := tn^.enumList^.first;
			found := false;
			while not found and (enum # nil) do
			    if enum^.enumOrd = trunc(value) then
				found := true;
			    else
				enum := enum^.next;
			    end;
			end;
			if not found then
			    Error('VAL: value not in enumeration');
			else
			    result^.enumVal := enum;
			end;
		    end;
		elsif tn^.kind in DataTypeSet{DTINTEGER,DTCARDINAL} then
		    result^.kind := DTINTEGER;
		    result^.cardVal := value;
		else
		    error := ERRTYPE1;
		end;
	    end;
	
	| BIPMIN, BIPMAX :
	    if p1^.kind # CPCONST then
		error := ERRTYPE1;
	    elsif p1^.cpConst^.kind in DataTypeSet{DTINTEGER,DTCARDINAL} then
		if not (p2^.cpConst^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
		then
		    ErrorName(proc^.name,
			'Both parameters of $ must be same type');
		else
		    if (bip = BIPMIN) =
			(p1^.cpConst^.cardVal < p2^.cpConst^.cardVal) then
			result^ := p1^.cpConst^;
		    else
			result^ := p2^.cpConst^;
		    end;
		end;
	    elsif p1^.cpConst^.kind in DataTypeSet{DTREAL,DTLONGREAL} then
		if not (p2^.cpConst^.kind in DataTypeSet{DTREAL,DTLONGREAL})
		then
		    ErrorName(proc^.name,
			'Both parameters of $ must be same type');
		else
		    if (bip = BIPMIN) =
			(p1^.cpConst^.realVal < p2^.cpConst^.realVal) then
			result^ := p1^.cpConst^;
		    else
			result^ := p2^.cpConst^;
		    end;
		end;
	    else
		error := ERRTYPE1;
	    end;
	
	| BIPFIRST, BIPLAST :
	    if p1^.kind # CPTYPE then
		ErrorName(proc^.name,'Function $ requires a type as parameter');
	    else
		tn := p1^.cpType;
		if tn^.kind = DTREAL then
		    if bip = BIPFIRST then
			value := minreal;
		    else
			value := maxreal;
		    end;
		    result^.kind := DTREAL;
		    result^.realVal := value;
		elsif tn^.kind = DTLONGREAL then
		    if bip = BIPFIRST then
			value := minlongreal;
		    else
			value := maxlongreal;
		    end;
		    result^.kind := DTLONGREAL;
		    result^.realVal := value;
		else
		    if bip = BIPFIRST then
			value := LowerBoundOf(tn);
		    else
			value := UpperBoundOf(tn);
		    end;
		    if tn^.kind = DTCHAR then
			result^.kind := DTCHAR;
			result^.charVal := chr(trunc(value));
		    elsif tn^.kind = DTBOOLEAN then
			result^.kind := DTBOOLEAN;
			result^.boolVal := value = 1.0;
		    elsif tn^.kind = DTENUMERATION then
			found := false;
			enum := tn^.enumList^.first;
			while not found and (enum # nil) do
			    if longfloat(enum^.enumOrd) = value then
				found := true;
			    else
				enum := enum^.next;
			    end;
			end;
			if not found then
			    ErrorName(proc^.name,
				    '$: value not in enumeration?');
			else
			    result^.kind := DTENUMERATION;
			    result^.enumVal := enum;
			end;
		    else
			(* integers *)
			result^.kind := DTINTEGER;
			result^.cardVal := value;
		    end;
		end;
	    end;
	
	| BIPTBITSIZE, BIPTBYTESIZE, BIPTWORDSIZE :
	    if p1^.kind # CPTYPE then
		ErrorName(proc^.name,'Function $ requires a type as parameter');
	    else
		size := SizeOf(p1^.cpType);
		if bip = BIPTBYTESIZE then
		    size := (size+BYTESIZE-1) div BYTESIZE;
		elsif bip = BIPTWORDSIZE then
		    size := (size+WORDSIZE-1) div WORDSIZE;
		end;
		result^.kind := DTCARDINAL;
		result^.cardVal := longfloat(size);
	    end;
	
	| BIPBITSIZE, BIPBYTESIZE, BIPWORDSIZE :
	    if p1^.kind # CPVAR then
		ErrorName(proc^.name,
			    'Function $ requires a variable as a parameter');
	    else
		tn := p1^.cpVar^.varType;
		size := SizeOf(tn);
		if bip = BIPBYTESIZE then
		    size := (size+BYTESIZE-1) div BYTESIZE;
		elsif bip = BIPWORDSIZE then
		    size := (size+WORDSIZE-1) div WORDSIZE;
		end;
		result^.kind := DTCARDINAL;
		result^.cardVal := longfloat(size);
	    end;
	
	| BIPHIGH, BIPLOW, BIPNUMBER :
	    if p1^.kind # CPVAR then
		ErrorName(proc^.name,
			    'Function $ requires a variable as a parameter');
	    else
		tn := BaseType(p1^.cpVar^.varType);
		if tn^.kind # DTARRAY then
		    ErrorName(proc^.name,
				'Function $ requires an array as a parameter');
		elsif nump = 1 then
		    subnum := 1;
		elsif p2^.kind # CPCONST then
		    ErrorName(proc^.name,
			'Function $ dimension number must be a constant');
		elsif not (p2^.cpConst^.kind in 
			DataTypeSet{DTINTEGER,DTCARDINAL}) then
		    ErrorName(proc^.name,
			'Function $ dimension number must be a cardinal');
		else
		    subnum := trunc(p2^.cpConst^.cardVal);
		    if subnum <= 0 then
			ErrorName(proc^.name,
			    'Function $ dimension number must be positive');
		    end;
		end;
		atn := tn;
		fi := 1;
		while (fi < subnum) do
		    fi := fi + 1;
		    atn := BaseType(atn^.elementType);
		    if atn^.kind # DTARRAY then
			ErrorNameNumber(proc^.name,
			   "$(a, %): array a doesn't have that many dimensions",
			    subnum);
			exit while;
		    end;
		end;
		if totalErrors # saveTotalErrors then
		elsif atn^.arrayKind # ARRAYNORMAL then
		   ErrorName(proc^.name,'Function $ array size not a constant');
		else
		    if bip = BIPHIGH then
			value := UpperBoundOf(atn^.indexType);
		    elsif bip = BIPLOW then
			value := LowerBoundOf(atn^.indexType);
		    else
			value := NumberOf(atn^.indexType);
		    end;
		    if value >= 0.0 then
			result^.kind := DTINTEGER;
		    else
			result^.kind := DTCARDINAL;
		    end;
		    result^.cardVal := value;
		end;
	    end;
	    
	end;
    end;
    case error of
    | ERRNONE    :
    | ERRNUMP    : ErrorName(sym^.name,'Incorrect number of parameters for $')
    | ERRTYPE1   : ErrorName(sym^.name,'Wrong type, $ parameter #1')
    | ERRTYPE2   : ErrorName(sym^.name,'Wrong type, $ parameter #2')
    | ERRBADFUNC : ErrorName(sym^.name, 
			'$ cannot be evaluated at compile time');
    end;
    return result;
end ConstBuiltinFunction;
$end (* modula2 *)

begin
    enumTableNumber := 0;
end CheckBuiltin.
