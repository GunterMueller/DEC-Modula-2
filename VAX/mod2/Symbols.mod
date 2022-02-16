implementation module Symbols;

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
    Writef, output;

from MemLib import
    ALLOCATE;

from Strings import
    String, NonHashText, WriteString, EqualAnyCase;

from Globals import
    TraceSymtab, DEBUG;
$if modula2 then
from Globals import standardKeywordFlag;
$end

from Errors import
    ErrorName, SilentError;

from Decls import
    CheckEqualType, CheckEqualProc;
 
from TypeInfo import
    StoredType, BaseType;

(* symbol table *)
const
    SYMBOLTABLESIZE = 1000;
    type HashIndex = [0..SYMBOLTABLESIZE];
var
    symbolTable : array HashIndex of Symbol;

procedure StartScope (open : boolean) : Scope;
var
    scope : Scope;
begin
    if DEBUG and TraceSymtab then
	Writef(output, 'StartScope %d\n', generateBlockNumber);
    end;
    new(scope);
    scope^.block := generateBlockNumber;
    scope^.open := open;
    scope^.enclosing := currScope;
    new(scope^.symbols);
    scope^.symbols^.first := nil;
    scope^.symbols^.last := nil;
    currScope := scope;
    generateBlockNumber := generateBlockNumber + 1;
    return scope;
end StartScope;

procedure OpenScope (scope : Scope);
var
    newScope : Scope;
begin
    (* Re-open scope.  No definitions permitted *)
    if DEBUG and TraceSymtab then
	Writef(output, 'OpenScope %d\n', scope^.block);
    end;
    new(newScope);
    newScope^.block := scope^.block;
    newScope^.open := true;
    newScope^.enclosing := currScope;
    newScope^.symbols := nil;
    currScope := newScope;
end OpenScope;

procedure EndScope;
begin
    if DEBUG and TraceSymtab then
	Writef(output, 'EndScope %d\n', currScope^.block);
    end;
    currScope := currScope^.enclosing;
end EndScope;

procedure LookUpSymbol (name : String; scope : Scope) : Symbol;
(* Try finding symbol, first in scope provided, then chain down all open
   scopes.  If still not found, and started in currScope, take a look in 
   the builtinScope (integer, etc.).  *)
var
    sym     : Symbol;
    symName : String;
    hash    : integer;
    originalScope : Scope;
begin
    originalScope := scope;
    loop
	if DEBUG and TraceSymtab then
	    Writef(output, 'LookUpSymbol block %d\n', scope^.block);
	end;
	hash := (name^.hash + scope^.block) mod (SYMBOLTABLESIZE+1);
	sym := symbolTable[hash];
	while (sym # nil) do
	    if DEBUG and TraceSymtab then
		Writef(output, 'LookUpSymbol ');
		WriteString(output, sym^.name);
		Writef(output, ' %d', sym^.block);
		WriteString(output, name);
		Writef(output, ' %d %d\n', scope^.block, hash);
	    end;
	    if (sym^.block = scope^.block) then
		if (sym^.name = name) or 
			((sym^.symCase = ANYCASE) and 
			    EqualAnyCase(sym^.name,name)) then
		    sym^.used := true;
		    return sym;
		end;
	    end;
	    sym := sym^.nextInTable;
	end;
	if scope^.open then
	    scope := scope^.enclosing;
	elsif scope = builtinScope then
	    exit;
	elsif originalScope = currScope then
	    scope := builtinScope;
	else
	    exit;
	end;
    end (* loop *);
    if DEBUG and TraceSymtab then
	Writef(output, 'LookUpSymbol did not find ');
	WriteString(output,name);
	Writef(output, ' %d %d\n', originalScope^.block, hash);
    end;
    return nil;
end LookUpSymbol;

procedure DefineSymbol (var sym : Symbol; name : String; scope : Scope;
	symCase : SymbolCase; kind : SymbolKind) : boolean;
var
    hash, builtinHash : integer;
    found : boolean;
begin
$if modula2 then
    if not standardKeywordFlag then
        if (scope^.block <= MAXBUILTINSCOPES) and (scope^.block # 0) then
	    symCase := ANYCASE; (* Builtin scope but not the compilation unit's *)
        end;
    end;
$end (* modula2 *)
    hash := (name^.hash + scope^.block) mod (SYMBOLTABLESIZE+1);
    sym := symbolTable[hash];
$if pascal then
    (* Gross hack to detect redefinition of input and output *)
    if ((name = inputString) or (name = outputString)) and
	((scope^.block > MAXBUILTINSCOPES) or (scope^.block = 0)) then
	ErrorName(name, 'File "$" cannot be redefined');
        sym := nil;
    end;
$end (* pascal *)
    found := false;
    while (sym # nil) do
	if (sym^.block = scope^.block) then
	    if (sym^.name = name)
$if modula2 then
		    or (((sym^.symCase = ANYCASE) or (symCase = ANYCASE)) and
			EqualAnyCase(sym^.name,name))
$end		    
		    then
		(* if previously SYMANY, just replace current value *)
		if (sym^.kind = SYMANY)then
		    sym^.kind := kind;
		    return true;
		else
		    sym^.used := true;
		    return false;
		end;
	    end;
	end;
	sym := sym^.nextInTable;
    end;
$if modula2 then
(* Look for it in builtin scope if it isn't in currScope, just to make sure
   you don't redefine INTEGER *)
    if (sym = nil) and standardKeywordFlag and
	    (currModule # nil) and (scope = currModule^.scope) then
        builtinHash := (name^.hash + builtinScope^.block) mod (SYMBOLTABLESIZE+1);
        sym := symbolTable[builtinHash];
        while (sym # nil) do
	    if sym^.block = builtinScope^.block then
	        if (sym^.name = name)
			or (((sym^.symCase = ANYCASE) or (symCase = ANYCASE)) and
			EqualAnyCase(sym^.name,name)) 
			    then
		    sym^.used := true;
		    return false;
	        end;
	    end;
	    sym := sym^.nextInTable;
        end;
    end;
$end

    new(sym);
    sym^.name	      := name;
    sym^.block        := scope^.block;
    sym^.nextInTable  := symbolTable[hash];
    symbolTable[hash] := sym;
    sym^.kind         := kind;
    sym^.symCase      := symCase;
    sym^.used         := false;
    if DEBUG and TraceSymtab then
	Writef(output, 'DefineSymbol ');
	WriteString(output,sym^.name);
	Writef(output, ' %d %d\n', sym^.block, hash);
    end;
    sym^.next := nil;
    if scope^.symbols^.last = nil then
	scope^.symbols^.first := sym;
    else
	scope^.symbols^.last^.next := sym;
    end;
    scope^.symbols^.last := sym;
    return true;
end DefineSymbol;

procedure DumpSymbols;
var
    i : integer;
    sym : Symbol;
begin
    for i := 0 to SYMBOLTABLESIZE do
	sym := symbolTable[i];
	while sym # nil do
	    Writef(output, 'hv=%d b=%d %n ', i, sym^.block, sym^.kind);
	    WriteString(output,sym^.name);
	    if sym^.kind = SYMMODULE then
		Writef(output, 's=%d es=%d', sym^.symModule^.scope^.block,
			sym^.symModule^.exportScope^.block);
	    end;
	    Writef(output, '\n');
	    sym := sym^.nextInTable;
	end;
    end;
end DumpSymbols;

(* QualifiedName:  Looks up a possible qualified name *)
(* Removes idents from names as it searches.  Leaves extras, if any, in names *)
procedure QualifiedName (names : IdentList) : Symbol;
var
    sym : Symbol;
    id : IdentNode;
    scope : Scope;
begin
    assert ((names # nil) and (names^.first # nil));
    scope := currScope;
    id := names^.first;
    repeat
	sym := LookUpSymbol(id^.name,scope);
	if sym = nil then
	    ErrorName(id^.name,'Symbol $ not found');
	    if DefineSymbol(sym, id^.name, currModule^.scope, ONECASE, SYMANY) then
	    end;
	    return nil;
	else
	    (* remove name from front of list *)
	    id := id^.next;
	    names^.first := id;
	    if sym^.kind = SYMANY then
		SilentError();
		return nil;
	    elsif sym^.kind = SYMMODULE then
		scope := sym^.symModule^.exportScope;
	    else
		return sym;
	    end;
	end;
    until (id = nil);
    return sym;
end QualifiedName;

procedure TypeOf (names : IdentList) : TypeNode;
var
    tn : TypeNode;
    sym : Symbol;
begin
    sym := QualifiedName(names);
    if sym = nil then
	tn := anyTypeNode;
    elsif names^.first # nil then
	ErrorName(sym^.name,'Qualification error on type $');
	tn := anyTypeNode;
    elsif (sym^.kind # SYMTYPE) then
	ErrorName(sym^.name,'$ is not a type');
	tn := anyTypeNode;
    else
	tn := StoredType(sym^.symType);
	if tn^.size = 0 then
	    ErrorName(sym^.name,'Type $ not defined before use');
	end;
    end;
    return tn;
end TypeOf;


procedure Port(sym : Symbol; scope : Scope) : Symbol;
var
    nsym, esym, saveNext, saveNextInTable : Symbol;
    saveBlock : BlockNumber;
    enum : EnumNode;
    tn : TypeNode;
    id : IdentNode;
    mn : ModuleNode;
begin
    if sym = nil then
	return nil	(* ||| Happens for a redefined constant - CED 6/17/88*)
    end;
    if DefineSymbol(nsym,sym^.name,scope,sym^.symCase,sym^.kind) then
	(* copy contents of symbol.  Need to keep next and block *)
        saveNextInTable := nsym^.nextInTable;
	saveNext := nsym^.next;
	saveBlock := nsym^.block;
	nsym^ := sym^;
	nsym^.nextInTable := saveNextInTable;
	nsym^.next := saveNext;
	nsym^.block := saveBlock;
	if sym^.kind = SYMTYPE then
	    tn := BaseType(sym^.symType);
	    if tn^.kind = DTENUMERATION then
		enum := tn^.enumList^.first;
		while enum # nil do
		    esym := Port(enum^.enumSym,scope);
		    enum := enum^.next;
		end;
	    end;
	end;
    else
	(* sym is being ported, but we found nsym, so nsym is considered to *)
	(* be defined first *)
	CheckEqualSym(nsym,sym);
    end;
    return nsym;
end Port;

procedure CheckEqualSym(sym1, sym2 : Symbol);
(* ||| Can't most cases, like SYMENUM be turned into an error? *)
var
    error : boolean;
begin
    error := false;
    if sym1^.kind = sym2^.kind then
	case sym1^.kind of
	| SYMPROC   :	CheckEqualProc(sym1,sym2)
	| SYMTYPE   :	CheckEqualType(sym1,sym2^.symType,true)
	| SYMCONST  :	error := sym1^.symConst # sym2^.symConst
	| SYMVAR    :	error := sym1^.symVar # sym2^.symVar
	| SYMMODULE :	error := sym1^.symModule # sym2^.symModule
	| SYMFIELD  :	error := sym1^.symField # sym2^.symField
	| SYMENUM   :	error := sym1^.symEnum # sym2^.symEnum
	end;
    else
	error := true;
    end;
    if error then
	ErrorName(sym1^.name,'Ported symbol $ redefined in block');
    end;
end CheckEqualSym;

var
    i : HashIndex;
    globalScope: Scope;

begin (* Symbols *)
    stringDataType[DTPOINTER]       := NonHashText('pointer');
    stringDataType[DTRECORD]	    := NonHashText('record');
    stringDataType[DTARRAY]	    := NonHashText('array');
    stringDataType[DTINTEGER]       := NonHashText('integer');
    stringDataType[DTBOOLEAN]       := NonHashText('boolean');
    stringDataType[DTCHAR]	    := NonHashText('char');
    stringDataType[DTRENAME]	    := NonHashText('rename');
    stringDataType[DTOPAQUE]	    := NonHashText('opaque');
    stringDataType[DTSTRING]	    := NonHashText('string');
    stringDataType[DTREAL]	    := NonHashText('real');
    stringDataType[DTLONGREAL]      := NonHashText('longreal');
    stringDataType[DTSET]	    := NonHashText('set');
    stringDataType[DTCARDINAL]      := NonHashText('cardinal');
    stringDataType[DTSUBRANGE]      := NonHashText('subrange');
    stringDataType[DTENUMERATION]   := NonHashText('enumeration');
    stringDataType[DTPROC]	    := NonHashText('proc');
    stringDataType[DTWORD]	    := NonHashText('word');
    stringDataType[DTBYTE]	    := NonHashText('byte');
    stringDataType[DTANY]	    := NonHashText('any');

    for i := 0 to SYMBOLTABLESIZE do
	symbolTable[i] := nil;
    end;
    withQualList := nil;

    generateBlockNumber := 0;
    currScope := nil;
    (* globalScope is scope with globally exported identifiers *)
    globalScope := StartScope(false);
    (* leave globalScope active *)

    (* builtinScope is scope with builtin identifiers *)
    builtinScope := StartScope(false);
    EndScope;

$if pascal then
    programFileNameList := nil;
    programFileVarList := nil;
$end
end Symbols.
