module imc;

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

import unix;
from io import
    output, writef, sreadf, swritef;
from system import
    TByteSize, Adr;
from parameters import
    NumParameters, GetParameter;
from strings import
    Compare, Assign, Append;
from MemLib import
    ALLOCATE;
import aOut;
import stab;
from Strings import
    AddChar, NewString, NewText, NonHashText, String, CopyString, WriteString;
from list import
    AddToList;
from numtab import
    Number, LookUpNumber, DefineNumber,
    MAXBUILTINTYPES, traceNumtab, cardIntTypeNode;
from symtab import
    TypeNode, EnumNode, DataType, currScope, FieldNode, ParamNode,
    ModuleNode, PortNode, ConstNode, SetValue, ParamKind, SymbolKind,
    traceSymtab, ArrayKind, PointerKind, GlobalSymKind, fileList;
from porttab import
    DefinePort, DefineModule, CheckExports,
    currModule, tracePorttab, errorCount, autoMakeFlag, WatchModule,
    ExamineModule, moduleList, logErrorsFlag, IgnoreModule, 
    libraryCheckFlag, fatalErrorFlag, standardCardinalFlag, 
    standardStringFlag, standardKeywordFlag,
    numCompares, numEq, numQuick, numetoi, numitoe, numetoe,
    numCheck, numCheckConst, numCheckProc, numSetCanon,  numCheckType,
    numCheckVar, numTypeStack, numTypeStackLoop;
from execute import
    AddArg, AddString, Execute, MAXARGS;

const
$if titan then
    EXPORTSYMBOL = aOut.N_IMC;  (* nlist type for imc export symbol *)
$else
    EXPORTSYMBOL = stab.N_MOD2;	(* nlist type for imc export symbol *)
$end
    LOCALSYMBOL = stab.N_LSYM;	(* nlist type for local symbol *)

    ARCHIVEMAGICSTRING = "!<arch>\n";	(* marker for archive file *)
    ARCHIVEMAGICSTRINGSIZE = 8;
    ARCHIVEDIRECTORYSIZE = 60;

    FILENAMESIZE = 1024;
type
    ObjectHeader = record
	case boolean of
	| TRUE : 		(* header for an object file *)
	    ex : aOut.exec;
	|
	FALSE : 	(* header for an archive file *)
	    armagic : array [1..ARCHIVEMAGICSTRINGSIZE] of char;
	end;
    end;
    ArchiveDirectory = record
	name : array [1..16] of char;
	date : array [1..12] of char;
	usergroup : array [1..12] of char;	(* beware alignment! *)
	mode : array [1..8] of char;
	size : array [1..12] of char;		(* last 2 chars are `\n *)
    end;

    CharPtr = dynarray @nocheck @nocount of char;

    CharSet = set of char;
    FileName = array [0..FILENAMESIZE] of char;
var
    modName	    : FileName;
    moduleName      : FileName;
    errorMsg	    : array [1..100] of char;
    echoFlag, executeFlag : boolean;
    makeFlags       : FileName;
    inArchive       : boolean;
    fileListIndex   : cardinal;
    traceFiles      : boolean;
    identStartChar, identContinueChar, digitChar : array char of boolean;

procedure Panic(const name, msg : array of char);
begin
    writef(output,"%s : %s\n",name,msg);
    halt;
end Panic;

procedure SaveArg(const arg : array of char);
begin
    inc(fileListIndex);
    fileList[fileListIndex] := NonHashText(arg);
end SaveArg;

procedure BuildProc(retType : TypeNode) : TypeNode;
var
    tn : TypeNode;
    pn : ParamNode;
begin
    new(tn);
    tn^.kind := DTPROC;
    tn^.size := -1;
    tn^.alignment := -1;
    tn^.retType := retType;
    tn^.numParams := GetNumber();
    tn^.paramList := nil;
    tn^.inlineTimeStamp := 0;
    SkipOver(';');
    while currChar # ';' do
	new(pn);
	if currChar in CharSet{'0'..'9'} then
	    pn^.name := nil;
	else
	    pn^.name := GetString();
	    SkipOver(':');
	end;
	pn^.paramType := GetType();
	SkipOver(',');
	pn^.kind := val(ParamKind,cardinal(GetNumber()));
	tn^.paramList := AddToList(tn^.paramList,pn);
	SkipOver(';');
    end;
    return tn;
end BuildProc;

procedure BuildRecord(tn : TypeNode);
var
    fn : FieldNode;
begin
    tn^.kind := DTRECORD;
    tn^.size := GetNumber();
    SkipOver(',');
    tn^.alignment := GetNumber();
    SkipOver(',');
    tn^.fieldList := nil;
    repeat
	new(fn);
	fn^.name := GetString();
	SkipOver(':');
	fn^.fieldType := GetType();
	SkipOver(',');
	fn^.offset := GetNumber();
	SkipOver(',');
	fn^.size := GetNumber();
	tn^.fieldList := AddToList(tn^.fieldList,fn);
	SkipOver(';');
    until currChar = ';';
    NextChar();
end BuildRecord;

procedure BuildEnum(tn : TypeNode);
var
    en : EnumNode;
begin
    tn^.kind := DTENUMERATION;
    tn^.enumCount := 0;
    tn^.enumList := nil;
    repeat
	inc(tn^.enumCount);
	new(en);
	en^.name := GetString();
	SkipOver(':');
	en^.enumOrd := GetNumber();
	en^.enumType := tn;
	tn^.enumList := AddToList(tn^.enumList,en);
	SkipOver(',');
    until currChar = ';';
    NextChar();
end BuildEnum;

procedure NewTypeNode(dt : DataType) : TypeNode;
var
    tn : TypeNode;
begin
    new(tn);
    tn^.kind := dt;
    tn^.canonicalType := nil;
    tn^.size := -1;
    tn^.alignment := -1;
    return tn;
end NewTypeNode;

(* GetType: Get a type pointer *)
procedure GetType():TypeNode;
var
    tn, retType, ignoreTn : TypeNode;
    indirectType : boolean;
    size, alignment, ptrKindOrd : integer;
    ptrKind : PointerKind;
begin
    size := -1;
    alignment := -1;
    ptrKind := PTRMODULA;
    if currChar = 'i' then
	repeat
	    NextChar();
	until currChar = ',';
	NextChar();
	indirectType := true;
    else
	indirectType := false;
    end;
    case currChar of
    | '@' :
	NextChar();
	case currChar of
	| 'a' :
	    NextChar();
	    alignment := GetNumber();
	    SkipOver(';');
	    tn := GetType();
	    tn^.alignment := alignment;
	| 's' :
	    NextChar();
	    size := GetNumber();
	    SkipOver(';');
	    tn := GetType();
	    tn^.size := size;
	| 'p' :
	    NextChar();
	    ptrKindOrd := GetNumber();
	    ptrKind := val(PointerKind,ptrKindOrd);
	    SkipOver(';');
	    tn := GetType();
	    if tn^.kind = DTPOINTER then
		tn^.ptrKind := ptrKind;
	    elsif (tn^.kind = DTOPENARRAY) and (tn^.arrayKind = ARRAYDYNAMIC)
	    then
		tn^.dynArrayKind := ptrKind;
	    else
		DataError('Pointer attribute on non-pointer');
	    end;
	else
	    DataError('Unknown type attribute');
	end;
    | '0'..'9':
	tn := GetTypeNumber();
	if tn <> nil then 
	    tn^.size := size;
	    tn^.alignment := alignment;
	end;
    | 'r':
	NextChar;
	tn := NewTypeNode(DTSUBRANGE);
	tn^.baseType := GetType();
	SkipOver(';');
	tn^.subMinOrd := GetNumber();
	SkipOver(';');
	tn^.subMaxOrd := GetNumber();
    | 'a':
	NextChar;
	tn := NewTypeNode(DTARRAY);
	tn^.indexType := GetType();
	SkipOver(';');
	tn^.elementType := GetType();
    | 'O':
	NextChar;
	tn := NewTypeNode(DTOPENARRAY);
	tn^.arrayKind := ARRAYOPEN;
	tn^.numDimensions := GetNumber();
	SkipOver(',');
	tn^.openElementType := GetType();
	tn^.dynArrayKind := ptrKind;
    | 'D':
	NextChar;
	tn := NewTypeNode(DTOPENARRAY);
	tn^.arrayKind := ARRAYDYNAMIC;
	tn^.numDimensions := GetNumber();
	SkipOver(',');
	tn^.openElementType := GetType();
	tn^.dynArrayKind := ptrKind;
    | 'E':
	NextChar;
	tn := NewTypeNode(DTOPENARRAY);
	tn^.arrayKind := ARRAYSUBARRAY;
	tn^.numDimensions := GetNumber();
	SkipOver(',');
	tn^.openElementType := GetType();
	tn^.dynArrayKind := ptrKind;
    | 'S':
	NextChar;
	tn := NewTypeNode(DTSET);
	tn^.setRange := GetType();
    | 'o':
	NextChar;
	tn := NewTypeNode(DTOPAQUE);
	tn^.opaqueName := GetString();
	if currChar = ',' then
	    NextChar();
	    ignoreTn := GetType();
	end;
	SkipOver(';');
    | 's':
	NextChar;
	tn := NewTypeNode(DTRECORD);
	BuildRecord(tn);
    | 'f':
	NextChar;
	retType := GetType();
	SkipOver(',');
	tn := BuildProc(retType);
    | 'p':
	NextChar;
	tn := BuildProc(nil);
    | 'e':
	NextChar;
	tn := NewTypeNode(DTENUMERATION);
	BuildEnum(tn);
    | '*':
	NextChar;
	tn := NewTypeNode(DTPOINTER);
	tn^.toType := GetType();
	tn^.ptrKind := ptrKind;
    else
	DataError('Unknown type');
	tn := nil;
    end;
    if indirectType then
	SkipOver(';');
    end;
    return tn;
end GetType;

procedure BuildConst():ConstNode;
var
    cn : ConstNode;
    sv : SetValue;
    en : EnumNode;
begin
    new(cn);
    case currChar of
    | 'r':
	NextChar;
	cn^.kind := DTREAL;
	cn^.realVal := GetValue();
    | 'i':
	NextChar;
	cn^.kind := DTCARDINAL;
	cn^.cardVal := GetNumber();
    | 'b':
	NextChar;
	cn^.kind := DTBOOLEAN;
	cn^.boolVal := GetNumber();
    | 'c':
	NextChar;
	cn^.kind := DTCHAR;
	cn^.charVal := GetNumber();
    | 's':
	NextChar;
	cn^.kind := DTSTRING;
	SkipOver('''');
	loop
	    if currChar = '''' then
		NextChar;
		if currChar # '''' then
		    exit;
		end;
		AddChar('''');
	    else
		AddChar(currChar);
	    end;
	    NextChar;
	end;
	cn^.strVal := NewString();
    | 'S':
	NextChar;
	cn^.kind := DTSET;
	new(sv);
	sv^.setType := GetType();
	SkipOver(',');
	sv^.size := GetNumber();
	SkipOver(',');
	sv^.value := GetValue();
	cn^.setVal := sv;
    | 'e':
	NextChar;
	cn^.kind := DTENUMERATION;
	new(en);
	en^.enumType := GetType();
	SkipOver(',');
	en^.enumOrd := GetNumber();
	cn^.enumVal := en;
    else
	DataError('Unknown constant');
	cn := nil;
    end;
    return cn;
end BuildConst;

procedure DefineTypeNumber(number : cardinal) : Number;
var
    num : Number;
    tn : TypeNode;
begin
    tn := GetType();
    if not DefineNumber(num,number,0) then
	if num^.numType^.kind # DTOPAQUE then
	    swritef(errorMsg,'Type %d already defined',number);
	    DataError(errorMsg);
	else
	    (* update the type, but don't affect pointers to it *)
	    num^.numType^ := tn^;
	end;
    else
	num^.numType := tn;
    end;
    return num;
end DefineTypeNumber;

(* GetTypeNumber: Get the type corresponding to the type number *)
procedure GetTypeNumber():TypeNode;
var
    number : integer;
    num : Number;
    tn : TypeNode;
begin
    number := GetNumber();
    if currChar = '=' then
	NextChar();
	num := DefineTypeNumber(number);
    else
	num := LookUpNumber(number,0);
	if num = nil then
	    new(tn);
	    tn^.size := -1;
	    tn^.alignment := -1;
	    tn^.kind := DTOPAQUE;
	    if not DefineNumber(num,number,0) then
		swritef(errorMsg,'Missing type %d already defined',number);
		DataError(errorMsg);
	    else
		num^.numType := tn;
	    end;
	end;
    end;
    return num^.numType;
end GetTypeNumber;

var
    currEntry : CharPtr;
    currCharIndex : cardinal;
    currChar : char;

procedure @inline NextChar();
begin
    currCharIndex := currCharIndex + 1;
    currChar := currEntry^[currCharIndex];
end NextChar;

procedure SkipOver(c : char);
var
    eligible : boolean;
    entry : CharPtr;
begin
    if currChar # c then
	swritef(errorMsg,'Expected "%c", found "%c"',c,currChar);
	DataError(errorMsg);
    else
	eligible := currChar in CharSet{',', ';'};
	NextChar;
	if eligible and (currChar = '?') then
	    (* continuation: go to the next entry *)
	    entry := Adr(stringTab^[symTab^[symTabIndex].n_un.n_strx]);
	    inc(symTabIndex);
	    InitChar(entry);
	end;
    end;
end SkipOver;

const
    MAXDATAERRORS = 100;
var
    dataErrorCount : cardinal;
procedure DataError(const msg : array of char);
begin
    writef(output,'Error in symbol table information: %s: char %d "%s"\n',
	msg,currCharIndex,currEntry^);
    inc(dataErrorCount);
    if dataErrorCount > MAXDATAERRORS then
	Panic('Too many errors','files are messed up');
    end;
end DataError;

procedure GetNumber() : integer;
var
    n : integer;
begin
    if currChar = '-' then
	NextChar;
	n := 0;
	while digitChar[currChar] do
	    n := n * 10 + integer(ord(currChar) - ord('0'));
	    NextChar;
	end;
	return -n;
    elsif digitChar[currChar] then
	n := integer(ord(currChar) - ord('0'));
	NextChar;
	while digitChar[currChar] do
	    n := n * 10 + integer(ord(currChar) - ord('0'));
	    NextChar;
	end;
	return n;
    else
	DataError('Number expected');
	return 0;
    end;
end GetNumber;

procedure GetString() : String;
    var start : cardinal;
        s     : array [0..2047] of char;
	i     : cardinal;
begin
    if not identStartChar[currChar] then
	DataError('Identifier expected');
	return nil;
    end;
(*    start := currCharIndex; *)
    i := 0;
    repeat
	s[i] := currChar;
	inc(i);
	currCharIndex := currCharIndex + 1;
        currChar := currEntry^[currCharIndex];
(* ||| Kludge till find bug about optimized dynarray refs in @inline procedures
	NextChar; *)
    until not identContinueChar[currChar];
(*    return NewText(currEntry^[start:currCharIndex-start]); *)
    s[i] := 0C;
    return NewText(s);
end GetString;

procedure GetValue() : String;
begin
    while currChar # ';' do
	AddChar(currChar);
	NextChar;
    end;
    return NewString();
end GetValue;

(* InitChar: Initialize NextChar routine *)
procedure InitChar(entry : CharPtr);
begin
    currEntry := entry;
    currCharIndex := 0;
    currChar := currEntry^[currCharIndex];
end InitChar;

(* ProcessLocalEntry: Process the next local entry *)
procedure ProcessLocalEntry();
var
    number : integer;
    tn : TypeNode;
    num : Number;
    entry : CharPtr;
begin
    entry := Adr(stringTab^[symTab^[symTabIndex].n_un.n_strx]);
    inc(symTabIndex);
    InitChar(entry);
    if echoFlag then
	writef(output,"l %s\n",entry^);
    end;
    while currChar # ':' do
	if currChar = 0C then
	    return;
	end;
	NextChar;
    end;
    NextChar;
    if currChar # 't' then
	return;
    end;
    NextChar;
    number := GetNumber();
    if (number <= MAXBUILTINTYPES) or (currChar # '=') then
	return;
    end;
    NextChar;
    num := DefineTypeNumber(number);
end ProcessLocalEntry;

(* ProcessExportEntry: Process the next export entry *)
procedure ProcessExportEntry() : boolean;
var
    name, modname : String;
    unqual, imported : boolean;
    extern : GlobalSymKind;
    pn : PortNode;
    tn : TypeNode;
    entryType : char;
    mn : ModuleNode;
    entry : CharPtr;
    timeStamp : integer;
begin
    entry := Adr(stringTab^[symTab^[symTabIndex].n_un.n_strx]);
    inc(symTabIndex);
    InitChar(entry);
    if echoFlag then
	writef(output,"X %s\n",entry^);
    end;
    modname := GetString();
    if currChar = '.' then
	NextChar;
	name := GetString();
    else
	name := nil;
    end;
    SkipOver(':');
    SkipOver('X');
    unqual := currChar = '0';
    NextChar;
    imported := currChar = '0';
    NextChar;
    if currChar = '0' then
	extern := GSNORMAL;
    elsif currChar = '1' then
	extern := GSINTERNAL;
    elsif currChar = '2' then
	extern := GSGLOBAL;
    elsif currChar = '3' then
	extern := GSEXTERNAL;
    elsif currChar = '4' then
	extern := GSINLINE;
    else
	Panic(fileName,"bad symbol kind");
    end;
    NextChar;
    entryType := currChar;
    NextChar;
    case entryType of
    | 'v':
	new(pn);
	pn^.kind := SYMVAR;
	pn^.symVar := GetType();
	DefinePort(modname,name,pn,imported,unqual,extern,currModule);
    | 'p':
	new(pn);
	pn^.kind := SYMPROC;
	timeStamp := GetNumber();
	SkipOver(',');
	pn^.symProc := BuildProc(nil);
	pn^.symProc^.inlineTimeStamp := timeStamp;
	DefinePort(modname,name,pn,imported,unqual,extern,currModule);
    | 'f':
	new(pn);
	pn^.kind := SYMPROC;
	timeStamp := GetNumber();
	SkipOver(',');
	tn := GetType();
	SkipOver(',');
	pn^.symProc := BuildProc(tn);
	pn^.symProc^.inlineTimeStamp := timeStamp;
	DefinePort(modname,name,pn,imported,unqual,extern,currModule);
    | 't':
	new(pn);
	pn^.kind := SYMTYPE;
	pn^.symType := GetType();
	DefinePort(modname,name,pn,imported,unqual,extern,currModule);
    | 'c':
	new(pn);
	pn^.kind := SYMCONST;
	SkipOver('=');
	pn^.symConst := BuildConst();
	DefinePort(modname,name,pn,imported,unqual,extern,currModule);
    | 'm':
	(* new module *)
	if inArchive then
	    mn := DefineModule(modname,0);
	else
	    mn := DefineModule(modname,fileListIndex);
	end;
	if mn^.defined then
	    writef(output,"Duplicate module %s(",fileName);
	    WriteString(output,modname);
	    writef(output,") ignored\n");
	    return(false);
	end;
	mn^.defined := true;
	currScope := mn^.scope;
	if logErrorsFlag then
	    writef(output,"Module ");
	    WriteString(output,modname);
	    writef(output,'\n');
	end;
	if inArchive then
	    mn^.watchErrors := TRUE;
	end;
	mn^.named := not inArchive;
	currModule := mn;
    | 'z':
	currModule := nil;
	(* end of external information *)
	return(false);
    else
	swritef(errorMsg,'Unknown symbol type %c',currChar);
	DataError(errorMsg);
    end;
    return(true);
end ProcessExportEntry;

var
    stringTab : dynarray of char;
    symTab : dynarray of aOut.nlist;
    symTabIndex : integer;

(* ProcessFile:  Process the file at the current seek position *)
procedure ProcessFile(obj : integer; header : ObjectHeader);
var
    readLen : integer;
    stringTabSize, stringTabOffset : cardinal;
    numSymbols, symTabOffset : integer;
    ignore : integer;
begin
    if header.ex.a_magic # aOut.OMAGIC then
	return;
    end;
    (* read in symbol table *)
    numSymbols := header.ex.a_syms div TByteSize(aOut.nlist);
    if numSymbols <= 0 then
	Panic(fileName,"bad object file");
    end;
    symTabOffset := header.ex.a_text+header.ex.a_data +
		header.ex.a_trsize + header.ex.a_drsize;
    ignore := unix.lseek(obj,symTabOffset,1);
    new(symTab,numSymbols);
    readLen := unix.read(obj,symTab^,header.ex.a_syms);

    (* get size of string table *)
    readLen := unix.read(obj,stringTabSize,TByteSize(integer));
    if (readLen # TByteSize(integer)) or (stringTabSize < TByteSize(integer))
    then
	Panic(fileName,"bad object file");
    end;

    (* read in string table; first word was size *)
    new(stringTab,stringTabSize);
    readLen := unix.read(obj,stringTab^[TByteSize(integer):0],
	    stringTabSize-TByteSize(integer));

    symTabIndex := 0;
    while symTabIndex < numSymbols do
(* |||
with symTab^[symTabIndex] do
writef(output, 'type = %2x, other = %2x, desc = %4x\n',
n_type, n_other, n_desc);
end;
*)
	if symTab^[symTabIndex].n_type = EXPORTSYMBOL then
	    if not ProcessExportEntry() then
		exit while;
	    end;
$if titan then
	elsif (currModule # nil) and
		((symTab^[symTabIndex].n_type = aOut.N_DBX) and
		(symTab^[symTabIndex].n_subtype = LOCALSYMBOL)) then
$else
	elsif (currModule # nil) and
		(symTab^[symTabIndex].n_type = LOCALSYMBOL) then
$end
	    ProcessLocalEntry();
	else
	    inc(symTabIndex);
	end;
    end;

end ProcessFile;

procedure ProcessArchive(obj : integer);
var
    arOffset, moduleSize : cardinal;
    dir : array [1..ARCHIVEDIRECTORYSIZE] of char;
    readLen : integer;
    header : ObjectHeader;
begin
    inArchive := TRUE;
    (* start after magic string *)
    arOffset := ARCHIVEMAGICSTRINGSIZE;
    loop
	(* position to next module *)
	ignore := unix.lseek(obj,arOffset,0);

	(* get directory entry *)
	readLen := unix.read(obj,dir,ARCHIVEDIRECTORYSIZE);
	if readLen # ARCHIVEDIRECTORYSIZE then
	    exit;
	end;
	readLen := sreadf(dir,"%s %*d %*d %*d %*d %d",moduleName,moduleSize);

	(* read in object file header *)
	readLen := unix.read(obj,header,TByteSize(aOut.exec));
	if readLen # TByteSize(aOut.exec) then
	    exit;
	end;
	if traceFiles then
	    writef(output, '  -------- Start archive %s, module %s\n',
		fileName, moduleName);
	end;
	ProcessFile(obj,header);
	if traceFiles then
	    writef(output, '  -------- End archive %s, module %s\n',
		fileName, moduleName);
	end;

	arOffset := (arOffset + ARCHIVEDIRECTORYSIZE + moduleSize+1) div 2 * 2;
    end;
    inArchive := FALSE;
end ProcessArchive;

var
    fileName, arg : FileName;
    i, j, readLen, fileNameLength, ignore : integer;
    objFile : integer;
    header : ObjectHeader;
    mn : ModuleNode;
    loopch : char;
    objectModifiedTime : integer;
    allX : boolean;

begin
    for loopch := first(char) to last(char) do
	if loopch in CharSet{'a'..'z','A'..'Z','_'} then
	    identStartChar[loopch] := true;
	    identContinueChar[loopch] := true;
	    digitChar[loopch] := false;
	elsif loopch in CharSet{'0'..'9'} then
	    identStartChar[loopch] := false;
	    identContinueChar[loopch] := true;
	    digitChar[loopch] := true;
	else
	    identStartChar[loopch] := false;
	    identContinueChar[loopch] := false;
	    digitChar[loopch] := false;
	end;
    end (* for *);
    modName := "mod";
    currModule := nil;
    dataErrorCount := 0;
    fileListIndex := 0;
    echoFlag := FALSE;
    traceNumtab	    := FALSE;
    traceSymtab     := FALSE;
    tracePorttab    := FALSE;
    traceFiles      := FALSE;
    autoMakeFlag    := FALSE;
    logErrorsFlag   := FALSE;
    libraryCheckFlag := FALSE;
    makeFlags := "";
    inArchive := FALSE;
    if NumParameters <= 1 then
	Panic("usage","mod2.2 files.o");
    end;
    if NumParameters > MAXARGS div 2 then
	Panic("usage","Cannot handle that many files");
    end;
    for i := 1 to NumParameters-1 do
	GetParameter(i,fileName,fileNameLength);
	if fileName[0] = '-' then
	    case fileName[1] of
	    | 'B':
		j := 2;
		while fileName[j] # 0C do
		    modName[j-2] := fileName[j];
		    inc(j);
		end;
		modName[j-2] := 0C;

	    | 'e':
		echoFlag := TRUE;
	    | 'l':
		SaveArg(fileName);
		(* get end of library name *)
		j := 2;
		while fileName[j] # 0C do
		    arg[j-2] := fileName[j];
		    inc(j);
		end;
		arg[j-2] := 0C;
		Assign(fileName,"/lib/lib");
		Append(fileName,arg);
		Append(fileName,".a");
		objFile := unix.open(fileName,unix.OPENREADONLY,0);
		if objFile >= 0 then
		    if echoFlag then
			writef(output,"File %s\n",fileName);
		    end;
		    ProcessArchive(objFile);
		    ignore := unix.close(objFile);
		else
		    Assign(fileName,"/usr/lib/lib");
		    Append(fileName,arg);
		    Append(fileName,".a");
		    objFile := unix.open(fileName,unix.OPENREADONLY,0);
		    if objFile >= 0 then
			ProcessArchive(objFile);
			ignore := unix.close(objFile);
		    else
			Assign(fileName,"/usr/local/lib/lib");
			Append(fileName,arg);
			Append(fileName,".a");
			objFile := unix.open(fileName,unix.OPENREADONLY,0);
			if objFile >= 0 then
			    ProcessArchive(objFile);
			    ignore := unix.close(objFile);
			else
			    Panic(arg,"Cannot open library file");
			end;
		    end;
		end;
	    | 'L':
		libraryCheckFlag := TRUE;
	    | 'm':
		autoMakeFlag := TRUE;
		Assign(makeFlags,fileName);
	    | 'N':
		IgnoreModule(fileName);
	    | 's':
		case fileName[2] of
		| 0C:
		    standardCardinalFlag := TRUE;
		    standardStringFlag := TRUE;
		    standardKeywordFlag := TRUE;
		| 'c':
		    standardCardinalFlag := TRUE;
		| 'k':
		    standardKeywordFlag := TRUE;
		| 's':
		    standardStringFlag := TRUE;
		else
		    Panic("Unknown option",fileName);
		end;
		if standardCardinalFlag then
		    cardIntTypeNode^.kind := DTCARDINAL;
		end;
	    | 'T':
		if Compare(fileName,'=',"-Tnumtab") then
		    traceNumtab := TRUE;
		elsif Compare(fileName,'=',"-Tsymtab") then
		    traceSymtab := TRUE;
		elsif Compare(fileName,'=',"-Tporttab") then
		    tracePorttab := TRUE;
		elsif Compare(fileName, '=', "-Tfiles") then
		    traceFiles := TRUE;
		end;
	    | 'v':
		logErrorsFlag := TRUE;
	    | 'W':
		WatchModule(fileName);
	    | 'X':
		ExamineModule(fileName);
	    | else
		Panic("Unknown option",fileName);
	    end;
	else
	    if i # NumParameters then
		SaveArg(fileName);
	    end;
	    objFile := unix.open(fileName,unix.OPENREADONLY,0);
	    if objFile < 0 then
		Panic(fileName,"Cannot open file");
	    else
		if echoFlag then
		    writef(output,"File %s\n",fileName);
		end;
		readLen := unix.read(objFile,header,TByteSize(aOut.exec));
		if readLen # TByteSize(aOut.exec) then
		    Panic(fileName,"Error reading file header");
		end;
		if Compare(header.armagic,'=',ARCHIVEMAGICSTRING) then
		    ProcessArchive(objFile);
		elsif header.ex.a_magic = aOut.OMAGIC then
		    if traceFiles then
			writef(output, '  -------- Start file %s\n', fileName);
		    end;
		    ProcessFile(objFile, header);
		    if traceFiles then
			writef(output, '  -------- End file %s\n', fileName);
		    end;
		else
		    Panic(fileName,"Not an object file or library");
		end;
		ignore := unix.close(objFile);
	    end;
	end;
    end;
    CheckExports();
    if (errorCount > 0) and autoMakeFlag and not fatalErrorFlag then
	AddArg("-c");
	AddArg(makeFlags);
	if libraryCheckFlag then
	    AddArg("-L");
	end;
	if logErrorsFlag then
	    AddArg("-v");
	end;
	i := 2;	(* skip -m *)
	while makeFlags[i] # 0C do
	    j := 0;
	    while (makeFlags[i] # ',') and (makeFlags[i] # ' ') and
		    (makeFlags[i] # 0C) do
		fileName[j] := makeFlags[i];
		inc(i);
		inc(j);
	    end;
	    fileName[j] := 0C;
	    AddArg(fileName);
	    while (makeFlags[i] = ' ')
		or (makeFlags[i] = ',')
	    do
		inc(i);
	    end;
	end;
	executeFlag := FALSE;
	(* Determine if there are any -W candidates.  If not, we have only
	   -X guys, who must be dependent on @inline timestamp of the .mod
	   file, but look fine because they were compiled after the .def *)
	mn := moduleList^.first;
	allX := true;
	loop
	    if mn = nil then
		exit;
	    end;
	    if mn^.outOfDate and (mn^.oldObject or mn^.examineErrors) then
		allX := false;
		exit;
	    end;
	    mn := mn^.next;
	end;
	writef(output,"Recompiling");
	mn := moduleList^.first;
	while mn # nil do
	    if mn^.outOfDate or (mn^.watchErrors and mn^.named) then
		CopyString(mn^.name,moduleName);
		if mn^.watchErrors then
		    (* Leave in watch mode...we already recompiled it *)
		    Assign(arg, '-W');
		    Append(arg, moduleName);
		    AddArg(arg);
		elsif (mn^.oldObject or mn^.examineErrors) or allX then
		    (* Recompile it *)
		    if mn^.named and (mn^.fileName # 0) then
			(* Put in watch status and recompile it *)
			Assign(arg,"-W");
			Append(arg,moduleName);
			AddArg(arg);
			CopyString(fileList[mn^.fileName],fileName);
			i := 0;
			while fileName[i] # 0C do
			    inc(i);
			end;
			if (fileName[i-2] = '.') and (fileName[i-1] = 'o') then
			    fileName[i-1] := 0C;
			    Append(fileName,"mod");
			    AddArg(fileName);
			    writef(output," %s",fileName);
			    fileList[mn^.fileName] := nil;
			else
			    fatalErrorFlag := TRUE;
			    writef(output,
				"Cannot determine source file from %s\n",
				fileName);
			end;
			executeFlag := TRUE;
		    else
			fatalErrorFlag := TRUE;
			writef(output,
			    "Cannot find file for module %s\n",moduleName);
		    end;
		else	    (* Just put in examine mode *)
		    Assign(arg, '-X');
		    Append(arg, moduleName);
		    AddArg(arg);
		end;
	    elsif not mn^.builtin and mn^.ignoreErrors then
		CopyString(mn^.name,moduleName);
		Assign(arg,"-N");
		Append(arg,moduleName);
		AddArg(arg);
	    end;
	    mn := mn^.next;
	end;
	writef(output,"\n");
	if executeFlag then
	    for i := 1 to fileListIndex do
		if fileList[i] # nil then
		    AddString(fileList[i]);
		end;
	    end;
	    Execute(modName);
	end;
    end;
    if errorCount > 100 then
	errorCount := 101;
    end;
    if logErrorsFlag then
	writef(output,"numCompares = %d\n",numCompares);
	writef(output,"numEq = %d\n",numEq);
	writef(output,"numQuick = %d\n",numQuick);
	writef(output,"numetoi = %d\n",numetoi);
	writef(output,"numitoe = %d\n",numitoe);
	writef(output,"numetoe = %d\n",numetoe);
	writef(output,"numCheck = %d\n",numCheck);
	writef(output,"numCheckConst = %d\n",numCheckConst);
	writef(output,"numCheckProc = %d\n",numCheckProc);
	writef(output,"numSetCanon = %d\n",numSetCanon);
	writef(output,"numCheckType = %d\n",numCheckType);
	writef(output,"numCheckVar = %d\n",numCheckVar);
	writef(output,"numTypeStack = %d\n",numTypeStack);
	writef(output,"numTypeStackLoop = %d\n",numTypeStackLoop);
    end;
    unix.uexit(errorCount);
end imc.
