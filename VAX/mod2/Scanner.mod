implementation module Scanner;

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


$const kjncomments = FALSE;


from SYSTEM import BYTESIZE;

from io import
    Writef, Writec, Writeb, Writec, output,
    SReadf, Reads, Readc, Readb, File, Open, Close, input;

from MemLib import
    ALLOCATE;

from Machine import
    CharSet, MAXCHAR, MAXCARD, MAXCARDI, TABCHAR, LINEFEEDCHAR,
    FORMFEEDCHAR, FileName, ExitProgram, 
    HugeInteger;

from Globals import
    standardStringFlag, genCheckFlag, debugSet, DEBUG, host, TargetMachine;

from Strings import
    String, NewString, NonHashString, AddChar, GetChar, WriteString, 
    CopyString, NonHashText, AddString, KnownText, KnownNonHashText;

$if modula2 then
from Machine import
    BACKSPACECHAR, RETURNCHAR, VERTICALTABCHAR;
from Globals import standardKeywordFlag, compiledDefFlag;
from Errors import SilentError;

$else

from Globals import
    standardPascalFlag, warningPascalFlag, bufferFlag;
from Strings import NewText;
$end

import IOLib;

from Tokens import
    Token;

from Keywords import
    KeyLookUp;

$if modula2 then
from Preprocess import
    InitPreprocess, Directive, PreprocessStartFile, PreprocessEndFile;
$end

from Errors import
    Error, ErrorName, ErrorNumber, ErrorUnsigned;

from Symbols import
    currFile, currLine, rootFile, DataType, ModuleKind;


const
    MAXREALDIGPLUSEXP = 26;	(* include .e-NN\0 *)
type
    NumString =  array [1..MAXREALDIGPLUSEXP] of char;

const
    LINESIZE = 1000;
type
    LineIndex  = [1..LINESIZE];
    Line = array LineIndex of char;

    FilePtr = pointer to FileRec;
    FileRec = record
	next		: FilePtr;
	name		: String;
	isCompiledDef   : boolean;
	makeCompiledDef : boolean;
	compiledDefFile : File;
	srcfile		: File;
	lineNumber      : integer;
	saveIndex       : LineIndex;
	line		: Line;
	savechar	: char;
	saveyychar      : integer;
	time		: TimeStamp;
    end;

var
    inFile	    : FilePtr;  (* Top file of file stack *)
    srcfile	    : File;     (* Current file *)
    isCompiledDef   : boolean;  (* Current file is compiled definition module *)
    ch		    : char;	(* current character *)
    lineIndex	    : integer;  (* current position in line *)
    startTokenPtr   : integer;  (* Position of beginning of token *)
    endOfFile       : boolean;
    eofOK	    : boolean;
    lastCommentLine : integer;  (* line number the current comment started on 
				   -1 means no open comment *)
    digitValue      : array char of [0..15];
    isIdentChar     : array char of boolean;
$if pascal then
    includeString   : String;   (* 'include' *)
$end

$IF vms THEN
    (* CED changed this from @external to @global on 4/1/87 ||| *)
    @global
$ELSE
    @external
$END
    yychar	    : integer;  (* yacc's current token *)


$if modula2 then
procedure InitPreprocessError(const message : array of char;
			      const line    : array of char;
			      const position: integer);
    var i : integer;
begin
    Writef(output, 'Bad program argument: %s\n', message);
    Writef(output, '%s\n', line);
    for i := 0 to position-1 do
	if line[i] = '\t' then
	    Writef(output, '\t');
	else
	    Writec(output, ' ');
	end;
    end;
    Writef(output, '^\n');
    SilentError();
end InitPreprocessError;
$end (* modula2 *)

procedure InitScanner(fn : String);
var i : cardinal;
begin
    if not InitFile(fn, MODPROGRAM, 'Cannot find file $ to compile') then
	ExitProgram(101);
    end;
$if modula2 then
    InitPreprocess(standardKeywordFlag, InitPreprocessError);
    for i := 0 to fn^.length - 1 - 4 (* .mod *) do
	AddChar(GetChar(fn, i));
    end;
$else
    for i := 0 to fn^.length - 1 - 2 (* .p *) do
	AddChar(GetChar(fn, i));
    end;
$end
    rootFile := NonHashString();
end InitScanner;

procedure ScanEofOK();
begin
    eofOK := true;
end ScanEofOK;

procedure EndFile();
begin
    if DEBUG and ('f' in debugSet) then
	Writef(output,'EndFile: ');
	WriteString(output, currFile);
	Writec(output, '\n');
    end;
    if inFile # nil then
	(* end of file, pop stack *)
	eofOK := false;
$if modula2 then
	PreprocessEndFile(ErrorNumber);
$end
	Close(inFile^.srcfile);
	if inFile^.makeCompiledDef then
	    Close(inFile^.compiledDefFile);
	end;
	inFile := inFile^.next;
	if inFile = nil then
	    ch := 0C;
	    endOfFile := true;
	else
	    currFile := inFile^.name;
	    currLine := inFile^.lineNumber;
	    srcfile := inFile^.srcfile;
	    isCompiledDef := inFile^.isCompiledDef;
	    ch := inFile^.savechar;
	    lineIndex := inFile^.saveIndex;
	    yychar := inFile^.saveyychar;
	    endOfFile := false;
	end;
    end;
end EndFile;

$if modula2 then
procedure DirectiveError(const message : array of char;
			 const position: integer);
begin
startTokenPtr := position + 1;
Error(message);
end DirectiveError;

$else (* pascal *)

procedure IncludeFile;
    var quoteChar : char;
	fileName  : String;
begin
    while inFile^.line[lineIndex] in CharSet{' ', TABCHAR} do
	inc(lineIndex);
    end;
    startTokenPtr := lineIndex;
    while inFile^.line[lineIndex] in CharSet{'a'..'z'} do
	AddChar(inFile^.line[lineIndex]);
	inc(lineIndex);
    end;
    if NewString() # includeString then
	Error('Fatal error: "include" expected');
	ExitProgram(101);
    end;
    while inFile^.line[lineIndex] in CharSet{' ', TABCHAR} do
	inc(lineIndex);
    end;
    quoteChar := inFile^.line[lineIndex];
    if not (quoteChar in CharSet{'"', "'"}) then
	startTokenPtr := lineIndex;
	Error ('Fatal error: " or '' expected');
	ExitProgram(101);
    end;
    inc(lineIndex);
    while not (inFile^.line[lineIndex] in CharSet{'"', "'", '\n'}) do
	AddChar(inFile^.line[lineIndex]);
	inc(lineIndex);
    end;
    startTokenPtr := lineIndex;
    if inFile^.line[lineIndex] # quoteChar then
	AddChar(quoteChar);
	ErrorName(NonHashString(), 'Missing closing $ for include file name');
    end;
    if not ((inFile^.line[lineIndex-1] in CharSet{'h', 'i'}) and
	    (inFile^.line[lineIndex-2] = '.')) then
	Error('Fatal error: Include file name must end in .h or .i');
	ExitProgram(101);
    end;
    fileName := NonHashString();
    if not InitFile(fileName, MODDEFINITION,
	    'Fatal Error: Cannot find file $ for #include') then
	ExitProgram(101);
    end;
end IncludeFile;
$end (* pascal *)

procedure NextLine;
    var gotALine : boolean;
	skip     : boolean;
begin 
    skip := false;
    loop
	gotALine := Reads(srcfile,inFile^.line);
	inc(inFile^.lineNumber);
	if inFile^.makeCompiledDef then
	    Writec(inFile^.compiledDefFile, CHAR(TKENDOFLINE));
	end;
	currLine := inFile^.lineNumber;
	if not gotALine then
	    endOfFile := true;
	    exit;
	else
$if modula2 then
	    lineIndex := 1;
	    while inFile^.line[lineIndex] in CharSet{' ', TABCHAR} do
		inc(lineIndex);
	    end;

	    if inFile^.line[lineIndex] = '$' then
		Directive(inFile^.line, currLine, DirectiveError, skip);
	    elsif not skip then
		ch := inFile^.line[lineIndex];
		exit;
	    end;
$else (* pascal *)
	    if inFile^.line[1] = '#' then
		lineIndex := 2;
		IncludeFile;
		exit;
	    else
		lineIndex := 1;
		ch := inFile^.line[1];
		exit;
	    end;
$end
	end (* if not gotALine then ... else ... *);
    end (* loop *);
end NextLine;

procedure @inline NextChar;
begin
    inc(lineIndex);
    ch := inFile^.line[lineIndex];
end NextChar;


procedure InitFile(      fn      : String;
	           const kind    : ModuleKind;
		   const message : array of char) : boolean; 
var
    f		    : FilePtr;
    newfile	    : File;
    extension       : FileName;
    fne		    : String;
    fileName	    : FileName;
    makeCompiledDef : boolean;
    compiledDefName : FileName;
begin
    isCompiledDef := false;
    makeCompiledDef := false;
    fne := fn;
    if fne = nil then
	srcfile := input;
    else
$if modula2 then
	case kind of
	| MODDEFINITION :
	    if compiledDefFlag then
		extension := '.d';
		isCompiledDef := true;
	    else
		extension := '.def';
	    end;
	| MODIMPLEMENTATION : 
	    extension := '.mod';
	| else
	    extension := '';
	end;

	AddString(fn);
	fne := NonHashText(extension);
	CopyString(fne,fileName);
	newfile := IOLib.Mod2Open(fileName, fne^.length);
	if (newfile = nil) and (kind = MODDEFINITION) and compiledDefFlag then
	    isCompiledDef := false;
	    makeCompiledDef := true;
	    compiledDefName := fileName;
	    AddString(fn);
	    fne := NonHashText('.def');
	    CopyString(fne, fileName);
	    newfile := IOLib.Mod2Open(fileName, fne^.length);
	end;
	if newfile = nil then
	    ErrorName(fne, message);
	    return false;
	end;
$else (* pascal *)
        CopyString(fne, fileName);
        newfile := IOLib.Mod2Open(fileName, fne^.length);
        if newfile = nil then
	    ErrorName(fne, message);
	    return false;
        end;
$end
    end;

    srcfile := newfile;
    if inFile # nil then
	inFile^.savechar := ch;
	inFile^.saveIndex := lineIndex;
	inFile^.saveyychar := yychar;
	yychar := -1;
    end;
    new(f);
    f^.name := fne;
    f^.srcfile := srcfile;
    f^.lineNumber := 0;
    f^.next := inFile;
    if makeCompiledDef then
	f^.compiledDefFile := Open(compiledDefName, 'w');
	if f^.compiledDefFile = nil then
	    ErrorName(fne, 'Could not open .d file for definition file $');
	    makeCompiledDef := false;
	end;
    end;
    f^.isCompiledDef := isCompiledDef;
    f^.makeCompiledDef := makeCompiledDef;
    if fn = nil then
	f^.time := 0;
    else
	f^.time := IOLib.GetFileTime(srcfile);
    end;
    inFile := f;
    currFile := fne;
    currLine := 0;
    endOfFile := false;
    eofOK := false;
$if modula2 then
    PreprocessStartFile();
$end
    if DEBUG and ('f' in debugSet) then
	Writef(output, 'InitFile: ');
	WriteString(output,fn);
	Writec(output, '\n');
    end;
    if not isCompiledDef then
	NextLine;
    end;
    return true;
end InitFile;

procedure LastModifiedTime() : TimeStamp;
begin
    return inFile^.time;
end LastModifiedTime;

procedure PrevChar;
begin
    dec(lineIndex);
end PrevChar;

(* constants for scanning numbers *)
const
    MAXREALDIG = 18;

var
    maxExp, minExp : integer;
    maxRealStr, minRealStr : NumString;

procedure yylex (): integer;
var
    i, value 	: integer;
    lastCh 	: char;
    strDelim 	: char;
    str 	: String;
    tokenKind	: Token;

(**)procedure HandleNumbers;
var
    i, j, exp, expsign, dotpos : integer;
    strnum : NumString;
    numberTooLong : boolean;
(**)(**)procedure SetTokenInteger;
var
    base : HugeInteger;
    value : HugeInteger;	(* must be >= 32 bits of mantissa *)
    legal : CharSet;
begin
$if modula2 then
    case strnum[i] of
    | 'h', 'H' :
	legal := CharSet{'0'..'9','a'..'f','A'..'F'};
	base := 16.0;
	i := i - 1;
    | 'b', 'B' :
	legal := CharSet{'0'..'7'};
	base := 8.0;
	i := i - 1;
    | 'c', 'C' :
	legal := CharSet{'0'..'7'};
	base := 8.0;
	i := i - 1;
	tokenKind := TKCHARCONST;
    | else
	legal := CharSet{'0'..'9'};
	base := 10.0;
    end;
$else (* pascal *)
    legal := CharSet{'0'..'9'};
    base := 10.0;
$end
    value := 0.0;
    j := 1;
    while j <= i do
	if strnum[j] in legal then
	    value := value * base + longfloat(digitValue[strnum[j]]);
	else
	    Error('Bad digit in number');
	    j := i;
	end;
	inc(j);
    end;
    if tokenKind = TKCHARCONST then
	if value > MAXCHAR then
	    ErrorNumber('Char constant out of range [0..%]', trunc(MAXCHAR));
	    value := MAXCHAR;
	end;
	yylval.con^.kind := DTCHAR;
	yylval.con^.charVal := chr(trunc(value));
    else
	if (value > MAXCARD) then
	    ErrorUnsigned('Number out of range [0..%]', MAXCARDI);
	    value := MAXCARD;
	end;
	yylval.con^.kind := DTCARDINAL;
	yylval.con^.cardVal := value;
    end;
end SetTokenInteger;

begin (*HandleNumbers*)
    tokenKind := TKCARDCONST;
    new(yylval.con);
    numberTooLong := false;
    i := 0;
    dotpos := 0;
    repeat
	if i < MAXREALDIG then
	    i := i + 1;
	else
	    numberTooLong := true;
	    (* Error case sticks last char of number into strnum *)
	end;
	strnum[i] := ch;
	NextChar;
    until not (ch in CharSet{'0'..'9','a'..'f','h','A'..'F','H'});
    if numberTooLong then
	Error('Number too long');
    end;
    if ch # '.' then	(* integer *)
	SetTokenInteger;
    else
	NextChar;
	if ( ch = '.' ) then	(* next token is dotdot *)
	    PrevChar;
	    SetTokenInteger;	(* so it was really an integer *)
	else
	    tokenKind := TKREALCONST;
	    dotpos := i;
	    j := 2;
	    while j <= i do
		if not (strnum[j] in CharSet{'0'..'9'}) then
		    Error('Bad digit in real number');
		    j := i;
		end;
		inc(j);
	    end;
	    if numberTooLong then
		(* Just skip digits *)
		while ch in CharSet{'0'..'9'} do
		    NextChar;
		end;
	    else
		while (ch in CharSet{'0'..'9'}) do
		    if i < MAXREALDIG then
			i := i + 1;
			strnum[i] := ch;
		    else
			numberTooLong := true;
		    end;
		    NextChar;
		end;
		if numberTooLong then
		    Error('Number too long');
		end;
	    end;
	    exp := 0;
	    expsign := 1;
	    if ( ch = 'e' ) or ( ch = 'E' ) then
		(* Get exponent specification *)
		NextChar;
		if ch = '-' then
		    expsign := -1;
		    NextChar;
		elsif ch = '+' then
		    NextChar;
		end;
		if ch in CharSet{'0'..'9'} then
		    repeat
			exp := 10*exp + ord(ch) - ord('0');
			NextChar;
		    until not (ch in CharSet{'0'..'9'});
		else
		    Error("Digit expected after 'E' in real number");
		end;
	    end;
	    exp := expsign*exp + dotpos;
	    strnum[i+1] := 0C;
	    if ((exp-1) > maxExp) or
		    (((exp-1) = maxExp) and (strnum > maxRealStr)) then
		Error('Real number too large');
	    elsif ((exp-1) < minExp) or
		    (((exp-1) = minExp) and (strnum < minRealStr)) then
		Error('Real number too small');
	    else
		exp := exp - i;
		i := i + 1;
		strnum[i] := 'e';
		if exp < 0 then
		    i := i + 1;
		    strnum[i] := '-';
		    exp := -exp;
		end;
		i := i + 1;
		strnum[i] := chr(ord('0') + exp div 10);
		i := i + 1;
		strnum[i] := chr(ord('0') + exp mod 10);
		i := i + 1;
		strnum[i] := chr(0);
	    end;
	    yylval.con^.kind := DTREAL;
		assert(SReadf(strnum, '%F', yylval.con^.realVal) = 1,
		'Compiler constructed a badly formatted real string');
	end (* if real number *);
    end;
end HandleNumbers;

$if modula2 then
(**)procedure GetComment(terminateChar : char);
$if kjncomments then
(*	modified 12/2/86 kjn to accept /* */ as comment delimiters *)
(* this will accept either type of comments nested within a comment
		but the same char used to open each comment must end  
		that comment, i.e. hybrids don't match. *) 
$end

var temp     : char;
var saveLine : integer;

begin (*GetComment*)
    temp := ')';
    saveLine := lastCommentLine;
    lastCommentLine := currLine;
    loop
	if ch = '*' then
	    NextChar;
	    if ch = terminateChar then
		NextChar;
		exit;
	    end;
$if kjncomments then
	elsif ((ch = '(') or (ch = '/')) then
	    if	(ch = '(')
	    then	temp := ')';
	    else	temp := '/';
	    end;
$end
	elsif ch = '(' then
	    NextChar;
	    if ch = '*' then
		NextChar;
		(* nested comment, recur *)
		GetComment(temp);
		if endOfFile then
		    return;
		end;
	    end;
	elsif ch = 0C then
	    NextLine;
	    if endOfFile then
		return;
	    end;
	else
	    NextChar;
	end;
    end (* loop *);
    lastCommentLine := saveLine;
end GetComment; (*GetComment*)

$else (* pascal *)

(**)procedure GetComment(terminateChar : char; nestedStar, nestedBrace : boolean);
var saveLine : integer;
begin (* GetComment*)
    saveLine := lastCommentLine;
    lastCommentLine := currLine;
    loop
	if ch = terminateChar then
	    NextChar;
	    if terminateChar = '*' then
		if ch = ')' then
		    NextChar;
		    exit;
		end;
	    else (* definitely terminate comment *)
	        exit;
	    end;
	elsif ch = '(' then
	    startTokenPtr := lineIndex;
	    NextChar;
	    if ch = '*' then (* nested comment, is it okay? *)
		if nestedStar then
		    Error('(* not allowed in a (* ... *) comment');
		end;
		NextChar;
		GetComment('*', true, nestedBrace);
		if endOfFile then
		    return;
		end;
	    end;
	elsif ch = '{' then (* nested comment, is it okay? *)
	    startTokenPtr := lineIndex;
	    if nestedBrace then 
		Error('{ not allowed in a { ... } comment');
	    end;
	    NextChar;
	    GetComment('}', nestedStar, true);
	    if endOfFile then
		return;
	    end;
	elsif ch = 0C then
	    NextLine;
	    if endOfFile then
		return;
	    end;
	else
	    NextChar;
	end;
    end (* loop *);
    lastCommentLine := saveLine;
end GetComment;

$end (* pascal *)

procedure CommentFlags();
(* Handle $s, $C, $w *)
    procedure SetFlag(var flag : cardinal);
    begin
	NextChar;
	if (ch = '+') then
	    flag := 2*flag + 1;
	    NextChar;
	elsif (ch = '-') then
	    flag := 2*flag;
	    NextChar;
	elsif (ch = '^') then
	    flag := flag div 2;
	end;
    end SetFlag;

$if pascal then
    procedure SetValueFlag(var flag : cardinal; const default : cardinal);
   begin
	NextChar();
	if ch in CharSet{'0'..'9'} then
	    flag := 0;
	    repeat
		flag := flag*10 + ord(ch) - ord('0');
		NextChar();
	    until not (ch in CharSet{'0'..'9'});
	else
	    flag := default;
	end;
    end SetValueFlag;
$end (* pascal *)

begin (* CommentFlags *)
    if ch # '$' then
	return;
    end;
    repeat
	NextChar;
	case ch of
$if pascal then
	| 's' :
	    SetFlag(standardPascalFlag);    (* non-standard constructs *)
	| 'w' :
	    SetFlag(warningPascalFlag);     (* suppress warnings *)
	| 'b' :
	    SetValueFlag(bufferFlag, 2);    (* output buffering *)
$end
	| 'C' :
	    SetFlag(genCheckFlag);
	| else
	    (* no action *)
	end;
    until ch # ',';
end CommentFlags;

    (* Variables for reading from compiled definition files *)
    var bytes  : integer;
	length : @size 16 integer;
	slength: @size 8 [0..255];
	hash   : integer;
	instr  : array [0..1023] of char;
	kludge : CHAR;
begin (*yylex*)
    if not endOfFile and isCompiledDef then
	(* ||| Fix baseVar setting in CheckFuncProc *)
	loop
	    if Readc(srcfile, kludge) = 0 then end;
	    tokenKind := Token(kludge);
	    case tokenKind of
	    | TKENDOFFILE :
		endOfFile := true;
		return ord(TKENDOFFILE);

	    | TKENDOFLINE :
		inc(inFile^.lineNumber);
		currLine := inFile^.lineNumber;
		(* Go through loop again *)

	    | TKIDENT :
		bytes := Readb(srcfile, length, BYTESIZE(length));
		bytes := Readb(srcfile, instr, length);
		instr[length] := 0C;
		bytes := Readb(srcfile, hash, BYTESIZE(hash));
		yylval.str := KnownText(instr, length, hash);
		return ord(TKIDENT);

	    | TKSMALLIDENT :
		bytes := Readb(srcfile, slength, BYTESIZE(slength));
		bytes := Readb(srcfile, instr, slength);
		instr[slength] := 0C;
		bytes := Readb(srcfile, hash, BYTESIZE(hash));
		yylval.str := KnownText(instr, slength, hash);
		return ord(TKIDENT);

	    | TKCARDCONST :
		new(yylval.con);
		yylval.con^.kind := DTCARDINAL;
		bytes := Readb(srcfile, yylval.con^.cardVal,
			BYTESIZE(yylval.con^.cardVal));
		return ord(TKCARDCONST);

	    | TKREALCONST :
		new(yylval.con);
		yylval.con^.kind := DTREAL;
		bytes := Readb(srcfile, yylval.con^.realVal,
			BYTESIZE(yylval.con^.realVal));
		return ord(TKREALCONST);

	    | TKCHARCONST :
		new(yylval.con);
		yylval.con^.kind := DTCHAR;
		bytes := Readc(srcfile, yylval.con^.charVal);
		return ord(TKCHARCONST);

	    | TKSTRCONST :
		new(yylval.con);
		yylval.con^.kind := DTSTRING;
		bytes := Readb(srcfile, length, BYTESIZE(length));
		bytes := Readb(srcfile, instr, length);
		instr[length] := 0C;
		yylval.con^.strVal := KnownNonHashText(instr, length);
		return ord(TKSTRCONST);

	    | TKSMALLSTRCONST :
		new(yylval.con);
		yylval.con^.kind := DTSTRING;
		bytes := Readb(srcfile, slength, BYTESIZE(slength));
		bytes := Readb(srcfile, instr, slength);
		instr[slength] := 0C;
		yylval.con^.strVal := KnownNonHashText(instr, slength);
		return ord(TKSTRCONST);

	    | else
		return ord(tokenKind);

	    end; (*case*)
	end (* loop *);
    end;

    tokenKind := TKNULL;
    repeat
	if endOfFile then
	    if lastCommentLine >= 0 then
		ErrorNumber(
		    'Fatal error: comment at line % unclosed at end of file', 
		    lastCommentLine);
		ExitProgram(101);
	    end;
$if modula2 then
	    if not eofOK and (inFile # nil) then
		ErrorName(currFile, 'Fatal Error: unexpected end of file $');
		ExitProgram(101);
	    end;
	    tokenKind := TKENDOFFILE;
$else
	    EndFile;
	    if endOfFile then
		tokenKind := TKENDOFFILE;
	    end;
$end
	else
	    startTokenPtr := lineIndex;
	    case (ch) of
	    | 0C, '\n' :
		NextLine;

	    | ' ', FORMFEEDCHAR, TABCHAR:
		NextChar;

$if modula2 then
	    | 'a'..'z', 'A'..'Z', '@', '_':
$else (* pascal *)
	    | 'a'..'z', 'A'..'Z':
$end
		repeat
$if modula2 then
		    if upperCaseFlag then
			ch := CAP(ch);
		    end;
$end
		    AddChar(ch);
		    NextChar;
		until not isIdentChar[ch];
		yylval.str := NewString();
		tokenKind := KeyLookUp(yylval.str);  (*see if keyword or ident*)

	    | '0'..'9':
		HandleNumbers;		(*handles ints and reals*)

	    | '<' :
		NextChar;
		if (ch = '>') then
		    tokenKind := TKNOTEQUAL;
		    NextChar;
		elsif (ch = '=') then
		    tokenKind := TKLSEQUAL;
		    NextChar;
		else
		    tokenKind := TKLESS;
		end;
		
	    | '~' :
		tokenKind := TKNOT;
	        NextChar;
	    
	    | '=' :
	        tokenKind := TKEQUALS;
    	        NextChar;
    	
	    | '>' :
	        NextChar;
	        if (ch = '=') then
	    	tokenKind := TKGREQUAL;
		    NextChar;
		else
	    	    tokenKind := TKGREATER;
		end;
		
	    | ':' :
		NextChar;
		if (ch = '=') then
		    tokenKind := TKASSIGN;
		    NextChar;
		else
		    tokenKind := TKCOLON;
		end;
		
	    | '+' :
		tokenKind := TKPLUS;
		NextChar;
		
	    | '-' :
		tokenKind := TKMINUS;
		NextChar;
		
	    | '*' :
		NextChar;
$if kjncomments then
		if ((ch = ')') or (ch = '/')) then
$else
		if (ch = ')') then
$end
		    NextChar;
		    Error('Close comment with no corresponding open comment');
		else
		    tokenKind := TKASTERISK;
		end;

	    | ';' :
		tokenKind := TKSEMICOLON;
		NextChar;
		
	    | ')' :
		tokenKind := TKRPAREN;
		NextChar;
		
	    | '(' :
		NextChar;
		if (ch = '*') then		(*start comment*)
		    NextChar;
		    CommentFlags();
$if modula2 then
		    GetComment(')');
$else
		    GetComment('*', true, false);
$end
		else
		    tokenKind := TKLPAREN;
		end;
		
	    | '/' :
$if kjncomments then
		NextChar;
		if (ch = '*') then		(*start comment*)
		    NextChar;
		    CommentFlags();
		    GetComment('/');
		else tokenKind := TKSLASH;
		end;
$else
		tokenKind := TKSLASH;
		NextChar;
$end
		
	    | '{' :
		NextChar;
$if modula2 then
		tokenKind := TKLBRACE;
$else
		CommentFlags();
		GetComment('}', false, true);
$end

$if modula2 then
	    | '}' :
		tokenKind := TKRBRACE;
		NextChar;
		
	    | '|' :
		tokenKind := TKBAR;
		NextChar;

$else (* pascal *)
	    | '}' :
		NextChar;
		Error('Close comment with no corresponding open comment');
$end
		
	    | '^' :
		tokenKind := TKUPARROW;
		NextChar;
		
	    | '.' :
		NextChar;
		if (ch = '.') then
		    tokenKind := TKDOTDOT;
		    NextChar;
		else
		    tokenKind := TKDOT;
		end;
		
	    | '[' :
		tokenKind := TKLBRACKET;
		NextChar;
		
	    | ']' :
		tokenKind := TKRBRACKET;
		NextChar;
		
	    | '&' :
		tokenKind := TKAMPERSAND;
		NextChar;
		
	    | ',' :
		tokenKind := TKCOMMA;
		NextChar;
		
$if modula2 then
	    | '#' :
		tokenKind := TKSHARP;
		NextChar;
$end
		
$if modula2 then
	    | "'", '"' :		(*string*)
$else
	    | "'", '"', '#':
		if ch # "'" then
		    Error("Character/string delimiter is '");
		end;
$end
		strDelim := ch;		(*save apostr. or quotes*)
		NextChar;
		loop
		    if ch = strDelim then
			NextChar;
			if ch = strDelim then
			    (* doubled delimiter = delimiter character *)
			    AddChar(ch);
			    NextChar;
			    if standardStringFlag then
				if strDelim = "'" then
				    Error("'' for ' in string not standard");
				else
				    Error('"" for " in string not standard');
				end;
			    end;
			else
			    (* ||| Normally means end of string, but could look
			       for things like 'Don't like this' *)
			    exit;
			end;
$if modula2 then
		    elsif not standardStringFlag and (ch = '\\') then
			NextChar;
			case ch of
			| 'n' :
			    AddChar(LINEFEEDCHAR);	(* newline *)
			| 't' :
			    AddChar(TABCHAR);		(* tab *)
			| 'v' :
			    AddChar(VERTICALTABCHAR);
			| 'r' :
			    AddChar(RETURNCHAR);	(* return *)
			| 'f' :
			    AddChar(FORMFEEDCHAR);	(* form feed *)
			| 'b' :
			    AddChar(BACKSPACECHAR);     (* backspace *)
			| '0'..'9' :
			    i := 1;
			    value := 0;
			    while (i <= 3) and (ch in CharSet{'0'..'9'}) do
				value := value*8 + ord(ch)-ord('0');
				NextChar;
				i := i + 1;
			    end;
			    AddChar(chr(value));
			    PrevChar;	(* don't consume extra char *)
			| 0C, '\n' :	    
			    (* continued next line *)
			    NextLine;
			    PrevChar;
			| else
			    AddChar(ch);		(* etc. ', ", \ *)
			end;
			NextChar;
$end
		    elsif ch = 0C then
			Error('String crosses line boundary');
			NextLine;
			exit;
		    else
			(* normal character *)
			AddChar(ch);
			NextChar;
		    end;
		end (* loop *);
		str := NonHashString();
		if str^.length = 1 then
		    tokenKind := TKCHARCONST;
		    new(yylval.con);
		    yylval.con^.kind := DTCHAR;
		    yylval.con^.charVal := GetChar(str,0);
		else
		    tokenKind := TKSTRCONST;
		    new(yylval.con);
		    yylval.con^.kind := DTSTRING;
		    yylval.con^.strVal := str;
		end; (*if*)
		
	    | else (* all other cases *) 
		AddChar(ch);
		ErrorName(NonHashString(),'Invalid character "$"');
		NextChar;
 	    end; (*case*)
	end (* if *);
    until tokenKind # TKNULL;

(* |||
	Writef(output, 'yylex: %n ', tokenKind);
	case tokenKind of
	| TKIDENT :
	    Writef(output, 'ident: ');
	    WriteString(output,yylval.str);
	    
	| TKCARDCONST :
	    Writef(output, 'int: %1.0F', yylval.con^.cardVal);
	    
	| TKREALCONST :
	    Writef(output, 'real: %1.1G', yylval.con^.realVal);
	    
	| TKCHARCONST :
	    Writef(output, 'char: %c', yylval.con^.charVal);
	    
	| TKSTRCONST :
	    Writef(output, 'string: ');
	    WriteString(output,yylval.con^.strVal);
	| else (* nothing *)
	end; (*case*)
	Writec(output, '\n');
*)

    if (inFile # nil) and inFile^.makeCompiledDef then
	case tokenKind of
	| TKIDENT :
	    if yylval.str^.length < 256 then
		Writec(inFile^.compiledDefFile, CHAR(TKSMALLIDENT));
		Writeb(inFile^.compiledDefFile, yylval.str^.length,
			BYTESIZE(slength));
	    else
		Writec(inFile^.compiledDefFile, CHAR(TKIDENT));
		Writeb(inFile^.compiledDefFile, yylval.str^.length,
			BYTESIZE(length));
	    end;
	    WriteString(inFile^.compiledDefFile, yylval.str);
	    Writeb(inFile^.compiledDefFile, yylval.str^.hash,
		    BYTESIZE(yylval.str^.hash));
	    
	| TKCARDCONST :
	    Writec(inFile^.compiledDefFile, CHAR(TKCARDCONST));
	    Writeb(inFile^.compiledDefFile, yylval.con^.cardVal,
		    BYTESIZE(yylval.con^.cardVal));
	    
	| TKREALCONST :
	    Writec(inFile^.compiledDefFile, CHAR(TKREALCONST));
	    Writeb(inFile^.compiledDefFile, yylval.con^.realVal,
		    BYTESIZE(yylval.con^.realVal));
	    
	| TKCHARCONST :
	    Writec(inFile^.compiledDefFile, CHAR(TKCHARCONST));
	    Writec(inFile^.compiledDefFile, yylval.con^.charVal);
	    
	| TKSTRCONST :
	    if yylval.con^.strVal^.length < 256 then
		Writec(inFile^.compiledDefFile, CHAR(TKSMALLSTRCONST));
		Writeb(inFile^.compiledDefFile, yylval.con^.strVal^.length,
			BYTESIZE(slength));
	    else
		Writec(inFile^.compiledDefFile, CHAR(TKSTRCONST));
		Writeb(inFile^.compiledDefFile, yylval.con^.strVal^.length,
			BYTESIZE(length));
	    end;
	    WriteString(inFile^.compiledDefFile, yylval.con^.strVal);

	| else
	    Writec(inFile^.compiledDefFile, CHAR(tokenKind));
	end; (*case*)
    end;

    return ord(tokenKind);
end yylex;

procedure LineWrite;
var
    i    : integer;
    f    : File;
    fs   : String;
    fn   : FileName;
begin
    if inFile # nil then
	(* Get line out of .def if we are parsing .d *)
	if isCompiledDef then
	    AddString(currFile);
	    fs := NonHashText('ef');
	    CopyString(fs, fn);
	    f := IOLib.Mod2Open(fn, fs^.length);
	    if f = nil then
		return;
	    end;
	    if inFile^.time < IOLib.GetFileTime(f) then
		(* .def has been changed since .d created *)
		Writef(output, '<<< out-of-date .d file >>>\n');
		Close(f);
		return;
	    end;
	    for i := 1 to currLine do
		if Reads(f, inFile^.line) then end;
	    end;
	    Close(f);
	end;
	i := 1;
	loop
	    case inFile^.line[i] of
	    | TABCHAR, ' '..'~' :
		Writec(output, inFile^.line[i]);
	    | LINEFEEDCHAR, 0C :
		exit;
	    | else
		Writec(output, '?');
	    end;
	    inc(i);
	end;
	Writec(output, '\n');
	if isCompiledDef then
	    return;
	end;
	for i := 1 to startTokenPtr-1 do
	    if inFile^.line[i] = TABCHAR then
		Writec(output, TABCHAR);
	    else
		Writec(output, ' ');
	    end;
	end;
	Writef(output, '^\n');
    end;
end LineWrite;

begin (* Scanner *)
    for ch := first(char) to last(char) do
	case ch of
	| '0'..'9' :
	    digitValue[ch] := ord(ch) - ord('0');
	| 'a'..'f' :
	    digitValue[ch] := ord(ch) - ord('a') + 10;
	| 'A'..'F' :
	    digitValue[ch] := ord(ch) - ord('A') + 10;
	| else
	    digitValue[ch] := 0;
	end;
	isIdentChar[ch] := ch in CharSet{'a'..'z', 'A'..'Z', '0'..'9','_','$'};
    end;

    ch := 0C;
    inFile := nil;
    lastCommentLine := -1;

    startTokenPtr := 0;
    
    case host of
    | TARGETVAX :
	maxExp :=  38;
	maxRealStr := '17014118346046923';
	minExp := -39;
	minRealStr := '29387358770557188';
    | TARGETTITAN, TARGETTITANM :
	maxExp := 307;
	(* ||| Get exact number when conversions work on Titan *)
	maxRealStr := '900000000000000000';
	minExp := -309;
	minRealStr := '550000000000000000';
    end;
$if pascal then
    includeString := NewText('include');
$end

end Scanner.
