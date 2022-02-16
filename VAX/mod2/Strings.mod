implementation module Strings;

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
    Writef, Writec, Writeb, output;

from MemLib import
    ALLOCATE;

const
    STRINGHASHSIZE = 1357;
type
    HashIndex   = [0..STRINGHASHSIZE];
var
    stringTable		    : StringBlock;
    stringTableIndex,			    (* Next available position *)
    saveStringTableIndex    : StringIndex;  (* Start of current text *)
    stringHash		    : array HashIndex of String;

procedure ExpandStringTable;
(* Create a new StringBlock, and copy the text we have so far into it *)
var
    newBlock : StringBlock;
    i : StringIndex;
begin
    new(newBlock);
    (* copy in any partial text in old table *)
    stringTableIndex := stringTableIndex - saveStringTableIndex;
    for i := 0 to stringTableIndex-1 do
	newBlock^.s[i] := stringTable^.s[saveStringTableIndex+i];
    end;
    newBlock^.next := stringTable;
    stringTable := newBlock;
    saveStringTableIndex := 0;
end ExpandStringTable;


procedure NonHashString() : String;
(* Just return a string pointer, and don't worry about hashing stuff *)
var
    newStr : String;
begin
    new(newStr);
    newStr^.next := nil;
    newStr^.block := stringTable;
    newStr^.index := saveStringTableIndex;
    newStr^.length := stringTableIndex - saveStringTableIndex;
    newStr^.hash := 0;
    saveStringTableIndex := stringTableIndex;
    return newStr;
end NonHashString;


procedure NewString (): String;
(* Calculate the hash value for the new string, and see if the text
   already has been entered in the stringTable.  If so, just return the
   existing string pointer and reclaim the space for the text;  if not,
   create a new string pointer. *)
var
    newStr, prevStr : String;
    newStrBlock : StringBlock;
    i, length, hash : integer;
    newStrIndex : StringIndex;
    hashIndex : HashIndex;
begin
    hash := 0;

    for i := saveStringTableIndex to stringTableIndex-1 do
	hash := (hash * 7 + ord(cap(stringTable^.s[i])));
    end;

    hash := abs(hash);
    length := stringTableIndex - saveStringTableIndex;
    hashIndex := hash mod STRINGHASHSIZE;
    newStr := stringHash[hashIndex];
    prevStr := nil;

    while newStr # nil do
	if (newStr^.hash = hash) and (newStr^.length = length) then
	    i := 0;
	    newStrBlock := newStr^.block;
	    newStrIndex := newStr^.index;
	    while (i < length) and
		(newStrBlock^.s[newStrIndex+i] 
		    = stringTable^.s[saveStringTableIndex+i]) do
		i := i + 1;
	    end;
	    if i = length then (* matched all the way, use existing entry *)
		(* free up space taken by the "new" string *)
		stringTableIndex := saveStringTableIndex;
		(* rearrange list to put newStr at front *)
		if prevStr # nil then
		    prevStr^.next := newStr^.next;
		    newStr^.next := stringHash[hashIndex];
		    stringHash[hashIndex] := newStr;
		end;
		return newStr;
	    end;
	end;
	prevStr := newStr;
	newStr := newStr^.next;
    end (* while *);

    (* String is not already in table *)
    new(newStr);
    newStr^.next := stringHash[hashIndex];
    stringHash[hashIndex] := newStr;
    newStr^.block := stringTable;
    newStr^.index := saveStringTableIndex;
    newStr^.length := length;
    newStr^.hash := hash;
    saveStringTableIndex := stringTableIndex;
    return newStr;
end NewString;

(* ||| length should be removed, use array slices instead *)
procedure KnownText(const s      : array of char;
		    const length : integer;
		    const hash   : integer)
	: String;
(* Hash value and length of the string are known.  See if the text
   already has been entered in the stringTable.  If so, just return the
   existing string pointer;  if not, store text in stringTable and then
   create a new string pointer. *)
var
    newStr, prevStr : String;
    newStrBlock     : StringBlock;
    i		    : integer;
    newStrIndex     : StringIndex;
    hashIndex       : HashIndex;
begin
    hashIndex := hash mod STRINGHASHSIZE;
    newStr := stringHash[hashIndex];
    prevStr := nil;

    while newStr # nil do
	if (newStr^.hash = hash) and (newStr^.length = length) then
	    i := 0;
	    newStrBlock := newStr^.block;
	    newStrIndex := newStr^.index;
	    while (i < length) and (newStrBlock^.s[newStrIndex+i] = s[i]) do
		i := i + 1;
	    end;
	    if i = length then (* matched all the way, use existing entry *)
		(* rearrange list to put newStr at front *)
		if prevStr # nil then
		    prevStr^.next := newStr^.next;
		    newStr^.next := stringHash[hashIndex];
		    stringHash[hashIndex] := newStr;
		end;
		return newStr;
	    end;
	end;
	prevStr := newStr;
	newStr := newStr^.next;
    end (* while *);

    (* String is not already in table, copy text in... *)
    if stringTableIndex + length > STRINGBLOCKSIZE then
	ExpandStringTable;
    end;
    for i := 0 to length-1 do
	stringTable^.s[stringTableIndex+i] := s[i];
    end;
    inc(stringTableIndex, length);

    (* ... and enter into hash bucket *)
    new(newStr);
    newStr^.next := stringHash[hashIndex];
    stringHash[hashIndex] := newStr;
    newStr^.block := stringTable;
    newStr^.index := saveStringTableIndex;
    newStr^.length := length;
    newStr^.hash := hash;
    saveStringTableIndex := stringTableIndex;
    return newStr;
end KnownText;

var
    errorGetChar : ErrorProc;

procedure GetChar(s : String; charNum : StringLength) : char;
begin
    if charNum >= s^.length then
	errorGetChar('GetChar: String table error?');
    end;
    return s^.block^.s[s^.index+charNum];
end GetChar;

procedure AddChar(const c : char);
begin
    if stringTableIndex >= STRINGBLOCKSIZE then
	ExpandStringTable;
    end;
    stringTable^.s[stringTableIndex] := c;
    stringTableIndex := stringTableIndex + 1;
end AddChar;

procedure AddText(const s : array of char);
var
    i : integer;
begin
    i := 0;
    while (s[i] # 0C) do
	if stringTableIndex >= STRINGBLOCKSIZE then
	    ExpandStringTable;
	end;
	stringTable^.s[stringTableIndex] := s[i];
	stringTableIndex := stringTableIndex + 1;
	i := i + 1;
    end;
end AddText;

procedure NonHashText(const s : array of char) : String; 
var
    i : integer;
begin
    i := 0;
    while (s[i] # 0C) do
	if stringTableIndex >= STRINGBLOCKSIZE then
	    ExpandStringTable;
	end;
	stringTable^.s[stringTableIndex] := s[i];
	stringTableIndex := stringTableIndex + 1;
	i := i + 1;
    end;
    return NonHashString();
end NonHashText;

(* ||| Use slices, get rid of length *)
procedure KnownNonHashText(const s : array of char; const length : integer)
    : String;
var
    i : integer;
begin
    if stringTableIndex + length > STRINGBLOCKSIZE then
	ExpandStringTable;
    end;
    for i := 0 to length-1 do
	stringTable^.s[stringTableIndex+i] := s[i];
    end;
    inc(stringTableIndex, length);
    return NonHashString();
end KnownNonHashText;

procedure NewText(const s : array of char) : String;
var
    i : integer;
begin
    i := 0;
    while (s[i] # 0C) do
	if stringTableIndex >= STRINGBLOCKSIZE then
	    ExpandStringTable;
	end;
	stringTable^.s[stringTableIndex] := s[i];
	stringTableIndex := stringTableIndex + 1;
	i := i + 1;
    end;
    return NewString();
end NewText;

procedure AddString(const str : String);
var
    i : integer;
begin
    if (stringTableIndex + str^.length) >= STRINGBLOCKSIZE then
	ExpandStringTable;
    end;
    for i := 0 to str^.length-1 do
	stringTable^.s[stringTableIndex] := str^.block^.s[str^.index+i];
	stringTableIndex := stringTableIndex + 1;
    end;
end AddString;

procedure WriteString(f : File; const s : String);
(* Much more common than WriteConstString.  No special-cases chars. *)
$IF vms THEN
var
    index, i : integer;
$END

begin
    if s # nil then
$IF vms THEN
	index := s^.index;
	for i := 0 to s^.length-1 do
	    Writec(f, s^.block^.s[index+i]);
	end;
(* |||  The above used to be commented out and this was put in its place.
	I (CED) switched it back on 5/14/87 because Writeb on VMS
	doesn't work the same way as it does on ULTRIX (or should
	I be talking about fwrite more specifically).
	fwrite on VMS was putting out one character per line
	(following each character with '\n' - treating each character as
	one RMS record with an implied CR)
	in some cases, whereas fputc always works properly under ULTRIX or VMS.
*)
$ELSE
	Writeb(f, s^.block^.s[s^.index], s^.length);
$END
    end;
end WriteString;

procedure WriteStringConst(f : File; const s : String);
(* Used by GenPC, GenT.  Writes out a string that is being enclosed
   in quotes to the p-code or Mahler file. *)
var
    i : integer;
begin
    if s # nil then
	for i := 0 to s^.length-1 do
	    case s^.block^.s[s^.index+i] of
	    | '"' :
		Writef(f, '\\"');
	    | '\\' :
		Writef(f, '\\\\');
	    | 0C..37C, 177C..377C :
		Writef(f, '\\%c%c%c',  
		   chr(ord('0') + (ord(s^.block^.s[s^.index+i]) div 64) mod 8),
		    chr(ord('0') + (ord(s^.block^.s[s^.index+i]) div 8) mod 8),
		    chr(ord('0') + ord(s^.block^.s[s^.index+i]) mod 8));
	    | else
		Writec(f, s^.block^.s[s^.index+i]);
	    end;
	end;
    end;
end WriteStringConst;


procedure CopyString(const str : String; var arr : array of char);
var
    i : cardinal;
begin
    if str = nil then
	arr[0] := 0C;
    else
	for i := 0 to str^.length-1 do
	    arr[i] := str^.block^.s[str^.index+i];
	end;
	arr[str^.length] := 0C;
    end;
end CopyString;

procedure DumpStrings();
var
    i : HashIndex;
    str : String;
begin
    for i := 0 to STRINGHASHSIZE do
	str := stringHash[i];
	while str # nil do
	    Writef(output, 'hv=%d:', i);
	    WriteString(output,str);
	    Writec(output, '\n');
	    str := str^.next;
	end;
    end;
end DumpStrings;


procedure EqualAnyCase(const left, right : String) : boolean;
var
    i : integer;
begin
    if (left^.hash # right^.hash) or (left^.length # right^.length) then
	return false;
    end;
    i := 0;
    while (i<left^.length) do
	if (CAP(left^.block^.s[left^.index+i]) # 
		CAP(right^.block^.s[right^.index+i])) then
	    return false;
	end;
	i := i + 1;
    end;
    return true;
end EqualAnyCase;

procedure Compare(const left, right : String) : integer;
(* Lexicographic compare.  Return <0 if less, =0 if equal, >0 if greater *)
   var i, minLength : cardinal;
       result : integer;
begin
    if left^.length < right^.length then
	minLength := left^.length;
    else
	minLength := right^.length;
    end;
    i := 0;
    loop
	if i = minLength then   (* equal so far, compare lengths *)
	    return left^.length - right^.length;
	else
	    result := ord(left^.block^.s[left^.index+i]) 
		    - ord(right^.block^.s[right^.index+i]);
	    if result = 0 then
		inc(i);
	    else
		return result;
	    end;
	end;
    end;
end Compare;


procedure InitStrings(const error : ErrorProc);
begin
    errorGetChar := error;
end InitStrings;

procedure StringError(const message : array of char);
begin
    Writef(output, '%s\n', message);
end StringError;

var sh : HashIndex;
begin
    errorGetChar := StringError;
    for sh := 0 to STRINGHASHSIZE do
	stringHash[sh] := nil;
    end;
    stringTableIndex := 0;
    saveStringTableIndex := 0;
    stringTable := nil;
    ExpandStringTable;
end Strings.
