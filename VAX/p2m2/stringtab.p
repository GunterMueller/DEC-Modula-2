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

#include "stringtab.h"
#include "tokens.h"
#include "error.h"
#include "globals.h"
#include "util.h"

var
    stringTab : StringBlock;
    stringTabIndex, saveStringTabIndex : StringIndex;
    stringHash : array [0..STRINGHASHSIZE] of StringEntry;

procedure ExpandStringTable;
var
    newBlock : StringBlock;
    newTabIndex, i, currStringSize : StringIndex;
begin
    new(newBlock);
    newTabIndex := 0;
    { copy in any partial string in old table }
    currStringSize := stringTabIndex - saveStringTabIndex;
    for i := 0 to currStringSize-1 do begin
	newBlock^.s[newTabIndex] := stringTab^.s[saveStringTabIndex+i];
	newTabIndex := newTabIndex + 1;
    end;
    newBlock^.next := stringTab;
    stringTab := newBlock;
    stringTabIndex := newTabIndex;
    saveStringTabIndex := 0;
end;

procedure InitStringTable;
var sh : integer;
begin
    for sh := 0 to STRINGHASHSIZE do begin
	stringHash[sh] := nil;
    end;
    stringTabIndex := 0;
    saveStringTabIndex := 0;
    stringTab := nil;
    ExpandStringTable;
end;

function NewString{: String; external};
var
    newStr : StringEntry;
    i, length, hash : integer;
    found : boolean;
    ordc : integer;
begin
    hash := 0;
    for i := saveStringTabIndex to stringTabIndex-1 do begin
	if stringTab^.s[i] in ['A'..'Z'] then begin
	    ordc := ord(stringTab^.s[i]) - ord('A') + ord('a');
	end else begin
	    ordc := ord(stringTab^.s[i]);
	end;
	hash := (hash * 12345 + ordc) mod 111111;
    end;
    length := stringTabIndex - saveStringTabIndex;
    newStr := stringHash[hash mod STRINGHASHSIZE];
    found := false;
    while (newStr <> nil) and not found do begin
	if newStr^.hash = hash then begin
	    if newStr^.length = length then begin
		found := true;
		i := 0;
		while found and (i < length) do begin
		    if newStr^.block^.s[newStr^.index+i]
			    <> stringTab^.s[saveStringTabIndex+i]
		    then begin
			found := false;
		    end;
		    i := i + 1;
		end;
	    end;
	end;
	if not found then begin
	    newStr := newStr^.next;
	end;
    end;
    if not found then begin
	new(newStr);
	newStr^.next := stringHash[hash mod STRINGHASHSIZE];
	stringHash[hash mod STRINGHASHSIZE] := newStr;
	newStr^.block := stringTab;
	newStr^.index := saveStringTabIndex;
	newStr^.length := length;
	newStr^.hash := hash;
	saveStringTabIndex := stringTabIndex;
    end else begin
	stringTabIndex := saveStringTabIndex;
    end;
    NewString := newStr;
end;

function GetChar{(s : String; charNum : StringLength) : char};
begin
    if charNum >= s^.length then begin
	Error('GetChar: String table error?');
	halt;
    end;
    GetChar := s^.block^.s[s^.index+charNum];
end;

procedure AddChar{(c : char)};
begin
    if stringTabIndex >= STRINGBLOCKSIZE then begin
	ExpandStringTable;
    end;
    stringTab^.s[stringTabIndex] := c;
    stringTabIndex := stringTabIndex + 1;
end;

procedure AddText{(s : ShortString)};
var
    i : integer;
begin
    i := 1;
    while s[i] <> ' ' do begin
	if stringTabIndex >= STRINGBLOCKSIZE then begin
	    ExpandStringTable;
	end;
	stringTab^.s[stringTabIndex] := s[i];
	stringTabIndex := stringTabIndex + 1;
	i := i + 1;
    end;
end;

procedure AddString{(str : String)};
var
    i : integer;
begin
    for i := 0 to str^.length-1 do begin
	if stringTabIndex >= STRINGBLOCKSIZE then begin
	    ExpandStringTable;
	end;
	stringTab^.s[stringTabIndex] := str^.block^.s[str^.index+i];
	stringTabIndex := stringTabIndex + 1;
    end;
end;

procedure OutString{(s : String); external};
var
    i : integer;
begin
    if s <> nil then begin
	for i := 0 to s^.length-1 do begin
	    OutChar(s^.block^.s[s^.index+i]);
	end;
    end;
end;

procedure OutStringConst{(s : String); external};
var
    i, ic : integer;
begin
    OutChar('''');
    if s <> nil then begin
	for i := 0 to s^.length-1 do begin
	    if s^.block^.s[s^.index+i] = '''' then begin
		OutChar(''''); OutChar('''');
	    end else if s^.block^.s[s^.index+i] = '\' then begin
		OutChar('\'); OutChar('\');
	    end else if s^.block^.s[s^.index+i] in [' '..'~'] then begin
		OutChar(s^.block^.s[s^.index+i]);
	    end else if s^.block^.s[s^.index+i] = chr(9) then begin
		OutChar('\'); OutChar('t');
	    end else if s^.block^.s[s^.index+i] = chr(10) then begin
		OutChar('\'); OutChar('n');
	    end else begin
		ic := ord(s^.block^.s[s^.index+i]);
		{ ord of char in octal, the hard way }
		OutChar('\'); OutChar(chr(ord('0')+i div 64));
		OutChar(chr(ord('0')+(i div 8) mod 8));
		OutChar(chr(ord('0')+i mod 8));
	    end;
	end;
    end;
    OutChar('''');
end;

procedure WriteString{(var f : text; s : String); external};
var
    i : integer;
begin
    if s <> nil then begin
	for i := 0 to s^.length-1 do begin
	    write(f,s^.block^.s[s^.index+i]);
	end;
    end;
end;

procedure WriteStringConst{(var f : text; s : String); external};
var
    i : integer;
begin
    write(f,'''');
    if s <> nil then begin
	for i := 0 to s^.length-1 do begin
	    if s^.block^.s[s^.index+i] = '''' then begin
		write(f,'''''');
	    end else if s^.block^.s[s^.index+i] = '\' then begin
		write(f,'\\');
	    end else if s^.block^.s[s^.index+i] in [' '..'~'] then begin
		write(f,s^.block^.s[s^.index+i]);
	    end else if s^.block^.s[s^.index+i] = chr(9) then begin
		write(f,'\t');
	    end else if s^.block^.s[s^.index+i] = chr(10) then begin
		write(f,'\n');
	    end else begin
		write(f,'\',ord(s^.block^.s[s^.index+i]):1 oct);
	    end;
	end;
    end;
    write(f,'''');
end;

procedure StringToFileName{(s : String; var fn : FileName); external};
var
    i : integer;
begin
    fn := '  ';
    for i := 0 to s^.length-1 do begin
	fn[i] := s^.block^.s[s^.index+i];
    end;
end;

procedure DumpStringTab;
var
    i : integer;
    str : StringEntry;
begin
    for i := 0 to STRINGHASHSIZE do begin
	str := stringHash[i];
	while str <> nil do begin
	    write(output,'hv=',i:0,':');
	    WriteString(output,str);
	    writeln(output);
	    str := str^.next;
	end;
    end;
end;

function EqualAnyCase{(a,b : String) : boolean};
var
    equal : boolean;
    i : integer;
    ac, bc : char;
begin
    equal := (a^.hash = b^.hash) and (a^.length = b^.length);
    if equal then begin
	i := 0;
	while equal and (i<a^.length) do begin
	    ac := a^.block^.s[a^.index+i];
	    if ac in ['a'..'z'] then begin
		ac := chr(ord(ac)-ord('a')+ord('A'));
	    end;
	    bc := b^.block^.s[b^.index+i];
	    if bc in ['a'..'z'] then begin
		bc := chr(ord(bc)-ord('a')+ord('A'));
	    end;
	    equal := ac = bc;
	    i := i + 1;
	end;
    end;
    EqualAnyCase := equal;
end;