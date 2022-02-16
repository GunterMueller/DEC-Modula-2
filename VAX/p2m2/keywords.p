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
#include "globals.h"
#include "keywords.h"

const
    KEYHASHSIZE = 100;
type
    KeywordRecPtr = ^ KeywordRec;
    KeywordRec = record
	str : String;
	token : Token;
	next : KeywordRecPtr;
    end;
var
    keywordTab : array [0..KEYHASHSIZE] of KeywordRecPtr;
    tokenKeywordTab : array [TKAND..TKINCLUDE] of String;

procedure DumpKeywords;
var
    i : integer;
    key : KeywordRecPtr;
begin
    for i := 0 to KEYHASHSIZE do begin
	key := keywordTab[i];
	while key <> nil do begin
	    write(output,i:0,' ',key^.token:0,' ');
	    WriteString(output,key^.str);
	    writeln(output);
	    key := key^.next;
	end;
    end;
end;

procedure InstallKey(keyword : ShortString; token : Token);
var
    i : integer;
    str : String;
    key : KeywordRecPtr;
begin
    i := 1;
    while (i < SHORTSTRINGSIZE) and (keyword[i] <> ' ') do begin
	AddChar(keyword[i]);
	i := i + 1;
    end;
    str := NewString;
    if token in [TKAND..TKINCLUDE] then begin
	tokenKeywordTab[token] := str;
    end;
    new(key);
    key^.str := str;
    key^.token := token;
    key^.next := keywordTab[str^.hash mod (KEYHASHSIZE+1)];
    keywordTab[str^.hash mod (KEYHASHSIZE+1)] := key;
end;

procedure InitKeywords;
var
    i : integer;
begin {InitKeys}
	for i:=0 to KEYHASHSIZE do begin	{initialize key hash array}
	    keywordTab[i] := nil;
	end;
	InstallKey('AND             ', TKAND);	{put keywords in strTable}
	InstallKey('ARRAY           ', TKARRAY);
	InstallKey('BEGIN           ', TKBEGIN);
	InstallKey('BY              ', TKBY);
	InstallKey('CASE            ', TKCASE);
	InstallKey('CONST           ', TKCONST);
{	InstallKey('DEFINITION      ', TKDEFINITION);}
	InstallKey('DIV             ', TKDIV);
	InstallKey('DO              ', TKDO);
	InstallKey('ELSE            ', TKELSE);
{	InstallKey('ELSIF           ', TKELSIF);}
	InstallKey('END             ', TKEND);
{	InstallKey('EXIT            ', TKEXIT);}
{	InstallKey('EXPORT          ', TKEXPORT);}
	InstallKey('FOR             ', TKFOR);
{	InstallKey('FROM            ', TKFROM);}
	InstallKey('IF              ', TKIF);
{	InstallKey('IMPLEMENTATION  ', TKIMPLEMENTATION);}
{	InstallKey('IMPORT          ', TKIMPORT);}
	InstallKey('IN              ', TKIN);
{	InstallKey('LOOP            ', TKLOOP);}
	InstallKey('MOD             ', TKMOD);
{	InstallKey('MODULE          ', TKMODULE);}
	InstallKey('NOT             ', TKNOT);
	InstallKey('OF              ', TKOF);
	InstallKey('OR              ', TKOR);
{	InstallKey('POINTER         ', TKPOINTER);}
	InstallKey('PROCEDURE       ', TKPROCEDURE);
{	InstallKey('QUALIFIED       ', TKQUALIFIED);}
	InstallKey('RECORD          ', TKRECORD);
	InstallKey('REPEAT          ', TKREPEAT);
{	InstallKey('RETURN          ', TKRETURN);}
	InstallKey('SET             ', TKSET);
	InstallKey('THEN            ', TKTHEN);
	InstallKey('TO              ', TKTO);
	InstallKey('TYPE            ', TKTYPE);
	InstallKey('UNTIL           ', TKUNTIL);
	InstallKey('VAR             ', TKVAR);
	InstallKey('WHILE           ', TKWHILE);
	InstallKey('WITH            ', TKWITH);
{	InstallKey('@SIZE           ', TKATSIZE);}
{	InstallKey('@ALIGN          ', TKATALIGN);}
{	InstallKey('@PASCAL         ', TKATPASCAL);}
{	InstallKey('@NOCHECK        ', TKATNONE);}
{	InstallKey('@NILCHECK       ', TKATNIL);}
	InstallKey('INCLUDE        ', TKINCLUDE);
	InstallKey('DOWNTO          ',TKDOWNTO);
	InstallKey('EXTERNAL        ',TKEXTERNAL);
	InstallKey('FILE            ',TKFILE);
	InstallKey('FORWARD         ',TKFORWARD);
	InstallKey('FUNCTION        ',TKFUNCTION);
	InstallKey('HEX             ',TKHEX);
	InstallKey('GOTO            ',TKGOTO);
	InstallKey('LABEL           ',TKLABEL);
	InstallKey('PACKED          ',TKPACKED);
	InstallKey('PROGRAM         ',TKPROGRAM);

	if 'k' in debugSet then begin
	    DumpKeywords;
	end;
end; {InitKeys}

function KeyLookUp{(ident : String): Token};
var
	hash : integer;
	found : boolean;
	key : KeywordRecPtr;
	keyStr : String;
	i : integer;
	ci, ck : char;

begin {KeyLookUp}
    { keyword lookup ignores case.  Hash values are case-independent }
    found := false;
    hash := ident^.hash mod (KEYHASHSIZE+1);;
    key := keywordTab[hash];
    while (key <> nil) and not found do begin
	keyStr := key^.str;
	if (ident^.hash = keyStr^.hash) and (ident^.length = keyStr^.length)
	then begin
	    found := true;
	    i := 0;
	    while (i < ident^.length) and found do begin
		ci := GetChar(ident,i);
		ck := GetChar(keyStr,i);
		if ci in ['a'..'z'] then begin
		    ci := chr(ord(ci)-ord('a')+ord('A'));
		end;
		found := ci = ck;
		i := i + 1;
	    end;
	end;
	if not found then begin
	    key := key^.next;
	end;
    end;
    if key <> nil then begin
	KeyLookUp := key^.token;
    end else begin
	KeyLookUp := TKIDENT;
    end;
end; {KeyLookUp}

