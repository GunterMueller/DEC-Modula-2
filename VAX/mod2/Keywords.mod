implementation module Keywords;

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

$if modula2 then
from Globals import
    standardKeywordFlag;
$end

from Strings import
    String, NewText, StringBlock, StringIndex;

from Tokens import 
    Token;

const
    KEYHASHSIZE = 100;
type
    Keyword     = pointer to KeywordRec;
    KeywordRec  = record
	str     : String;
	token   : Token;
	next    : Keyword;
    end;
    HashIndex   = [0..KEYHASHSIZE];
var
    keywordTable : array HashIndex of Keyword;


procedure KeyLookUp(ident : String) : Token;
(* If ident is a reserved word, return the corresponding Token.  
   Otherwise return TKIDENT *)
var
	hashIndex   : HashIndex;
	key	    : Keyword;
	keyStr      : String;
	i	    : cardinal;
	identBlock  : StringBlock;
	keyBlock    : StringBlock;
	identIndex  : StringIndex;
	keyIndex    : StringIndex;

begin (*KeyLookUp*)
    (* Note: hash values are case-independent *)
    hashIndex := ident^.hash mod (KEYHASHSIZE+1);
    key := keywordTable[hashIndex];
    identIndex := ident^.index;

    while key # nil do
	keyStr := key^.str;
	if (ident^.hash = keyStr^.hash) and (ident^.length = keyStr^.length) then
	    i := 0;
	    identBlock := ident^.block;
	    keyBlock := keyStr^.block;
	    keyIndex := keyStr^.index;
$if modula2 then
	    if standardKeywordFlag then (* keywords must be upper case *)
$end
		while (i < ident^.length) and
			(identBlock^.s[identIndex+i] = 
			    keyBlock^.s[keyIndex+i]) do
		    i := i + 1;
		end;
$if modula2 then
	    else			(* keyword case doesn't matter *)
		while (i < ident^.length) and
                        (CAP(identBlock^.s[identIndex+i]) =
                            keyBlock^.s[keyIndex+i]) do
                    i := i + 1;
                end;
	    end;
$end
	    if i = ident^.length then
		return key^.token;
	    end;
	end (* if ident^.hash = ... *);
	key := key^.next;
    end (* while *);  
    return TKIDENT;
end KeyLookUp;

procedure InitKeywords(const dumpKeywords : boolean);

    procedure InstallKey(const keyword : array of char; const token : Token);
    var
	str : String;
	key : Keyword;
    begin
	str := NewText(keyword);
	new(key);
	key^.str := str;
	key^.token := token;
	key^.next := keywordTable[str^.hash mod (KEYHASHSIZE+1)];
	keywordTable[str^.hash mod (KEYHASHSIZE+1)] := key;
    end InstallKey;

var
    i   : HashIndex;
    key : Keyword;

begin (* InitKeywords *)
    for i:=0 to KEYHASHSIZE do	(*initialize key hash array*)
	keywordTable[i] := nil;
    end;

$if modula2 then
    InstallKey('AND',	    TKAND);	(*put keywords in strTable*)
    InstallKey('ARRAY',	    TKARRAY);
    InstallKey('BEGIN',	    TKBEGIN);
    InstallKey('BY',	    TKBY);
    InstallKey('CASE',	    TKCASE);
    InstallKey('CONST',	    TKCONST);
    InstallKey('DEFINITION',TKDEFINITION);
    InstallKey('DIV',	    TKDIV);
    InstallKey('DO',	    TKDO);
    InstallKey('ELSE',	    TKELSE);
    InstallKey('ELSIF',	    TKELSIF);
    InstallKey('END',	    TKEND);
    InstallKey('EXIT',	    TKEXIT);
    InstallKey('EXPORT',    TKEXPORT);
    InstallKey('FOR',	    TKFOR);
    InstallKey('FROM',	    TKFROM);
    InstallKey('IF',	    TKIF);
    InstallKey('IMPLEMENTATION',TKIMPLEMENTATION);
    InstallKey('IMPORT',    TKIMPORT);
    InstallKey('IN',	    TKIN);
    InstallKey('LOOP',	    TKLOOP);
    InstallKey('MOD',	    TKMOD);
    InstallKey('MODULE',    TKMODULE);
    InstallKey('NOT',	    TKNOT);
    InstallKey('OF',	    TKOF);
    InstallKey('OR',	    TKOR);
    InstallKey('POINTER',   TKPOINTER);
    InstallKey('PROCEDURE', TKPROCEDURE);
    InstallKey('QUALIFIED', TKQUALIFIED);
    InstallKey('RECORD',    TKRECORD);
    InstallKey('REPEAT',    TKREPEAT);
    InstallKey('RETURN',    TKRETURN);
    InstallKey('SET',	    TKSET);
    InstallKey('THEN',	    TKTHEN);
    InstallKey('TO',	    TKTO);
    InstallKey('TYPE',	    TKTYPE);
    InstallKey('UNTIL',	    TKUNTIL);
    InstallKey('VAR',	    TKVAR);
    InstallKey('WHILE',	    TKWHILE);
    InstallKey('WITH',	    TKWITH);
    InstallKey('@SIZE',	    TKATSIZE);
    InstallKey('@ALIGN',    TKATALIGN);
    InstallKey('@PASCAL',   TKATPASCAL);
    InstallKey('@C',	    TKATC);
    InstallKey('@NOCHECK',  TKATNOCHECK);
    InstallKey('@NILCHECK', TKATNILCHECK);
    InstallKey('@LOCAL',    TKATLOCAL);
    InstallKey('@NOCOUNT',  TKNOCOUNT);
    InstallKey('@EXTERNAL', TKEXTERNAL);
    InstallKey('@GLOBAL',   TKGLOBAL);
    InstallKey('@INLINE',   TKINLINE);
    InstallKey('@SHARED',   TKSHARED);
    InstallKey('@NOINIT',   TKNOINIT);
    InstallKey('DYNARRAY',  TKDYNARRAY);
(* ||| Get rid of the old @DYNARRAY *)
    InstallKey('@DYNARRAY', TKDYNARRAY);
    InstallKey('SUBARRAY',  TKSUBARRAY);
    InstallKey('@LEFTTORIGHT',  TKLEFTTORIGHT);
    InstallKey('@RIGHTTOLEFT',  TKRIGHTTOLEFT);
    InstallKey('@ASM',		TKATASM);

$else (* pascal *)
    InstallKey('and',	    TKAND);
    InstallKey('array',	    TKARRAY);
    InstallKey('begin',	    TKBEGIN);
    InstallKey('case',	    TKCASE);
    InstallKey('const',	    TKCONST);
    InstallKey('div',	    TKDIV);
    InstallKey('do',	    TKDO);
    InstallKey('downto',    TKDOWNTO);
    InstallKey('else',	    TKELSE);
    InstallKey('end',	    TKEND);
    InstallKey('external',  TKEXTERNAL);
    InstallKey('file',	    TKFILE);
    InstallKey('for',	    TKFOR);
    InstallKey('forward',   TKFORWARD);
    InstallKey('function',  TKFUNCTION);
    InstallKey('goto',	    TKGOTO);
    InstallKey('hex',	    TKHEX);
    InstallKey('if',	    TKIF);
    InstallKey('in',	    TKIN);
    InstallKey('label',	    TKLABEL);
    InstallKey('mod',	    TKMOD);
    InstallKey('not',	    TKNOT);
    InstallKey('oct',	    TKOCT);
    InstallKey('of',	    TKOF);
    InstallKey('or',	    TKOR);
    InstallKey('packed',    TKPACKED);
    InstallKey('procedure', TKPROCEDURE);
    InstallKey('program',   TKPROGRAM);
    InstallKey('record',    TKRECORD);
    InstallKey('repeat',    TKREPEAT);
    InstallKey('set',	    TKSET);
    InstallKey('then',	    TKTHEN);
    InstallKey('to',	    TKTO);
    InstallKey('type',	    TKTYPE);
    InstallKey('until',	    TKUNTIL);
    InstallKey('var',	    TKVAR);
    InstallKey('while',	    TKWHILE);
    InstallKey('with',	    TKWITH);
$end

    if dumpKeywords then
	for i := 0 to KEYHASHSIZE do
	    key := keywordTable[i];
	    while key # nil do
		Writef(output, '%d: %n \n', i, key^.token);
		key := key^.next;
	    end;
	end;
    end (* if dumpKeywords *);

end InitKeywords;

end Keywords.
