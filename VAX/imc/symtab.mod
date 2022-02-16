implementation module symtab;

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

from io import writef, output;
from MemLib import ALLOCATE;
from Strings import String, WriteString;
(* symbol table *)
const
    SYMTABSIZE = 1000;
var
    symTab : array [0..SYMTABSIZE] of Symbol;
    generateScope : cardinal;

procedure NewScope() : Scope;
begin
    generateScope := generateScope + 1;
    return generateScope;
end NewScope;

procedure DefineSymbol(var sym : Symbol; name : String; scope : Scope):boolean;
var
    hash : cardinal;
    found : boolean;
begin
    if scope = 0 then
	scope := currScope;
    end;
    hash := (name^.hash + scope) mod (SYMTABSIZE+1);
    sym := symTab[hash];
    found := false;
    while (sym <> nil) and not found do
	if (sym^.block = scope) and (sym^.name = name) then
	    found := true;
	else
	    sym := sym^.nextInTable;
	end;
    end;
    if found then
	(* do nothing *)
    else
	new(sym);
	sym^.name := name;
	sym^.block := scope;
	sym^.nextInTable := symTab[hash];
	symTab[hash] := sym;
	sym^.kind := SYMNULL;
(* ||| *)
	if traceSymtab then
	    writef(output,'DefineSymbol ');
	    WriteString(output,sym^.name);
	    writef(output,' %d %d\n',sym^.block,hash);
	end;
(**)
    end;
    return not found;
end DefineSymbol;

procedure DumpSymTab();
var
    i : cardinal;
    sym : Symbol;
begin
    for i := 0 to SYMTABSIZE do
	sym := symTab[i];
	while sym <> nil do
	    writef(output,'hv=%d b=%d k=%n ',i,sym^.block,sym^.kind);
	    WriteString(output,sym^.name);
	    writef(output,'\n');
	    sym := sym^.nextInTable;
	end;
    end;
end DumpSymTab;

var
    i : cardinal;
begin
    for i := 0 to SYMTABSIZE do
	symTab[i] := nil;
    end;
    generateScope := 0;
    moduleScope := NewScope();
    builtinScope := NewScope();
    globalScope := NewScope();
    currScope := NewScope();

end symtab.
