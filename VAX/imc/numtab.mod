implementation module numtab;

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
from symtab import currScope, DataType, builtinScope;
(* number table *)
const
    NUMTABSIZE = 1000;
var
    numTab : array [0..NUMTABSIZE] of Number;


procedure LookUpNumber(name : integer; scope : Scope) : Number;
var
    num : Number;
    numName : integer;
    found : boolean;
    hash : integer;
begin
    if scope = 0 then
	scope := currScope;
    end;
    if name <= MAXBUILTINTYPES then
	scope := builtinScope;
    end;
    found := false;
    if traceNumtab then
	writef(output,'LookUpNumber block %d\n',scope);
    end;
    hash := (name*name + integer(scope)) mod (NUMTABSIZE+1);
    num := numTab[hash];
    while not found and (num <> nil) do
	if traceNumtab then
	    writef(output,'LookUpNumber ');
	    writef(output,'%d',num^.name);
	    writef(output,' %d ',num^.block);
	    writef(output,'%d',name);
	    writef(output,' %d %d\n',scope,hash);
	end;
	if (num^.block = scope) and (num^.name = name) then
	    found := true;
	else
	    num := num^.nextInTable;
	end;
    end;
    if (num = nil) and traceNumtab then
	writef(output,'LookUpNumber did not find ');
	writef(output,'%d',name);
    end;
    if traceNumtab then
	writef(output,'LookUpNumber %d ',name);
	if num = nil then
	    writef(output,'not found\n');
	else
	    writef(output,'%x\n',integer(num^.numType));
	end;
    end;
    return num;
end LookUpNumber;

procedure DefineNumber(var num : Number; name : integer; scope : Scope):boolean;
var
    hash : integer;
    found : boolean;
begin
    if scope = 0 then
	scope := currScope;
    end;
    hash := (name*name + integer(scope)) mod (NUMTABSIZE+1);
    num := numTab[hash];
    found := false;
    while (num <> nil) and not found do
	if (num^.block = scope) and (num^.name = name) then
	    found := true;
	else
	    num := num^.nextInTable;
	end;
    end;
    if found then
	(* do nothing *)
	if traceNumtab then
	    writef(output,'DefineNumber %d ',name);
	    if num = nil then
		writef(output,'not found\n');
	    else
		writef(output,'%x\n',integer(num^.numType));
	    end;
	end;
    else
	new(num);
	num^.name := name;
	num^.block := scope;
	num^.nextInTable := numTab[hash];
	numTab[hash] := num;
	if traceNumtab then
	    writef(output,'DefineNumber ');
	    writef(output,'%d',num^.name);
	    writef(output,' %d %d\n',num^.block,hash);
	end;
    end;
    return not found;
end DefineNumber;

procedure DumpNumTab();
var
    i : cardinal;
    num : Number;
begin
    for i := 0 to NUMTABSIZE do
	num := numTab[i];
	while num <> nil do
	    writef(output,'hv=%d b=%d k=%d ',i,num^.block,ord(num^.numType^.kind));
	    writef(output,'%d',num^.name);
	    writef(output,'\n');
	    num := num^.nextInTable;
	end;
    end;
end DumpNumTab;

var
    i : cardinal;
    tn : TypeNode;
    num : Number;
    error : boolean;
begin
    for i := 0 to NUMTABSIZE do
	numTab[i] := nil;
    end;
    error := false;
    i := 1;
    new(tn);
    tn^.kind := DTINTEGER;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTCHAR;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTBOOLEAN;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTCARDINAL;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTREAL;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTLONGREAL;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTWORD;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTBYTE;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTADDRESS;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTFILE;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTPROCESS;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    i := i + 1;
    new(tn);
    tn^.kind := DTCARDINT;
    error := error or not DefineNumber(num,i,builtinScope);
    num^.numType := tn;
    cardIntTypeNode := tn;
    i := i + 1;

    if error then
	writef(output,"Could not define builtin types\n");
	halt;
    end;
end numtab.
