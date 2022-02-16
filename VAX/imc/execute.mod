implementation module execute;

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

from io import writef, output, terminal;
from system import adr;
from unix import execve, environ, uexit, File, fflush;
from strings import Assign, Append;
from parameters import GetEnvironment;
from Strings import CopyString;
from porttab import logErrorsFlag;

var
    numArgs : cardinal;
    args : array [0..MAXARGS] of ArgString;
    argsp : @dynarray @nocount of ArgString;
    argStrings : array [0..MAXARGS] of array [0..ARGSIZE] of char;
    pathString : array [0..ARGSIZE] of char;
    programFile : array [0..ARGSIZE] of char;

procedure Execute(const name : array of char);
var
    i, ignore : integer;
    j, k, nameLength : cardinal;
begin
    nameLength := 0;
    while (nameLength < number(name)-1) and (name[nameLength] # 0C) do
	inc(nameLength);
    end;
    args[numArgs] := nil;   
    argsp := adr(args);
    if logErrorsFlag then
	writef(output,"%s:",name);
	i := 0;
	while args[i] # nil do
	    writef(output," %s",argStrings[i]);
	    inc(i);
	end;
	writef(output,"\n");
    end;
    GetEnvironment("PATH",pathString,i);
    if i < 0 then
	Assign(pathString,":/usr/ucb:/bin:/usr/bin");
    end;
    i := 0;
    while pathString[i] # 0C do
	j := 0;
	while (pathString[i] # 0C) and (pathString[i] # ':') do
	    programFile[j] := pathString[i];
	    inc(i);
	    inc(j);
	end;
	if (j > 0) and (programFile[j-1] # '/') then
	    programFile[j] := '/';
	    inc(j);
	end;
	programFile[j] := 0C;
	Append(programFile,name);
	fflush(File(output));
	fflush(File(terminal));
	ignore := execve(programFile,adr(args),environ);
	(* won't return, if successful *)
	if pathString[i] # 0C then
	    inc(i);
	end;
    end;
    writef(output,"%s: Command not found\n",name);
    uexit(99);
end Execute;

procedure AddArg(const arg : array of char);
begin
     Assign(argStrings[numArgs],arg);
     args[numArgs] := adr(argStrings[numArgs]);
     inc(numArgs);
end AddArg;

procedure AddString(str : String);
begin
     CopyString(str,argStrings[numArgs]);
     args[numArgs] := adr(argStrings[numArgs]);
     inc(numArgs);
end AddString;

begin
    numArgs := 0;
end execute.
