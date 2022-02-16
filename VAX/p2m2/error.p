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
#include "error.h"
#include "scanner.h"

procedure MakeErrorString(var str : String; string : ShortString);
begin
    AddText(string);
    str := NewString;
end;

procedure ErrorAll(name : String; msg : ErrorString; number : integer;
	printNumber : boolean);
var
    i, j : integer;
begin
    write(output,'!!! ');
    if readingFile and (inFile <> nil) then begin
	if inFile^.name = nil then begin
	    write(output,'input');
	end else begin
	    WriteString(output,inFile^.name);
	end;
	write(output, ', line ',inFile^.lineNumber:1,': ');
    end else begin
	if currFile = nil then begin
	    write(output,'input');
	end else begin
	    WriteString(output,currFile);
	end;
	write(output, ', line ',currLine:1,': ');
    end;
    if name <> nil then begin
	WriteString(output,name);
	write(output,': ');
    end;
    i := ERRORSTRINGSIZE;
    while (i > 1) and (msg[i] = ' ') do begin
	i := i - 1;
    end;
    for j := 1 to i do begin
	write(output,msg[j]);
    end;
    if printNumber then begin
	write(output,number:1);
    end;
    writeln(output);
    if readingFile and (inFile <> nil) then begin
	LineWrite;
	for i := 1 to inFile^.ptr-1 do begin
	    if inFile^.line[i] = TAB then begin
		write(output,TAB);
	    end else begin
		write(output,' ');
	    end;
	end;
	writeln(output,'^');
    end;
    numberOfErrors := numberOfErrors + 1;
end;

procedure ErrorName{(name : String; msg : ErrorString)};
begin
    ErrorAll(name,msg,0,false);
end;

procedure ErrorNumber{(msg : ErrorString; number : integer)};
begin
    ErrorAll(nil,msg,number,true);
end;

procedure Error{(msg : ErrorString)};
begin
    ErrorAll(nil,msg,0,false);
end;

procedure GeneralError(fileName : String; lineNumber : integer;
    name : String; msg : ErrorString; number : integer; printNumber : boolean);
var
    i, j : integer;
begin
    write(output,'!!! ');
    WriteString(output,fileName);
    write(output,', line ',lineNumber:1,': ');
    if name <> nil then begin
	WriteString(output,name);
	write(output,': ');
    end;
    i := ERRORSTRINGSIZE;
    while (i > 1) and (msg[i] = ' ') do begin
	i := i - 1;
    end;
    for j := 1 to i do begin
	write(output,msg[j]);
    end;
    if printNumber then begin
	write(output,number:1);
    end;
    writeln(output);
    numberOfErrors := numberOfErrors + 1;
end;
