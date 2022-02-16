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

program p2m2 (input, output);

#include "tokens.h"
#include "stringtab.h"
#include "globals.h"
#include "error.h"
#include "scanner.h"
#include "io.h"
#include "keywords.h"
#include "util.h"

procedure yyparse; external;
var
    yydebug : integer;	{ shared with yacc parser }
    sourceFile : String;
    hFile : String;
    hFlag : boolean;
    argName : FileName;

    
procedure ScanArgs;
var
    i, j, k : integer;
    gotName : boolean;
    done: boolean;
begin

    debugSet := [];
    lowerCaseFlag := false;
    standardFlag := false;
    hFlag := false;
    pascalFlag := false;
    i := 1;
    gotName := false;
    while (argc>i) and not gotName do begin
	argv(i, argName);
	if (argName[0]='-') then begin
	    case (argName[1]) of
		'd' : begin
		    j := 2;
		    while ( argName[j] in ['a'..'z'] ) do begin
			debugSet := debugSet + [argName[j]];
			j := j + 1;
		    end; {while}
		end;
		'l' : begin
		    lowerCaseFlag := true;
		end;
		'h' : begin
		    hFlag := true;
		end;
		'p' : begin
		    pascalFlag := true;
		end;
		's' : begin
		    standardFlag := true;
		end;
	    end; {case}
	    i := i + 1;
	end else begin
	    gotName := true;
	end;
    end; {while}
    if i > argc then begin
	message('p2m2: missing source file');
	halt;
    end else begin
	j := 0;
	while argName[j] <> ' ' do begin
	    AddChar(argName[j]);
	    j := j + 1;
	end;
	sourceFile := NewString;
        if argName[j-2] <> '.' then begin
            sourceFileType := UNKNOWNFILE;
        end else if argName[j-1] = 'p' then begin
            sourceFileType := IMPLFILE;
        end else if argName[j-1] = 'h' then begin
            sourceFileType := DEFNFILE;
        end else begin
            sourceFileType := UNKNOWNFILE;
        end;
	if sourceFileType = UNKNOWNFILE then begin
	    message('p2m2: source file must end in .h or .p');
	    halt;
	end;
	if sourceFileType <> IMPLFILE then begin
	    hFlag := false;
	end;
	for k := 0 to j-3 do begin
	    AddChar(argName[k]);
	end;
        moduleName := NewString;

	if hFlag then begin
	    argName[j-1] := 'h';
	    for k := 0 to j-1 do begin
		AddChar(argName[k]);
	    end;
	    hFile := NewString;
	end;
    end;
end;

begin
    InitFiles;
    InitStringTable;
    ScanArgs;
    { initialization section }
    numberOfErrors := 0;
    TAB := chr(9);
    InitKeywords;

    if hFlag then begin
	{ parse .h file first }
	InitScanner(hFile);
	InitPass0;
	repeat
	    yyparse;
	until (currToken.kind = TKENDOFFILE);

	InitScanner(sourceFile);
	InitPass1;
	repeat
	    yyparse;
	until (currToken.kind = TKENDOFFILE);
    end else begin
	{ just do .p or .h file }
	InitScanner(sourceFile);
	InitPass0;
	InitPass1;
	repeat
	    yyparse;
	until (currToken.kind = TKENDOFFILE);
    end;

    Pass2;

    if numberOfErrors = 0 then begin
	message('No parsing errors');
    end else if numberOfErrors = 1 then begin
	message('1 parsing error');
	exit(numberOfErrors);
    end else begin
	message(numberOfErrors:1,' parsing errors');
	exit(numberOfErrors);
    end;
end {p2m2}.