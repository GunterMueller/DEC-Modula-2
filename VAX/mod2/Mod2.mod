module Mod2;

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


from io import Writef, output, Open;

$IF vms THEN
from unix import freopen,getenv;
$END

from Machine import
    argv, argc, yyparse, yydebug, CODEFILENAME, FileName, ExitProgram,
    CharSet;

from Globals import
    genDebugInfoFlag, genDebugInfoRuntimeFlag,
    genCheckFlag, genPtrAssignCheckFlag, optimFlag, 
    standardKeywordFlag, standardCardinalFlag, standardStringFlag, 
    compiledDefFlag,
    target, TargetMachine, 
    TraceCexpr, TraceDecls, TraceGenpc, TraceNexpr, TraceSymtab, 
    TraceStab, TraceNstmt, TraceOpt, TraceOptim, TraceGent, TraceMark,
    TraceActions, TraceCount,
    OptNloop, OptNreg, OptNind, OptNtail, OptNcall,
    internalCallFlag, debugSet, DebugSet, DEBUG, mainFileName;

$if pascal then
from Globals import
    bufferFlag;
$end

from Strings import
    InitStrings, AddChar, NonHashString, DumpStrings;

from Keywords import
    InitKeywords;

from Scanner import
    upperCaseFlag, InitScanner;

from Symbols import
    DumpSymbols, globalModule;

from Decls import
    standardExportFlag, adviseFlag, InitGlobalModule, CheckModule, 
    OutputImports;

from Errors import
    numberOfErrors, totalErrors, Error;

from InitBuiltin import
    InitStandardTypes, InitBuiltin;

from OTree import
    Optimize;

from GenCode import
    gprofFlag, genCountFlag, GenCode, codeFile;

var
    verboseFlag  : boolean;
    codeFileName : FileName;

procedure ErrorArg(const msg, fileName : array of char);
begin
$IF vms THEN
	Writef(output, 'mod2.exe:%s %s\n', msg, fileName);
$ELSE
	Writef(output, 'mod2.0:%s %s\n', msg, fileName);
$END
end ErrorArg;

procedure EqualArg(const a, b : array of char) : boolean;
var
    i : integer;
begin
    i := 0;
    while (a[i] = b[i]) and (a[i] # 0C) do
	i := i + 1;
    end;
    return a[i] = b[i];
end EqualArg;

procedure ScanArgs;
var
    i, j, k : integer;
    gotName : boolean;
    argName : FileName;
begin
    debugSet := DebugSet{};

    TraceCexpr := false;
    TraceDecls := false;
    TraceGenpc := false;
    TraceGent := false;
    TraceNexpr := false;
    TraceSymtab := false;
    TraceStab := false;
    TraceNstmt := false;
    TraceOpt := false;
    TraceOptim := false;
    TraceMark := false;
    TraceActions := false;
    TraceCount := false;

    genDebugInfoFlag := false;
    genDebugInfoRuntimeFlag := false;
    genCheckFlag := 0;
    genPtrAssignCheckFlag := 0;
    optimFlag := false;
$if pascal then
    bufferFlag := 1;
    standardKeywordFlag := true;
$else
    standardKeywordFlag := false;
$end
    standardCardinalFlag := false;
    standardStringFlag := false;
    standardExportFlag := false;
    compiledDefFlag := false;
    gprofFlag := false;
    genCountFlag := false;
    internalCallFlag := false;
    upperCaseFlag := false;
    adviseFlag := false;
    verboseFlag := false;

    OptNloop := false;
    OptNreg := false;
    OptNind := false;
    OptNtail := false;
    OptNcall := true;

    codeFileName := CODEFILENAME;
    i := 1;
    gotName := false;
    while (argc()>i) and not gotName do
	argv(i, argName);
	if (argName[1]='-') then
	    case (argName[2]) of
	    | 'd' :
		j := 3;
		while ( argName[j] in CharSet{'a'..'z'} ) do
		    incl(debugSet, argName[j]);
		    j := j + 1;
		end; (*while*)
	    
	    | 'N' :
		if EqualArg(argName,'-Nreg') then
		    OptNreg := true;
		elsif EqualArg(argName,'-Nloop') then
		    OptNloop := true;
		elsif EqualArg(argName,'-Nind') then
		    OptNind := true;
		elsif EqualArg(argName,'-Ntail') then
		    OptNtail := true;
		elsif EqualArg(argName,'-Ncall') then
		    OptNcall := true;
		end;
	    
	    | 'T' :
		if EqualArg(argName,'-Tcexpr') then
		    TraceCexpr := true;
		elsif EqualArg(argName,'-Tdecls') then
		    TraceDecls := true;
		elsif EqualArg(argName,'-Tnexpr') then
		    TraceNexpr := true;
		elsif EqualArg(argName,'-Tnstmt') then
		    TraceNstmt := true;
		elsif EqualArg(argName,'-Tsymtab') then
		    TraceSymtab := true;
		elsif EqualArg(argName,'-Tgenpc') then
		    TraceGenpc := true;
		elsif EqualArg(argName,'-Tgent') then
		    TraceGent := true;
		elsif EqualArg(argName,'-Tstab') then
		    TraceStab := true;
		elsif EqualArg(argName,'-Tactions') then
		    TraceActions := true;
		elsif EqualArg(argName,'-Toptim') then
		    TraceOptim := true;
		elsif EqualArg(argName,'-Topt') then
		    TraceOptim := true;
		    TraceOpt := true;
		    TraceActions := true;
		elsif EqualArg(argName,'-Tcount') then
		    TraceCount := true;
		elsif EqualArg(argName,'-Tmark') then
		    TraceMark := true;
		    TraceOptim := true;
		    TraceOpt := true;
		else
		    ErrorArg('Unknown option: ',argName);
		    ExitProgram(101);
		end;
	    
	    | 't' :
		if (argName[3] = 'b') or (argName[3] = 0C) then
		    target := TARGETTITAN;
		elsif argName[3] = 'm' then
		    target := TARGETTITANM;
		else
		    ErrorArg('Unknown option: ',argName);
		    ExitProgram(101);
		end;

$if modula2 then	    
	    | 'a' :
		adviseFlag := true;
	    
	    | 'u' :
		upperCaseFlag := true;
$end
	    
	    | 'v' :
		verboseFlag := true;
	    
	    | 's' :
$if modula2 then
		if argName[3] = 0C then
		    standardStringFlag := true;
		    standardKeywordFlag := true;
		    standardCardinalFlag := true;
		    standardExportFlag := true;
		elsif argName[3] = 'c' then
		    standardCardinalFlag := true;
		elsif argName[3] = 'e' then
		    standardExportFlag := true;
		elsif argName[3] = 'k' then
		    standardKeywordFlag := true;
		elsif argName[3] = 's' then
		    standardStringFlag := true;
		else
		    ErrorArg('Unknown option: ',argName);
		    ExitProgram(101);
		end;
$else
		if argName[3] # 0C then
		    ErrorArg('Unknown option: ', argName);
		    ExitProgram(101);
		end;
$end

	    | 'g' :
		genDebugInfoFlag := true;
	    
	    | 'G' :
		genDebugInfoRuntimeFlag := true;

	    | 'o' :
		i := i + 1;
		if i >= argc() then
		    ErrorArg('No file for ',argName);
		    ExitProgram(101);
		end;
		argv(i,codeFileName);
	    
	    | 'C' :
		if argName[3] = 0C then
		    genCheckFlag := 1;
		elsif (argName[3] = 'p') then
		    genPtrAssignCheckFlag := 1;
		end;

$if pascal then
	    | 'b' :
		if argName[3] in CharSet{'0'..'9'} then
		    bufferFlag := 0;
		    j := 3;
		    repeat
			bufferFlag := bufferFlag*10 +
				ord(argName[j]) - ord('0');
			j := j + 1;
		    until not (argName[j] in CharSet{'0'..'9'});
		else
		    bufferFlag := 2;
		end;
$end (* pascal *)
	    
	    | 'O' :
		optimFlag := true;
		OptNcall := false;
	    
	    | 'p' :
		if argName[3] = 's' then
		    genCountFlag := true;
		else
		    gprofFlag := true;
		end;
	    
	    | 'j' :
		internalCallFlag := true;
	    
	    | 'y' :
		compiledDefFlag := true;

	    | 'x', 'D' : (* nothing - InitPreprocess scans for itself *)
	    else
	        ErrorArg('Unknown option: ',argName);
	        ExitProgram(101);
	    end (* case *);
	    i := i + 1;
	else
	    gotName := true;
	end;
    end; (*while*)
    if not gotName then
	mainFileName := nil;
    else
	j := 1;
	while argName[j] # 0C do
	    AddChar(argName[j]);
	    j := j + 1;
	end;
	mainFileName := NonHashString();
	if EqualArg(codeFileName,CODEFILENAME) then
	    if j > 5 then
		if (argName[j-4] = '.') and (argName[j-3] = 'm')
		    and (argName[j-2] = 'o') and (argName[j-1] = 'd') then
		    for i := 1 to j-4 do
			codeFileName[i] := argName[i];
		    end;
		    case target of
			| TARGETVAX :
			    codeFileName[j-3] := 'p';
			    codeFileName[j-2] := 'c';
			    codeFileName[j-1] := 'd';
			| TARGETTITAN, TARGETTITANM :
			    codeFileName[j-3] := 'i';
			    codeFileName[j-2] := 0C;
		    end (* case *);
		end;
	    end;
	end;
    end;
    InitScanner(mainFileName);
end ScanArgs;


begin   (* Mod2 *)
(* On VMS SYS$OUTPUT -> stdout -> IO_output gets bound to null sometimes. *)
(* As long as you "$ DEFINE MOD$OUTPUT SYS$OUTPUT" (in some fashion) *)
(* before you invoke mod2 - you should be OK - CED 4/6/87 *)
$IF vms THEN
    output := freopen(getenv("MOD$OUTPUT")^,"w",output);
$END

    InitStrings(Error);
    ScanArgs;
    if mainFileName = nil then
	codeFile := output;
    else
	codeFile := Open(codeFileName, 'w');
    end;
    if codeFile = nil then
	ErrorArg('Cannot open output file', codeFileName);
	ExitProgram(101);
    end;
    if DEBUG and ('p' in debugSet) then
	yydebug := 1;	(*turn on parser debug*)
    end;
    (* initialization section *)

    numberOfErrors := 0;
    totalErrors := 0;
    InitKeywords(DEBUG and ('k' in debugSet));
    (* These three must be in this order and after the above *)
    InitStandardTypes; InitGlobalModule; InitBuiltin;

    yyparse;

    genCheckFlag := 0;
    genPtrAssignCheckFlag := 0;
    CheckModule(globalModule);

    if DEBUG and ('s' in debugSet) then
	Writef(output,'String table\n');
	DumpStrings;
    end;
    if DEBUG and ('r' in debugSet) then
	Writef(output,'Symbol table\n');
	DumpSymbols;
    end;

    if numberOfErrors = 0 then
	if adviseFlag then
	    OutputImports;
	end;
	if verboseFlag then
	    Writef(output,'No parsing errors\n');
	end;
    elsif numberOfErrors = 1 then
	Writef(output,'1 parsing error\n');
	ExitProgram(numberOfErrors);
    else
	Writef(output,'%d parsing errors\n', numberOfErrors);
	ExitProgram(numberOfErrors);
    end;

    if optimFlag then
	if verboseFlag then
	    Writef(output,'Start optimization\n');
	end;
	Optimize;
	if verboseFlag then
	    Writef(output,'End optimization\n');
	end;
    end;

    GenCode;
    if numberOfErrors = 0 then
	if verboseFlag then
	    Writef(output,'No intermediate code generation errors\n');
	end;
    elsif numberOfErrors = 1 then
	Writef(output,'1 intermediate code generation error\n');
    else
	Writef(output, '%d intermediate code generation errors\n', 
		numberOfErrors);
	if numberOfErrors > 99 then
	    numberOfErrors := 99;
	end;
    end;

    ExitProgram(numberOfErrors);
end Mod2.
