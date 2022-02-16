implementation module Decls;

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
    Writef, Writes, Writec, output;

from MemLib import
    ALLOCATE;

from Machine import
    WORDSIZE;

from Strings import
    String, NonHashString, NonHashText, AddChar, AddString, 
    WriteString;

from Tokens import
    Token;

from Globals import
    TraceDecls, debugSet, DEBUG, standardKeywordFlag,
    compileModuleName, genCheckFlag;

from Scanner import
    LastModifiedTime;

from Symbols import
    MemoryType, MemoryTypeSet, SymbolCase, GlobalSymKind, Symbol, 
    ConstNode, TypeNode, VarNode, ModuleNode, ProcNode, STMTNONE, StmtList, 
    EnumNode, ParamNode, ParamList, IdentNode, IdentList, 
    CodeNode, CodeList, PortNode, ImportedNode, 
    ImportedList, SymbolKind,  
    DataType, DataTypeSet, InlineValueKind, ArrayKind, VarKind,
    ModuleKindSet, ModuleKind, BIPNOTBIP, ParamKind,
    procTypeNode, ExitStatus,
    addressTypeNode, currScope, FormerElementList, FormerNode,
    globalModule, currModule, globalProc, currProc, globalPortList,
    exitStatus, anyTypeNode, ImportNode, Scope, 
    currLine, currFile, StartScope, EndScope, DefineSymbol, LookUpSymbol,
    DisplayLevelSet;
    
from Errors import
    Error, ErrorName, numberOfErrors;

from Alloc import
    AllocateMemory, AllocateGlobal, InitAllocationNode;

from TypeInfo import
    BaseType, ActualType, StoredType, SizeOf, NewTypeNode, ReferenceByPoint;

from Consts import
    WriteConstant, OrdOf;

from BuildStmt import
    PrintStmtList;

from CheckStmt import
    CheckStmtList, CheckReturn;


$if modula2 then
from Machine import ExitProgram;
from Globals import MODULEINITNAME;
from Symbols import booleanTypeNode, Port;
from Scanner import InitFile;
from Former import MakeFormer;

$else

from io import SWritef;
from Strings import NewText;
from Globals import target, TARGETVAX;
from Symbols import 
    outputDeclared, outputString, inputDeclared, inputString, NULLLABEL, 
    FileVarNode, FileVarList, programFileNameList, programFileVarList, rootFile;
from Errors import ErrorNumber, ProcErrorName;
$end

const
    MODULEINITFLAG = '_initflag';


procedure GlobalName(name:String; theModule : ModuleNode; proc : ProcNode) : String;
var
    globalName : String;

    procedure AddModuleName(theModule : ModuleNode; proc : ProcNode);
    begin
	if (theModule = nil) or
	 ((theModule = globalModule) and ((proc = globalProc) or (proc = nil)))
	then
	    (* do nothing *)
	elsif proc = nil then
	    (* global thing, just modules *)
	    AddModuleName(theModule^.enclosing,proc);
$if modula2 then
	    AddString(theModule^.name);
	    AddChar('_');
$end
	elsif theModule^.enclosingProc = proc then
	    (* next level is a module *)
	    AddModuleName(theModule^.enclosing,proc);
$if modula2 then
	    AddString(theModule^.name);
	    AddChar('_');
$end
	elsif proc^.enclosingModule = theModule then
	    (* next level is a proc *)
	    AddModuleName(theModule,proc^.enclosing);
	    AddString(proc^.name);
	    AddChar('_');
	else
	    ErrorName(theModule^.name,'Module/proc list of $ confused');
	    ErrorName(proc^.name,'Module/proc list of $ confused');
	end;
    end AddModuleName;

begin (* GlobalName *)
    if DEBUG and TraceDecls then
	Writes(output,'GlobalName(');
	WriteString(output,name);
	Writec(output,',');
	if theModule # nil then
	    WriteString(output,theModule^.name);
	end;
	Writec(output,',');
	if proc # nil then
	    WriteString(output,proc^.name);
	end;
	Writes(output,')\n');
    end;
    if name = nil then
	globalName := nil;
    else
	AddModuleName(theModule,proc);
	AddString(name);
	globalName := NonHashString();
    end;
    return globalName;
end GlobalName;

procedure AddToCodeList (list : CodeList; newOne : CodeNode) : CodeList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
	list^.last := nil;
    end;
    if newOne # nil then
        newOne^.next := nil;
        if list^.first = nil then
            list^.first := newOne;
        else
            list^.last^.next := newOne;
        end;
        list^.last:= newOne;
    end;
    return list;
end AddToCodeList;

procedure InitGlobalModule;
begin
    (* This is a super-global module, not the main compilation unit. 
       It can be thought of as exporting all separately compiled modules. *)
    new(globalModule);
    globalModule^.kind := MODGLOBAL;
    globalModule^.scope := currScope;
    globalModule^.exportScope := currScope;
    globalModule^.body := nil;
    globalModule^.qualExports := nil;
    globalModule^.unqualExports := nil;
    new(globalModule^.imports);
    globalModule^.imports^.first := nil;
    globalModule^.imports^.last := nil;
    globalModule^.imported := nil;
    globalModule^.doingImport := false;
    globalModule^.exportAll := false;
    globalModule^.containsInline := false;
    globalModule^.noinit := true;
    globalModule^.doinit := false;
    globalModule^.enclosing := nil;
    globalModule^.enclosingProc := nil;
    new(globalModule^.modules);
    globalModule^.modules^.first := nil;
    globalModule^.modules^.last := nil;
    globalModule^.time := 0;
    currModule := globalModule;


    (* This is a super-global procedure that is "declared" inside
       globalModule.  It provides the executable parent for all
       initializations required by the separately compiled modules
       imported by the main compilation unit, as well as the main
       compilation unit's initialization. *)
    new(globalProc);
    globalProc^.name := nil;
    globalProc^.globalName := nil;
    globalProc^.procType := procTypeNode;
    globalProc^.doCheck := odd(genCheckFlag);
$if pascal then
    globalProc^.funcAssigned := false;
    globalProc^.returnVar := nil;
$end
    globalProc^.returnSeen := false;
    globalProc^.body := nil;
    globalProc^.code := AddToCodeList(nil,nil);
    globalProc^.enclosing := nil;
    globalProc^.enclosingModule := globalModule;
    globalProc^.tempMap := nil;
    globalProc^.scope := currScope;
    globalProc^.block := currScope^.block;
    globalProc^.displayLevel := 1;
    globalProc^.mem := InitAllocationNode();
    globalProc^.initFlagVar := nil;
    globalProc^.containsProcs := false;
$if pascal then
    globalProc^.containsFiles := false;
    globalProc^.OOBLabelList := nil;
    globalProc^.nextOOBIndex := 0;
    globalProc^.jmpBuf := nil;
$end
    globalProc^.containsUpLevel := DisplayLevelSet{};
    globalProc^.doesUpLevel := DisplayLevelSet{};
    globalProc^.next := nil;
    new(globalModule^.procs);
    globalModule^.procs^.first := globalProc;
    globalModule^.procs^.last := globalProc;
    currProc := globalProc;
    new(globalPortList);
    globalPortList^.first := nil;
    globalPortList^.last := nil;
    new(globalProc^.varList);
    globalProc^.varList^.first := nil;
    globalProc^.varList^.last := nil;

end InitGlobalModule;

$if pascal then
procedure DefineLabel(value : ConstNode);
var
    sym		: Symbol;
    name	: array [0..15] of char;
    nameString  : String;
    labelOrd    : cardinal;
begin
    if DEBUG and TraceDecls then
	Writef(output,'DefineLabel(%1.0F)\n', OrdOf(value));
    end;
    labelOrd := trunc(OrdOf(value));
    if (labelOrd < 1) or (labelOrd > NULLLABEL) then
	ErrorNumber('Label must be between 1 and %', NULLLABEL);
    end;
    SWritef(name, '%d', labelOrd);
    nameString := NewText(name);
    if DefineSymbol(sym, nameString, currScope, ONECASE, SYMLABEL) then
	new(sym^.symLabel);
	sym^.symLabel^.labelName := GlobalName(nameString, currModule,currProc);
	sym^.symLabel^.proc := currProc;
	sym^.symLabel^.OOBIndex := 0;
    else
	ErrorName(sym^.name, 'Label $ redefined');
    end;
end DefineLabel;
$end (* pascal *)

procedure DefineConst(name : String; value : ConstNode);
var
    sym : Symbol;
    error: boolean;
begin
    if DEBUG and TraceDecls then
	Writef(output,'DefineConst(');
	WriteString(output,name);
	Writec(output,'=');
	WriteConstant(output,value);
	Writef(output,')\n');
    end;
    if DefineSymbol(sym, name, currScope, ONECASE, SYMCONST) then
	sym^.symConst := value;
	if (currScope = currModule^.scope) and currModule^.exportAll 
		and currModule^.doingImport then
	    currModule^.qualExports := AddToIdentList(currModule^.qualExports,
				MakeIdent(name));
	end;
    else
	ErrorName(sym^.name,'Constant $ redefined');
    end;
end DefineConst;


procedure CheckEqualType(sym : Symbol; tn : TypeNode; portingSymbol : boolean);
var
    symTn : TypeNode;
begin
    symTn := StoredType(sym^.symType);
    tn := StoredType(tn);
(*  ||| If implementation did not automatically import that which was
        imported by the definition, a step I believe should be taken,
	then the "do nothing" case (and the portingSymbol parameter)
	should be eliminated, causing it to fall into the error else below. *)
    if (symTn = tn) and portingSymbol then
	(* do nothing *)
    elsif (symTn^.kind = DTRENAME) and (symTn^.renameType = nil) then
	symTn^.renameType := tn;
	symTn^.size := tn^.size;
	symTn^.alignment := tn^.alignment;
$if pascal then
	symTn^.containsFiles := tn^.containsFiles;
$end
	if symTn^.name = tn^.name then
	    symTn^.name := nil;
	    symTn^.theModule := nil;
	end;
    elsif symTn^.kind = DTOPAQUE then
	if tn^.size # symTn^.size then
	    ErrorName(sym^.name,
		'Size of actual type $ must be same as opaque type');
	end;
	symTn^.kind := DTRENAME;
	symTn^.renameType := tn;
$if pascal then
	symTn^.containsFiles := tn^.containsFiles;
$end
	if symTn^.name = tn^.name then
	    tn^.name := nil;
	    if tn^.theModule # nil then
		(* if there was a module affiliation, move it up *)
		symTn^.theModule := tn^.theModule;
		tn^.theModule := nil;
	    end;
	end;
    else
	ErrorName(sym^.name,'Type $ redefined');
    end;
end CheckEqualType;

procedure DefineType(name : String; value : TypeNode);
var
    sym : Symbol;
    tn, otn : TypeNode;
begin
    if DEBUG and TraceDecls then
	Writef(output,'DefineType(');
	WriteString(output,name);
	Writef(output,')\n');
    end;
    if (value # nil) and (value^.name = nil) then
	value^.name := name;
	if currModule^.kind = MODDEFINITION then
	    value^.theModule := currModule;
	end;
    end;
    if DefineSymbol(sym, name, currScope, ONECASE, SYMTYPE) then
	if value = nil then
	    (* opaque type *)
	    otn := NewTypeNode(DTOPAQUE);
	    otn^.size := WORDSIZE;
	    otn^.opaqueName := GlobalName(name,currModule,currProc);
	    otn^.name := name;
	    if currModule^.kind = MODDEFINITION then
		otn^.theModule := currModule;
	    end;
	    sym^.symType := otn;
	else
	    sym^.symType := value;
	end;
	if (currScope = currModule^.scope) and currModule^.exportAll 
		and currModule^.doingImport then
	    currModule^.qualExports := AddToIdentList(currModule^.qualExports,
				MakeIdent(name));
	end;
    elsif (sym^.kind = SYMTYPE) and (value # nil) then
        if (sym^.symType^.kind = DTRENAME) and (sym^.symType^.renameType = nil)
		and currModule^.exportAll
		and currModule^.doingImport then (* CED 3/16/88 *)
	    (* Defining a rename created by a pointer forward type, 
	       add to export list *)
	    currModule^.qualExports := 
		AddToIdentList(currModule^.qualExports, MakeIdent(name));
	end;
	CheckEqualType(sym,value,false);
    else
	ErrorName(name,'Type $ redefined');
    end;
    if DEBUG and TraceDecls then
	if value # nil then
	    Writef(output, 'size=%d, align=%d\n', value^.size, 
			value^.alignment);
	else
	    Writef(output,'value=nil\n');
	end;
    end;
end DefineType;


procedure AddToVarList(list : VarList; newOne : VarNode) : VarList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
    end;
    newOne^.next := nil;
    if list^.first = nil then
	list^.first := newOne;
    else
	list^.last^.next := newOne;
    end;
    list^.last:= newOne;
    return list;
end AddToVarList;

$if pascal then
procedure AddToFileVarList(list : FileVarList; newOne : FileVarNode) 
	: FileVarList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
    end;
    newOne^.next := nil;
    if list^.first = nil then
	list^.first := newOne;
    else
	list^.last^.next := newOne;
    end;
    list^.last:= newOne;
    return list;
end AddToFileVarList;
$end (* pascal *)

procedure DefineVarInProc(const name    : String;
			  const varType : TypeNode;
			  const mt      : MemoryType;
			  const global  : GlobalSymKind;
			  const initial : FormerNode;
			  const proc    : ProcNode) : VarNode;
var
    saveModule : ModuleNode;
    saveProc   : ProcNode;
    saveScope  : Scope;
    vn         : VarNode;
begin
    saveScope := currScope;
    saveProc := currProc;
    saveModule := currModule;
    currProc := proc;
    currScope := proc^.scope;
    currModule := proc^.enclosingModule;
    vn := DefineVar(name, varType, MEMNORMAL, global, initial);
    currModule := saveModule;
    currProc := saveProc;
    currScope := saveScope;
    return vn;
end DefineVarInProc;

procedure DefineVar(const name       : String;
		          varType    : TypeNode;
		    const mt	     : MemoryType;
		    const global     : GlobalSymKind;
		    const initial    : FormerNode): VarNode;
var
    sym	       : Symbol;
    vn	       : VarNode;
    atn	       : TypeNode;
    globalName : String;
begin
    if DEBUG and TraceDecls then
	Writef(output,'DefineVar(');
	WriteString(output,name);
	Writef(output,')\n');
    end;
    atn := ActualType(varType);
    if atn # nil then
	if (atn^.kind = DTARRAY) and (atn^.arrayKind # ARRAYNORMAL) and
		(mt # MEMPARAM) then
	    ErrorName(name,
	       'Open array type is valid only for parameters, not variable $');
	end;
$if pascal then
	if atn^.containsFiles then
	    currProc^.containsFiles := true;
	end;
	if target # TARGETVAX then
	    (* Ignore size, alignment stuff...this is procedure data. *)
	    varType := atn;
	end;
$end
    end;
    new(vn);
    vn^.varType := varType;
    vn^.kind := VARNORMAL;
    vn^.indirect := false;
    vn^.varParam := false;
    vn^.name := name;
    if name # nil then
	currProc^.varList := AddToVarList(currProc^.varList,vn);
    end;
    if (varType = nil) then
	(* don't allocate memory - error back there somewhere *)
	vn^.address.kind := MEMNONE;
    elsif currProc^.inlineProc then
        (* don't allocate memory yet *)
        vn^.address.kind := MEMINLINE;
        if mt = MEMPARAM then
	    vn^.address.inlineVal.kind := IPPARAM;
        else
            vn^.address.inlineVal.kind := ILVAR;
        end;
    elsif (name # nil) and (currProc = globalProc) and
	    (mt in MemoryTypeSet{MEMNORMAL,MEMFAST}) then
	if global = GSNORMAL then
	    globalName := GlobalName(name, currModule, currProc);
	else
	    globalName := name;
	end;
	AllocateGlobal(globalName, varType^.size, global,
		       (* defineMemory *) currModule^.kind # MODDEFINITION,
		       initial, vn^.address);
	vn^.address.gvn^.used := currModule^.kind in
			    ModuleKindSet{MODIMPLEMENTATION, MODPROGRAM};
    elsif (mt = MEMNORMAL) and (varType^.size <= WORDSIZE) and
	    (varType^.kind in DataTypeSet{DTPOINTER,DTINTEGER,DTBOOLEAN,
		DTCHAR,DTREAL, DTSET,DTCARDINAL,DTBYTE,DTWORD,DTSUBRANGE,
		DTENUMERATION,DTOPAQUE}) then
	(* try fast memory *)
	AllocateMemory(currProc^.mem,MEMFAST,SizeOf(varType),
		WORDSIZE,currProc,vn^.address);
	if initial # nil then
	    ErrorName(name,'$ is not static; initial value not allowed');
        end;
    else
	AllocateMemory(currProc^.mem,mt,SizeOf(varType),
		WORDSIZE,currProc,vn^.address);
	if initial # nil then
	    ErrorName(name,'$ is not static; initial value not allowed');
        end;
    end;
    if name # nil then
	if DefineSymbol(sym,name,currScope,ONECASE,SYMVAR) then
	    sym^.symVar := vn;
	if (currScope = currModule^.scope) and currModule^.exportAll 
		and currModule^.doingImport then
		currModule^.qualExports := 
		    AddToIdentList(currModule^.qualExports, MakeIdent(name));
	    end;
	else
            ErrorName(sym^.name,'Redefined variable $');
	end;
    end;
    return vn;
end DefineVar;

procedure DefineVarList(idList      : IdentList; 
		        varType     : TypeNode;
			global      : Token;
			shared      : Token;
			initial     : FormerElementList;
			externName  : ConstNode);
var
    id  : IdentNode;
    sym : Symbol;
    vn  : VarNode;
    gsk : GlobalSymKind;
    init : FormerNode;
begin
    if global = TKEXTERNAL then
	gsk := GSEXTERNAL;
    elsif global = TKGLOBAL then
	gsk := GSGLOBAL;
    else
	gsk := GSNORMAL;
    end;
$if modula2 then
    if initial # nil then
        init := MakeFormer(varType,initial);
    else
        init := nil;
    end;
$else (* pascal *)
    init := nil;
$end
    if (externName # nil) and (idList^.first # idList^.last) then
        Error('Cannot specify global name for list of variables');
	externName := NIL;
    end;
    id := idList^.first;
    while id # nil do
	vn := DefineVar(id^.name, varType, MEMNORMAL, gsk, init);
        if shared = TKSHARED then
            if vn^.address.kind = MEMGLOBAL then
                vn^.address.gvn^.shared := true;
            else
                ErrorName(id^.name,
		    '$ is not a static variable and cannot be SHARED');
            end;
	elsif externName # nil then
	    if vn^.address.kind = MEMGLOBAL then
		vn^.address.gvn^.globalName := externName^.strVal;
	    else
		ErrorName(id^.name,
		   '$ is not a static variable and cannot have a global name');
	    end;
	end;
	id := id^.next;
    end;
end DefineVarList;

procedure DefineModule(name : String; kind : Token) : ModuleNode;
var
    mn      : ModuleNode;
    sym     : Symbol;
    symCase : SymbolCase;
    i       : cardinal;
begin
$if pascal then
    if name = nil then  (* no program heading...just decls and procs *)
	name := rootFile;
	inputDeclared := true;
	outputDeclared := true;
    end;
$end (* pascal *)
    if DEBUG and ('i' in debugSet) then
	Writef(output,'DefineModule ');
	WriteString(output,name);
	Writef(output,' : ');
	WriteString(output,currFile);
	Writec(output, '\n');
    end;
    new(mn);
    mn^.enclosing := currModule;
    mn^.enclosingProc := currProc;
    mn^.name := name;
    new(mn^.procs);
    mn^.procs^.first := nil;
    mn^.procs^.last := nil;
    new(mn^.modules);
    mn^.modules^.first := nil;
    mn^.modules^.last := nil;
    mn^.unqualExports := nil;
    mn^.qualExports := nil;
    new(mn^.imports);
    mn^.imports^.first := nil;
    mn^.imports^.last := nil;
    mn^.imported := nil;
    mn^.doingImport := false;
    mn^.containsInline := false;
    mn^.noinit := currModule^.noinit and (currProc = currModule^.enclosingProc);
    mn^.doinit := false;
    mn^.exportAll := standardExportFlag;
    mn^.next := nil;
    if currModule^.modules^.first = nil then
        currModule^.modules^.first := mn;
    else
        currModule^.modules^.last^.next := mn;
    end;
    currModule^.modules^.last:= mn;
    if (kind = TKBEGIN)  then   (* kind = TKBEGIN for builtin modules *)
        if standardKeywordFlag then
	    symCase := ONECASE;
	else
	    symCase := ANYCASE;
	end;
	mn^.time := 0;
    else
        symCase := ONECASE;
	mn^.time := LastModifiedTime();
    end;
    if DefineSymbol(sym,name,currScope,symCase,SYMMODULE) then
	sym^.symModule := mn;
	if (currScope = currModule^.scope) and currModule^.exportAll
	    and currModule^.doingImport then
	    currModule^.qualExports := AddToIdentList(currModule^.qualExports,
				MakeIdent(name));
	end;
    else
	ErrorName(name,'Module name $ redefined');
    end;
    if currProc^.inlineProc then
        Error('Cannot define a module in an inline procedure');
    end;
    currModule := mn;
    mn^.scope := StartScope(false);
    (* create scope for qualified exports *)
    mn^.exportScope := StartScope(false);
    EndScope;

    case kind of
    | TKMODULE :
	mn^.kind := MODPROGRAM;
	if mn^.enclosing = globalModule then
	    (* program module, use scope for global proc *)
	    globalProc^.scope := mn^.scope;
	    globalProc^.block := mn^.scope^.block;
	    globalProc^.enclosingModule := mn;
	    compileModuleName := name;
	end;
    
    | TKIMPLEMENTATION :
	mn^.kind := MODIMPLEMENTATION;
	globalProc^.scope := mn^.scope;
	globalProc^.block := mn^.scope^.block;
	if mn^.enclosing # globalModule then
	    ErrorName(mn^.name,'Implementation module $ must not be nested');
	else
	    globalProc^.enclosingModule := mn;
$if modula2 then
	    globalProc^.name := NonHashText(MODULEINITNAME);
	    globalProc^.globalName := GlobalName(globalProc^.name,mn,nil);
	    globalProc^.initFlagVar := 
		    DefineVar(NonHashText(MODULEINITFLAG), booleanTypeNode,
					    MEMNORMAL, GSNORMAL, nil);
$else
	    globalProc^.name := name;
	    globalProc^.globalName := GlobalName(globalProc^.name, mn, nil);
$end
	    compileModuleName := name;
	end;

$if modula2 then
    | TKBEGIN, TKDEFINITION :
	mn^.kind := MODDEFINITION;
	mn^.doingImport := true;
$end (* modula2 *)	
    end (* case *);
    return mn;
end DefineModule;

$if modula2 then
procedure SetNoInit();
begin
    currModule^.noinit := true;
end SetNoInit;
$end


$if pascal then
procedure ProcessFileList(fileNameList : IdentList);
   var ident : IdentNode;
begin
    ident := fileNameList^.first;
    while ident # nil do 
	if ident^.name = outputString then
	    outputDeclared := true;
	elsif ident^.name = inputString then
	    inputDeclared := true;
	end;
	ident := ident^.next;
    end;
    programFileNameList := fileNameList;
end ProcessFileList;
$end (* pascal *)

procedure GlobalPort(sym : Symbol;
		     mn, tomn : ModuleNode;
		     isQualified : boolean);
var
    pn : PortNode;
begin
    if (sym # nil) and ((mn^.kind = MODIMPLEMENTATION) or
	    (tomn^.kind in ModuleKindSet{MODIMPLEMENTATION,MODPROGRAM}))
    then
	new(pn);
	pn^.sym := sym;
	pn^.theModule := mn;
	pn^.isQualified := isQualified;
	pn^.isExport := mn^.kind = MODIMPLEMENTATION;
	pn^.extern := GSNORMAL;
	if sym^.kind = SYMPROC then
	    pn^.extern := sym^.symProc^.extern;
	elsif sym^.kind = SYMVAR then
	    if sym^.symVar^.address.kind = MEMGLOBAL then
		pn^.extern := sym^.symVar^.address.gvn^.extern;
                sym^.symVar^.address.gvn^.used := true;
	    end;
	end;
        pn^.next := nil;
        if globalPortList^.first = nil then
            globalPortList^.first := pn;
        else
            globalPortList^.last^.next := pn;
        end;
        globalPortList^.last:= pn;
	if DEBUG and ('g' in debugSet) then
	    Writef(output,'GlobalPort: ');
	    WriteString(output,mn^.name);
	    Writec(output,'.');
	    WriteString(output,sym^.name);
	    Writef(output,' %n to ', sym^.kind);
	    WriteString(output,tomn^.name);
	    Writec(output, '\n');
	end;
        if mn^.kind = MODDEFINITION then
	    mn^.doinit := true;
	end;
    end;
end GlobalPort;

procedure ExportExternalProcs(mn : ModuleNode);
var
    sym : Symbol;
    id : IdentNode;
begin
    (* check qualified exports for procedures or modules *)
    if mn^.qualExports # nil then
	id := mn^.qualExports^.first;
	while id # nil do
	    sym := LookUpSymbol(id^.name,currScope);
	    if sym = nil then
		(* error, but already found *)
	    elsif sym^.kind = SYMPROC then
		sym^.symProc^.internalProc := false;
	    elsif sym^.kind = SYMMODULE then
		ExportExternalProcs(sym^.symModule);
	    end;
	    GlobalPort(sym,mn,globalModule,false);
	    id := id^.next;
	end;
    end;

    (* check unqualified exports for procedures or modules *)
    if mn^.unqualExports # nil then
	id := mn^.unqualExports^.first;
	while id # nil do
	    sym := LookUpSymbol(id^.name,currScope);
	    if sym = nil then
		(* error, but already found *)
	    elsif sym^.kind = SYMPROC then
		sym^.symProc^.internalProc := false;
	    elsif sym^.kind = SYMMODULE then
		ExportExternalProcs(sym^.symModule);
	    end;
	    GlobalPort(sym,mn,globalModule,false);
	    id := id^.next;
	end;
    end;
end ExportExternalProcs;

$if modula2 then
procedure ProcessExports(mn : ModuleNode);
var
    sym, nsym : Symbol;
    id : IdentNode;
    exportScope, enclScope : Scope;
begin
    if DEBUG and ('i' in debugSet) then
	Writef(output, 'ProcessExports ');
	WriteString(output, mn^.name);
	Writef(output, ' : ');
	WriteString(output, currFile);
	Writec(output, '\n');
    end;
    exportScope := mn^.exportScope;
    (* export qualified exports to module export scope *)
    if mn^.qualExports # nil then
	id := mn^.qualExports^.first;
	while id # nil do
	    sym := LookUpSymbol(id^.name,currScope);
	    if sym = nil then
		ErrorName(id^.name,'Exported identifier $ not found in module');
		if DefineSymbol(sym, id^.name, currScope, ONECASE, SYMANY) then
		end;
	    end;
	    if sym^.kind = SYMFIELD then
		ErrorName(id^.name, 'Cannot export field $');
	    else
		if mn^.enclosing = globalModule then
		    if sym^.kind = SYMPROC then
			sym^.symProc^.internalProc := false;
		    elsif sym^.kind = SYMMODULE then
			ExportExternalProcs(sym^.symModule);
		    end;
		    GlobalPort(sym,mn,globalModule,true);
		end;
		nsym := Port(sym,exportScope);
		if DEBUG and TraceDecls then
		    Writef(output,'Export ');
		    WriteString(output,sym^.name);
		    Writec(output, '\n');
		end;
	    end;
	    id := id^.next;
	end;
    end;

    (* export unqualified exports to enclosing scope and module export scope *)
    if mn^.unqualExports # nil then
	enclScope := currScope^.enclosing;

	id := mn^.unqualExports^.first;
	while id # nil do
	    sym := LookUpSymbol(id^.name,currScope);
	    if sym = nil then
		ErrorName(id^.name,'Exported identifier $ not found in module');
		if DefineSymbol(sym, id^.name, currScope, ONECASE, SYMANY) then
		end;
	    end;
	    if sym^.kind = SYMFIELD then
		ErrorName(id^.name, 'Cannot export field $');
	    else
		nsym := Port(sym,exportScope);
		if DEBUG and TraceDecls then
		    Writef(output,'Export ');
		    WriteString(output,sym^.name);
		    Writec(output, '\n');
		end;
		nsym := Port(sym,enclScope);

		if mn^.enclosing = globalModule then
		    if sym^.kind = SYMPROC then
			sym^.symProc^.internalProc := false;
		    elsif sym^.kind = SYMMODULE then
			ExportExternalProcs(sym^.symModule);
		    end;
		    GlobalPort(sym,mn,globalModule,false);
		end;
	    end;
	    id := id^.next;
	end;
    end;
end ProcessExports;
$end (* modula2 *)

procedure EndModule(mn : ModuleNode; body : StmtList; name : String);
var
    code	: CodeNode;
$if pascal then
    ident       : IdentNode;
    sym		: Symbol;
    fvn		: FileVarNode;
$end

begin

    if DEBUG and ('i' in debugSet) then
	Writef(output,'EndModule ');
	WriteString(output,mn^.name);
	Writef(output,' : ');
	WriteString(output,currFile);
	Writec(output, '\n');
    end;
    if mn^.kind # MODIMPLEMENTATION then
$if modula2 then
	ProcessExports(mn);
$else
	assert(mn^.qualExports = nil);
	assert(mn^.unqualExports = nil);
	(* Insure all files in PROGRAM heading are global file vars. *)
	if (programFileNameList # nil) then
	    ident := programFileNameList^.first;
	    while ident # nil do
		if (ident^.name = inputString) or 
			(ident^.name = outputString) then
		    (* skip it *)
		else
		    sym := LookUpSymbol(ident^.name, globalProc^.scope);
		    if sym = nil then
			ErrorName(ident^.name,
			    'File $ in program heading never declared');
		    elsif sym^.kind = SYMANY then
			(* error alread generated *)
		    elsif sym^.kind # SYMVAR then
			ErrorName(ident^.name,
			    'File $ in program heading must be declared a variable');
		    elsif (sym^.symVar^.varType = nil) or 
			    not (sym^.symVar^.varType^.kind in 
				DataTypeSet{DTANY, DTFILE}) then
			ErrorName(ident^.name,
			    'File $ in program heading must be declared a file');
		    else (* Got a good one *)
			new(fvn);
			fvn^.fileVar := sym^.symVar;
			programFileVarList := 
				AddToFileVarList(programFileVarList, fvn);
		    end;
		end;
		ident := ident^.next;
	    end (* while *);
	end (* if *);
$end
    end;
    if mn^.noinit and (body # nil) and (body^.first # nil) then
        ErrorName(mn^.name,
	          'Module initialization specified for NOINIT module $');
    end;
    mn^.body := body;
    mn^.doingImport := false;

    (* put code for this module on enclosing procedures code list *)
    new(code);
    code^.stmts := body;
    currProc^.code := AddToCodeList(currProc^.code,code);
    EndScope;
    currModule := mn^.enclosing;
    if (name # nil) and (name # mn^.name) then
	ErrorName(mn^.name,'Name of module $ does not appear on end');
    end;
end EndModule;

$if modula2 then
procedure GetInlineImpl(const mn : ModuleNode);
(* mn is a definition module.  If it contained inline declarations, find and
   start parsing the corresponding implementation file. *)
begin
    if mn^.containsInline then
	(* need to scan implementation module for inline procedures *)
	if DEBUG and TraceDecls then
	    Writef(output,' Looking for file\n');
	end;

	assert((currProc = globalProc) and (mn^.kind = MODDEFINITION),
	    'Trying to get inline implementation for wrong module');
	(* Look for implementation module *)
	if InitFile(mn^.name, MODIMPLEMENTATION,
		'Cannot find file $ for module with inline procedures') then
	    (* will parse implementation module *)
	    mn^.doingImport := false;
	end;
    end;
end GetInlineImpl;

procedure EndImplDef(name : String);
begin
    ProcessExports(currModule);
    if (name # nil) and (name # currModule^.name) then
	ErrorName(currModule^.name,'Name of module $ does not appear on end');
    end;
    (* All done reading this module's corresponding definition... CED 3/15/88 *)
    currModule^.doingImport := false;
end EndImplDef;
    
procedure ContinueModule(name : String) : ScanModuleNode;
var
    sym : Symbol;
    mn  : ModuleNode;
    smn : ScanModuleNode;
begin
    if DEBUG and ('i' in debugSet) then
        Writef(output,'ContinueModule ');
        WriteString(output,name);
        Writef(output,' : ');
        WriteString(output,currFile);
        Writec(output, '\n');
    end;
    new(smn);
    mn := nil;
    sym := LookUpSymbol(name,globalModule^.scope);
    if sym = nil then
        ErrorName(name,'ContinueModule: module $ unknown?');
    elsif sym^.kind # SYMMODULE then
        ErrorName(name,'ContinueModule: name $ not a module?');
    elsif sym^.symModule^.kind # MODDEFINITION then
        ErrorName(name,'ContinueModule: module $ not definition?');
    else
        mn := sym^.symModule;
        smn^.theModule := mn;
        smn^.saveScope := currScope;
        smn^.saveModule := currModule;
        currScope := mn^.scope;
        currModule := mn;
    end;
    return smn;
end ContinueModule;

procedure DisContinueModule(smn  : ScanModuleNode;
			    body : StmtList;
			    name : String);
var
    code : CodeNode;
    mn : ModuleNode;
begin
    mn := smn^.theModule;
    if DEBUG and ('i' in debugSet) then
        Writef(output,'DisContinueModule ');
        WriteString(output,name);
        Writef(output,' : ');
        WriteString(output,currFile);
        Writec(output, '\n');
    end;
    mn^.body := body;
    mn^.doingImport := false;

    EndScope;
    currScope := smn^.saveScope;
    currModule := smn^.saveModule;
    if (name # nil) and (name # mn^.name) then
        ErrorName(mn^.name,'Name of module $ does not appear on end');
    end;
end DisContinueModule;


procedure ExportAll;
begin
    currModule^.exportAll := true;
end ExportAll;

procedure ExportMissing();
begin
    if (currModule^.enclosing = globalModule) then
	currModule^.exportAll := true;
    end;
end ExportMissing;


procedure GetDefinitionModule(mn : ModuleNode);
begin
    if mn # nil then
	(* read in the definition module *)
	(* continue parsing with file *)
	if not InitFile(mn^.name, MODDEFINITION, 
	       'Cannot find definition module $ for implementation module') then
	    ExitProgram(101);
	end;
	if DEBUG and ('i' in debugSet) then
	    Writef(output,'GetDefinitionFile ');
	    WriteString(output,mn^.name);
	    Writef(output,' : ');
	    WriteString(output,currFile);
	    Writec(output, '\n');
	end;
	(* In case this is read again by accident ... CED 3/15/88 *)
	mn^.doingImport := true;
    end;
end GetDefinitionModule;

procedure SetGlobalProcName(const proc : ProcNode;
			const name : ConstNode);
begin
    assert(name^.kind = DTSTRING);
    proc^.globalName := name^.strVal;
end SetGlobalProcName;

$end (* modula2 *)

procedure DefineProc(name : String; global : Token): ProcNode;
var
    pn : ProcNode;
    sym : Symbol;
begin
    if DefineSymbol(sym,name,currScope,ONECASE,SYMPROC) then
	currProc^.containsProcs := true;
	new(pn);
	pn^.fileName := currFile;
	pn^.lineNumber := currLine;
	pn^.name := name;
	pn^.inlineProc := false;
	pn^.checked := false;
$if modula2 then
	case global of
	| TKEXTERNAL :
	    pn^.globalName := name;
	    pn^.extern := GSEXTERNAL;
	| TKGLOBAL :
	    pn^.globalName := name;
	    pn^.extern := GSGLOBAL;
	| TKATASM :
	    pn^.globalName := GlobalName(name, currModule, currProc);
	    pn^.extern := GSASM;
        | TKINLINE :
            pn^.globalName := GlobalName(name, currModule, currProc);
            pn^.extern := GSINLINE;
            pn^.inlineProc := true;
            currModule^.containsInline := true;
	| else
	    pn^.globalName := GlobalName(name, currModule, currProc);
	    pn^.extern := GSNORMAL;
	end;
$else (* pascal *)
	pn^.globalName := GlobalName(name, currModule, currProc);
	pn^.extern := GSNORMAL;
	pn^.funcAssigned := false;
	pn^.returnVar := nil;
$end
	pn^.returnSeen := false;
	if currProc^.inlineProc then
            Error('Cannot define a procedure in an inline procedure');
        end;
	pn^.procType := nil;
	pn^.doCheck := odd(genCheckFlag);
	pn^.builtin := BIPNOTBIP;
	pn^.body := nil;
	pn^.code := AddToCodeList(nil,nil);

	if (currScope = currModule^.scope) and currModule^.exportAll
		and currModule^.doingImport then (* CED 3/16/88 *)
	    currModule^.qualExports := AddToIdentList(currModule^.qualExports,
			    MakeIdent(name));
	end;
	pn^.scope := StartScope(true);
	pn^.block := pn^.scope^.block;
	pn^.mem := InitAllocationNode();
	pn^.displayLevel := currProc^.displayLevel+1;
	pn^.enclosing := currProc;
	pn^.enclosingModule := currModule;
	pn^.containsProcs := false;
$if pascal then
	pn^.containsFiles := false;
	pn^.OOBLabelList := nil;
	pn^.nextOOBIndex := 0;
	pn^.jmpBuf := nil;
$end
	pn^.internalProc := (pn^.extern = GSNORMAL) and
				(currModule^.kind # MODDEFINITION);
	pn^.tailRecursion := false;
	pn^.tempMap := nil;
	new(pn^.varList);
	pn^.varList^.first := nil;
	pn^.varList^.last := nil;
	currProc := pn;
	sym^.symProc := pn;

    else (* name is already declared *)
	if sym^.kind # SYMPROC then
	    ErrorName(name,'Symbol $ redefined');
	    pn := nil;
	elsif sym^.symProc^.builtin # BIPNOTBIP then
	    ErrorName(name,'Builtin procedure $ redefined');
	    pn := nil;
	elsif (sym^.symProc^.body # nil)
		or (currModule^.doingImport) then
	    ErrorName(sym^.symProc^.name,'Procedure $ redefined');
	    pn := nil;
	elsif global = TKATASM then
	    ErrorName(sym^.symProc^.name, 'ASM procedure $ cannot be defined');
	elsif standardExportFlag and
		(sym^.symProc^.enclosingModule # globalProc^.enclosingModule) then
	    ErrorName(sym^.symProc^.name,
		    'Cannot redefine imported procedure $');
	    pn := nil;
	else
	    if (sym^.symProc^.name # sym^.symProc^.globalName)
		    and (global = TKEXTERNAL) then
		ErrorName(sym^.symProc^.name,
		    'EXTERNAL for $ must be specified in definition module');
	    end;
	    if not sym^.symProc^.inlineProc and (global = TKINLINE) then
                ErrorName(sym^.symProc^.name,
		    'INLINE for $ must be specified in definition module');
            end;
	    if sym^.symProc^.inlineProc and (global # TKINLINE) then
		ErrorName(sym^.symProc^.name,
		    'INLINE was specified for $ in definition module');
	    end;
	    pn := sym^.symProc;
	    (* put procedure in proper scope nesting *)
	    pn^.scope^.enclosing := currScope;
	    currScope := pn^.scope;
	    currProc := pn;
	    pn^.fileName := currFile;
	    pn^.lineNumber := currLine;
	end;
    end;
    return pn;
end DefineProc;

procedure AddTypeToProc(proc : ProcNode; procType : TypeNode): ProcNode;
var
    atn : TypeNode;
    param : ParamNode;
begin
    if proc = nil then
	(* do nothing *)
    elsif proc^.procType # nil then
$if modula2 then
	CheckEqualProcType(proc,procType);
	(* Assume implementation header matches code *)
	proc^.procType := procType;
$else (* pascal *)
	if procType # procTypeNode then
	    ProcErrorName(proc, proc^.name,
		'No parameters allowed on definition of forward procedure $');
	end;
$end
    else
	proc^.procType := procType;
	if procType^.paramList # nil then
	    param := procType^.paramList^.first;
	    while param # nil do
		atn := ActualType(param^.paramType);
$if pascal then
		if target # TARGETVAX then
		    (* Ignore all size, alignment info *)
		    param^.paramType := atn;
		end;
$end
		if (atn # nil) and (atn^.kind = DTARRAY) and
			(atn^.arrayKind = ARRAYSUBARRAY) and
			(param^.kind = PARAMARRAYVALUE) then
		    ErrorName(param^.name,
			'SUBARRAY parameter $ must be passed by VAR or CONST');
		end;
		param := param^.next;
	    end;
	end;
    end;
    return proc;
end AddTypeToProc;

$if pascal then
procedure MakeExternal(proc : ProcNode);
begin
    proc^.extern := GSEXTERNAL;
    proc^.internalProc := false;
end MakeExternal;
$ end (* pascal *)


procedure EndProc(proc : ProcNode; body : StmtList; name : String);
var
    code : CodeNode;
    param : ParamNode;
    vn : VarNode;
begin
    if proc = nil then
	(* do nothing *)
    else
	if body # nil then
	    proc^.body := body;
	    proc^.time := LastModifiedTime();
	    proc^.next := nil;
            if currModule^.procs^.first = nil then
                currModule^.procs^.first := proc;
            else
                currModule^.procs^.last^.next := proc;
            end;
            currModule^.procs^.last:= proc;
	    new(code);
	    code^.stmts := body;
	    proc^.code := AddToCodeList(proc^.code,code);
$if pascal then
	    if proc^.procType^.funcType # nil then
		(* Allocate space to return function value *)
		proc^.returnVar := DefineVar(NonHashText("'returnValue"), 
		    proc^.procType^.funcType, MEMNORMAL, GSNORMAL, nil);
	    end;
$end
	    (* allocate variables for parameters *)
	    if proc^.procType^.paramList # nil then
		param := proc^.procType^.paramList^.first;
		while param # nil do
		    case param^.kind of
		    | PARAMVAR  :
			(* reference parameter: allocate address *)
			vn := DefineVar(param^.name,addressTypeNode,
					    MEMPARAM,GSNORMAL, nil);
			vn^.varType := param^.paramType;
			vn^.indirect := true;
			vn^.varParam := true;
		    
		    | PARAMVALUE,PARAMCONST :
			if param^.reference then
			    (* multiword parameter: allocate address *)
			    vn := DefineVar(param^.name,addressTypeNode,
				    MEMPARAM,GSNORMAL, nil);
			    vn^.varType := param^.paramType;
			    vn^.indirect := true;
			else
			    vn := DefineVar(param^.name,param^.paramType,
				    MEMPARAM,GSNORMAL, nil);
			end;
			if param^.kind = PARAMCONST then
			    vn^.kind := VARCONSTPARAM;
			end;
		    
		    | PARAMARRAYVAR, PARAMARRAYVALUE, PARAMARRAYCONST :
			vn := DefineVar(param^.name,param^.paramType,
					    MEMPARAM,GSNORMAL, nil);
			if param^.kind = PARAMARRAYCONST then
			    vn^.kind := VARCONSTPARAM;
			elsif param^.kind = PARAMARRAYVAR then
			    vn^.varParam := true;
			end;
			
		    end (* case *);
		    param^.paramVar := vn;
		    param := param^.next;
		end;
	    end;
	end (* if proc^.body # nil *);
	if (name # nil) and (name # proc^.name) then
	    ErrorName(proc^.name,'Name of procedure $ does not appear on end');
	end;
	currProc := currProc^.enclosing;
	EndScope;
    end;
end EndProc;

procedure CheckProc(pn : ProcNode);
var
    saveScope : Scope;
    saveProc : ProcNode;
    saveExitStatus : ExitStatus;
    param : ParamNode;
    vn : VarNode;
    saveNumberOfErrors : cardinal;
begin
    saveScope := currScope;
    saveProc := currProc;
    saveExitStatus := exitStatus;
    saveNumberOfErrors := numberOfErrors;
    currScope := pn^.scope;
    currProc := pn;
    exitStatus.loopActive := false;
    exitStatus.forActive := false;
    exitStatus.whileActive := false;
    exitStatus.repeatActive := false;
    exitStatus.innermostLoop := STMTNONE;

    if pn^.checked then
        (* no need to check it *)
    elsif pn^.beingChecked then
        ErrorName(pn^.name,'Recursive inline procedure $');
    elsif pn^.body # nil then
	if DEBUG and ('c' in debugSet) then
	    Writef(output,'Unchecked statements for procedure ');
	    WriteString(output,pn^.name);
	    Writec(output, '\n');
	    PrintStmtList(pn^.body,0);
	end;
	pn^.beingChecked := true;
	CheckStmtList(pn^.body);
	if pn^.procType^.funcType # nil then
	    CheckReturn(pn);
	end;
	if DEBUG and ('c' in debugSet) then
	    Writef(output,'Checked statements for procedure ');
	    WriteString(output,pn^.name);
	    Writec(output, '\n');
	    PrintStmtList(pn^.body,0);
	end;
	currFile := pn^.fileName;
	currLine := pn^.lineNumber;
        pn^.beingChecked := false;
    end;
    pn^.checked := true;
    if numberOfErrors # saveNumberOfErrors then
	(* Keep bad inline proc from being expanded *)
	pn^.inlineProc := false;
    end;
    currScope := saveScope;
    currProc := saveProc;
    exitStatus := saveExitStatus;
end CheckProc;

procedure CheckModule(mn : ModuleNode);
var
    submn : ModuleNode;
    pn : ProcNode;
    saveScope : Scope;
    saveModule : ModuleNode;
    imp : ImportNode;
begin
    saveModule := currModule;
    saveScope := currScope;
    currModule := mn;
    currScope := mn^.scope;

    (* look for any unresolved imports *)
    imp := mn^.imports^.first;
$if modula2 then
    while imp # nil do
	ProcessImport(imp,true);
	imp := imp^.next;
    end;
$else
    assert(imp = nil);
$end
    submn := mn^.modules^.first;
    while submn # nil do
	CheckModule(submn);
	submn := submn^.next;
    end;
    pn := mn^.procs^.first;
    while pn # nil do
	if not pn^.checked then
	    CheckProc(pn);
	end;
	pn := pn^.next;
    end;
    if mn^.body # nil then
	if DEBUG and ('c' in debugSet) then
	    Writef(output,'Unchecked statements for module ');
	    WriteString(output,mn^.name);
	    Writec(output, '\n');
	    PrintStmtList(mn^.body,0);
	end;
	CheckStmtList(mn^.body);
	if DEBUG and ('c' in debugSet) then
	    Writef(output,'Checked statements for module ');
	    WriteString(output,mn^.name);
	    Writec(output, '\n');
	    PrintStmtList(mn^.body,0);
	end;
    end;
    currModule := saveModule;
    currScope := saveScope;
end CheckModule;

procedure MakeParamList(kindToken : Token; idents : IdentList;
	paramType : TypeNode) : ParamList;
var
    pl : ParamList;
    pn : ParamNode;
    id : IdentNode;
    kind : ParamKind;
    reference : boolean;
begin
    if kindToken = TKVAR then
	kind := PARAMVAR;
	if (paramType^.kind = DTARRAY) and
		(paramType^.arrayKind # ARRAYNORMAL) then
	    kind := PARAMARRAYVAR;
	end;
	reference := true;
$if modula2 then
    elsif kindToken = TKCONST then
	kind := PARAMCONST;
	if (paramType^.kind = DTARRAY) and
		(paramType^.arrayKind # ARRAYNORMAL) then
	    kind := PARAMARRAYCONST;
	end;
	reference := ReferenceByPoint(paramType);
$end (* modula2 *)
    else
	kind := PARAMVALUE;
$if modula2 then
	if (paramType^.kind = DTARRAY) and
		(paramType^.arrayKind # ARRAYNORMAL) then
	    kind := PARAMARRAYVALUE;
	    if paramType^.arrayKind = ARRAYNOCOUNT then
		Error('@NOCOUNT parameter must be passed by VAR or CONST');
	    end;
	end;
$else (* pascal *)
	if paramType^.containsFiles then
	    Error('Files cannot be passed by value');
	end;
$end
	reference := ReferenceByPoint(paramType);
    end;
    (* put first param on list. *)
    (* Note: this works even if idents is nil, as in proc type definition *)
    new(pn);
    if idents = nil then
	pn^.name := nil;
    else
	pn^.name := idents^.first^.name;
    end;
    pn^.kind := kind;
    pn^.paramType := paramType;
    pn^.next := nil;
    pn^.reference := reference;
    new(pl);
    pl^.first := pn;
    pl^.last := pn;
    if idents # nil then
	(* do additional parameters, if more idents *)
	id := idents^.first^.next;
	while id # nil do
	    new(pn);
	    pn^.name := id^.name;
	    pn^.kind := kind;
	    pn^.paramType := paramType;
	    pn^.next := nil;
	    pn^.reference := reference;
	    pl^.last^.next := pn;
	    pl^.last := pn;
	    id := id^.next;
	end;
    end;
    return pl;
end MakeParamList;

procedure AppendParamList(some, more : ParamList) : ParamList;
begin
    if (some = nil) or (some^.first = nil) then
	return more;
    elsif (more = nil) or (more^.first = nil) then
	(* nothing to do *)
    else
	some^.last^.next := more^.first;
	some^.last := more^.last;
    end;
    return some;
end AppendParamList;

procedure ProcType(paramList : ParamList; funcType : TypeNode) : TypeNode;
var
    tn : TypeNode;
    num : integer;
    pn : ParamNode;
begin
    if (paramList = nil) and (funcType = nil) then 
        return procTypeNode;
    end;
    tn := NewTypeNode(DTPROC);
    tn^.size := WORDSIZE;
    tn^.paramList := paramList;
    tn^.funcType := funcType;
    num := 0;
    if paramList # nil then
	pn := paramList^.first;
	while pn # nil do
	    num := num + 1;
	    pn := pn^.next;
	end;
    end;
    tn^.numParams := num;
    return tn;
end ProcType;

procedure OpaqueWithSize(name : String; size : ConstNode) : TypeNode;
var
    tn : TypeNode;
begin
    tn := NewTypeNode(DTOPAQUE);
    tn^.size := trunc(OrdOf(size));
    if tn^.size <= 0 then
	Error('Invalid size specified');
	tn^.size := WORDSIZE;
    end;
    tn^.opaqueName := GlobalName(name,currModule,currProc);
    tn^.name := name;
    if currModule^.kind = MODDEFINITION then
	tn^.theModule := currModule;
    end;
    return tn;
end OpaqueWithSize;

procedure MakeIdent (name : String) : IdentNode;
var
    idn : IdentNode;
begin
    new(idn);
    idn^.name := name;
    return idn;
end MakeIdent;

$if modula2 then
procedure ImportDecl(fromIdent : String; idents : IdentList) : ImportNode;
var
    imp : ImportNode;
begin
    if DEBUG and TraceDecls then
	Writef(output,'Import: start\n');
    end;
    new(imp);
    imp^.fileName := currFile;
    imp^.lineNumber := currLine;
    imp^.fromIdent := fromIdent;
    imp^.idents := idents;
    if fromIdent # nil then
	imp^.searchList := AddToIdentList(nil,MakeIdent(fromIdent));
    else
	imp^.searchList := idents;
    end;
    imp^.currSearch := imp^.searchList^.first;
    imp^.saveModule := nil;
    imp^.saveScope := nil;
    imp^.next := nil;
    if currModule^.imports^.first = nil then
        currModule^.imports^.first := imp;
    else
        currModule^.imports^.last^.next := imp;
    end;
    currModule^.imports^.last := imp;
    return imp;
end ImportDecl;
$end (* modula2 *)

procedure EndImport(imp : ImportNode);
var
    modSym : Symbol;
begin
    if imp^.saveModule # nil then
	(* just returned from a declaration module *)
	(* restore to normal mode *)
	if currModule # globalModule then
	    ErrorName(imp^.currSearch^.name,'Missing end in imported module $');
	else
	    modSym := LookUpSymbol(imp^.currSearch^.name,currScope);
	    if modSym = nil then
		ErrorName(imp^.currSearch^.name,
		'Did not find module $ in import file');
	    end;
	end;
	currModule := imp^.saveModule;
	currScope := imp^.saveScope;
	imp^.saveModule := nil;
	imp^.saveScope := nil;
	imp^.currSearch := imp^.currSearch^.next;
    end;
end EndImport;

$if modula2 then
procedure ReadImport(imp : ImportNode) : ImportNode;
var
    modSym : Symbol;
    inMemory : boolean;
begin
    if DEBUG and TraceDecls then
	Writef(output,'ReadImport: continue=%n\n', imp^.saveModule#nil);
    end;
    EndImport(imp);
    inMemory := true;
    while inMemory and (imp^.currSearch # nil) do
	if DEBUG and TraceDecls then
	    Writef(output,'Import module ');
	    WriteString(output,imp^.currSearch^.name);
	    Writec(output, '\n');
	end;
	modSym := LookUpSymbol(imp^.currSearch^.name,currScope^.enclosing);
	if modSym # nil then
	    (* found, make sure it's a module *)
	    if modSym^.kind = SYMMODULE then
		if modSym^.symModule^.doingImport then
		    ErrorName(modSym^.name,'Recursive import of module $');
		end;
	    end;
	    imp^.currSearch := imp^.currSearch^.next;
	elsif currModule^.enclosing = globalModule then
	    (* not found, look for file *)
	    if DEBUG and TraceDecls then
		Writef(output,' Looking for file\n');
	    end;
	    assert(currProc = globalProc);

	    (* Look for external module *)
	    if not InitFile(imp^.currSearch^.name, MODDEFINITION,
		    'Cannot find file $ for imported module') then
		imp^.currSearch := imp^.currSearch^.next;
	    else
		(* save state of current module *)
		imp^.saveModule := currModule;
		imp^.saveScope := currScope;
		currModule := globalModule;
		currScope := globalModule^.scope;
		(* continue parsing with definition module *)
		inMemory := false;
	    end;
	else
	    (* non-global module, must be defined later *)
	    imp^.currSearch := imp^.currSearch^.next;
	end;
    end;
    if DEBUG and TraceDecls then
	Writef(output,'ReadImport: exit\n');
    end;
    return imp;
end ReadImport;
$end (* modula2 *)

procedure AddToImportedList (list : ImportedList; newOne : ImportedNode) : ImportedList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
	list^.last := nil;
    end;
    if newOne # nil then
        newOne^.next := nil;
        if list^.first = nil then
            list^.first := newOne;
        else
            list^.last^.next := newOne;
        end;
        list^.last:= newOne;
    end;
    return list;
end AddToImportedList;

procedure AddImport(sym : Symbol; fromModule, toModule : ModuleNode);
var
   imp : ImportedNode;
   tn : TypeNode;
   enum : EnumNode;
begin
    sym^.used := false;
    new(imp);
    imp^.sym := sym;
    imp^.fromModule := fromModule;
    toModule^.imported := AddToImportedList(toModule^.imported,imp);
end AddImport;

procedure GlobalImport(sym : Symbol; fromModule, toModule : ModuleNode;
	isQualified, complain : boolean);
var
    id : IdentNode;
    msym : Symbol;
    mn : ModuleNode;
begin
    GlobalPort(sym,fromModule,toModule,isQualified);
    if sym^.kind = SYMMODULE then
	mn := sym^.symModule;
	if mn^.qualExports # nil then
	    id := mn^.qualExports^.first;
	    while id # nil do
		msym := LookUpSymbol(id^.name,mn^.exportScope);
		if msym = nil then
		    if complain then
			ErrorName(id^.name,'$ not found on import');
			if DefineSymbol(msym, id^.name, mn^.exportScope, 
				ONECASE, SYMANY) then
			end;
			GlobalPort(msym, mn, toModule, true);
		    end;
		else
		    GlobalPort(msym,mn,toModule,true);
		end;
		id := id^.next;
	    end;
	end;
	if mn^.unqualExports # nil then
	    id := mn^.unqualExports^.first;
	    while id # nil do
		msym := LookUpSymbol(id^.name,mn^.exportScope);
		if msym = nil then
		    if complain then
			ErrorName(id^.name,'$ not found on import');
			if DefineSymbol(msym, id^.name, mn^.exportScope, 
				ONECASE, SYMANY) then
			end;
			GlobalPort(msym, mn, toModule, false);
		    end;
		else
		    GlobalPort(msym,mn,toModule,false);
		end;
		id := id^.next;
	    end;
	end;
    end;
end GlobalImport;

$if modula2 then
procedure ProcessImport(imp : ImportNode; complain : boolean);
var
    id, idnext : IdentNode;
    sym, nsym, msym : Symbol;
    scope : Scope;
    remainder : IdentList;
    fromModule, mn : ModuleNode;
    isQualified, firstTime : boolean;
begin
    if DEBUG and TraceDecls then
	Writef(output,'ProcessImport: start\n');
    end;
    firstTime := not complain;
    EndImport(imp);
    if complain then
	currFile := imp^.fileName;
	currLine := imp^.lineNumber;
    end;
    scope := nil;
    if imp^.fromIdent # nil then
	(* import from *)
	sym := LookUpSymbol(imp^.fromIdent,currScope^.enclosing);
	if sym = nil then
	    if complain then
		ErrorName(imp^.fromIdent,'Module $ not found for import');
	    end;
	elsif sym^.kind # SYMMODULE then
	    if complain then
		ErrorName(imp^.fromIdent,'Import "from" $ is not a module');
	    end;
	else
	    scope := sym^.symModule^.exportScope;
	    fromModule := sym^.symModule;
	    isQualified := true;
	    (* if module exists, complain on first pass, not on second *)
	    complain := not complain;
(* ||| do more detailed complaining: types, consts OK, procs, vars not
	    if complain and (currModule^.enclosing = globalModule) and
		    currModule^.noinit and not fromModule^.noinit then
		ErrorName(fromModule^.name,
		    'Cannot import module $ into a NOINIT module');
	    end;
*)
	end;   
    else
	scope := currScope^.enclosing;
	fromModule := currModule^.enclosing;
	isQualified := false;
    end;
    if scope = nil then
	(* do nothing *)
    elsif (imp^.idents = nil) and firstTime then
	(* import * *)
	if fromModule^.qualExports # nil then
	    id := fromModule^.qualExports^.first;
	    while id # nil do
		sym := LookUpSymbol(id^.name,scope);
		if sym = nil then
		    ErrorName(id^.name,'$ not found on import');
		    if DefineSymbol(sym, id^.name, currScope, 
			    ONECASE, SYMANY) then
		    end;
		else
		    nsym := Port(sym,currScope);    
		    if adviseFlag then
			AddImport(nsym,fromModule,currModule);
		    end;
		    if currModule^.enclosing = globalModule then
		       GlobalImport(sym,fromModule,currModule,isQualified,true);
		    end;
		end;
		id := id^.next;
	    end;
	end;
	if fromModule^.unqualExports # nil then
	    id := fromModule^.unqualExports^.first;
	    while id # nil do
		sym := LookUpSymbol(id^.name,scope);
		if sym = nil then
		    ErrorName(id^.name,'$ not found on import');
		    if DefineSymbol(sym, id^.name, currScope, 
			    ONECASE, SYMANY) then
		    end;
		else
		    nsym := Port(sym,currScope);    
		    if adviseFlag then
			AddImport(nsym,fromModule,currModule);
		    end;
		    if currModule^.enclosing = globalModule then
			GlobalImport(sym,fromModule,currModule,isQualified,true);
		    end;
		end;
		id := id^.next;
	    end;
	end;
    elsif imp^.idents # nil then
	remainder := nil;
	id := imp^.idents^.first;
	while id # nil do
	    idnext := id^.next;
	    sym := LookUpSymbol(id^.name,scope);
	    if sym = nil then
		if DEBUG and TraceDecls then
		    Writef(output,'Import ');
		    WriteString(output,sym^.name);
		    Writef(output,' not found\n');
		end;
		if complain then
		    ErrorName(id^.name,'$ not found on import');
		    if DefineSymbol(sym, id^.name, currScope, 
			    ONECASE, SYMANY) then
		    end;
		end;
		remainder := AddToIdentList(remainder,id);
	    else
		if (currModule^.enclosing = globalModule) and
			(sym^.kind = SYMMODULE) and
			currModule^.noinit and not sym^.symModule^.noinit then
		    ErrorName(sym^.symModule^.name,
			'Cannot import module $ into a NOINIT module');
		end;
		nsym := Port(sym,currScope);    
		if DEBUG and TraceDecls then
		    Writef(output,'Import ');
		    WriteString(output,sym^.name);
		    Writec(output, '\n');
		end;
		if adviseFlag then
		    AddImport(nsym,fromModule,currModule);
		end;
		if currModule^.enclosing = globalModule then
		    GlobalImport(sym,fromModule,currModule,isQualified,complain);
		end;
	    end;
	    id := idnext;
	end;
	imp^.idents := remainder;
    end;
end ProcessImport;
$end (* modula2 *)

procedure OutputModuleImports(mn : ModuleNode);
var
    frommn    : ModuleNode;
    submn     : ModuleNode;
    imp       : ImportedNode;
    i         : integer;
    enum      : EnumNode;
    sym	      : Symbol;
    used      : boolean;
    printName : boolean;
    tn        : TypeNode;
begin
    (* Do all nested modules first *)
    submn := mn^.modules^.first;
    while submn # nil do
	OutputModuleImports(submn);
	submn := submn^.next;
    end;
    if mn^.imported # nil then
	i := 0;
	frommn := nil;
	printName := true;
	imp := mn^.imported^.first;
	while imp # nil do
	    used := imp^.sym^.used;
	    if  not used and (imp^.sym^.kind = SYMTYPE) then
		tn := BaseType(imp^.sym^.symType);
		if tn^.kind = DTENUMERATION then
		    enum := tn^.enumList^.first;
		    (* Look for a used enumeration constant *)
		    while enum # nil do
			sym := LookUpSymbol(enum^.enumSym^.name, mn^.scope);
			if (sym # nil) and sym^.used then
			    used := true;
			    exit while;
			end;
			enum := enum^.next;
		    end;
		end;
	    end; (* special processing for enumeration types *)
	    if not used then
		(* print it out *)
		if printName then
		    if (mn^.kind = MODIMPLEMENTATION) or mn^.containsInline then
			Writes(output, 'Implementation ');
		    elsif mn^.kind = MODDEFINITION then
			Writes(output, 'Definition ');
		    end;
		    Writes(output, 'Module ');
		    WriteString(output, mn^.name);
		    Writes(output," doesn't use these imported identifiers:\n");
		    printName := false;
		end;
		if imp^.fromModule # frommn then
		    (* new module *)
		    if i > 0 then
			Writes(output,';\n');
		    end;
		    i := 0;
		    frommn := imp^.fromModule;
		    if frommn^.name = nil then
			Writes(output, '    import ');
		    else
			Writes(output, '    from ');
			WriteString(output,frommn^.name);
			Writes(output, ' import\n        ');
		    end;
		end;
		if i > 3 then
		    Writes(output,',\n        ');
		    i := 0;
		elsif i > 0 then
		    Writes(output,', ');
		end;
		WriteString(output,imp^.sym^.name);
		i := i + 1;
	    end;
	    imp := imp^.next;
	end;
	if i > 0 then
	    Writes(output,';\n');
	end;
    end;
end OutputModuleImports;

procedure OutputImports;
var
    mn : ModuleNode;
begin
    mn := globalModule^.modules^.first;
    while mn # nil do
	OutputModuleImports(mn);
	mn := mn^.next;
    end;
end OutputImports;

procedure AppendIdentList(some, more : IdentList) : IdentList;
begin
    if some = nil then
	some := more;
    elsif more = nil then
	(* nothing to do *)
    elsif more^.first = nil then
	(* nothing to add *)
    elsif some^.first = nil then
	(* nothing to add to *)
	some := more;
    else
	some^.last^.next := more^.first;
	some^.last := more^.last;
    end;
    return some;
end AppendIdentList;

$if modula2 then
procedure ExportDecl(idents : IdentList; qualToken : Token);
begin
    if standardExportFlag then
	(* ignore export statements *)
    elsif (qualToken = TKQUALIFIED) or 
	    (currModule^.enclosing = globalModule)  then
	currModule^.qualExports := AppendIdentList(currModule^.qualExports,
					idents);
    else
	currModule^.unqualExports := AppendIdentList(currModule^.unqualExports,
					idents);
    end;
end ExportDecl;
$end

procedure AddToIdentList (list : IdentList; newOne : IdentNode) : IdentList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
	list^.last := nil;
    end;
   if newOne # nil then
        newOne^.next := nil;
        if list^.first = nil then
            list^.first := newOne;
        else
            list^.last^.next := newOne;
        end;
        list^.last:= newOne;
    end;
    return list;
end AddToIdentList;

procedure SameTypeParam(dst, src : TypeNode) : boolean;
var
    same : boolean;
begin
    same := false;
    src := StoredType(src);
    dst := StoredType(dst);
    if (dst = src) or (dst = anyTypeNode) or (src = anyTypeNode) then
	same := true;
    elsif (dst = nil) or (src = nil) then
	(* do nothing *)
    elsif (dst^.kind = DTARRAY) and (src^.kind = DTARRAY) then
	if (dst^.arrayKind = src^.arrayKind) and 
		(dst^.arrayKind # ARRAYNORMAL) then
	    same := SameTypeParam(dst^.elementType, src^.elementType);
	end;
    end; 
    return same;
end SameTypeParam;

procedure CheckEqualProc(sym1, sym2 : Symbol);
var
    oldProc, newProc : ProcNode;
begin
    oldProc := sym1^.symProc;
    newProc := sym2^.symProc;
    CheckEqualProcType(oldProc,newProc^.procType);
    (* new proc inherits global name and external attribute from old *)
    newProc^.globalName := oldProc^.globalName;
    if newProc^.extern # oldProc^.extern then
	ErrorName(newProc^.name,
	'Global/external for $ inconsistent between definition and implementation');
    end;
    newProc^.extern := oldProc^.extern;
    newProc^.internalProc := newProc^.internalProc and oldProc^.internalProc;
    (* make old sym point to new proc *)
    sym1^.symProc := newProc;
end CheckEqualProc;

procedure CheckEqualProcType(proc : ProcNode; procType : TypeNode);
var
    pn1, pn2 : ParamNode;
    error : boolean;
begin
    error := false;
    if proc^.procType^.paramList = nil then
	pn1 := nil;
    else
	pn1 := proc^.procType^.paramList^.first;
    end;
    if procType^.paramList = nil then
	pn2 := nil;
    else
	pn2 := procType^.paramList^.first;
    end;
    while not error and (pn1 # nil) and (pn2 # nil) do
	if (pn1^.name # pn2^.name) then
	    error := true;
	end;
	if (pn1^.kind # pn2^.kind) or
		not SameTypeParam(pn1^.paramType,pn2^.paramType)
	then
	    error := true;
	end;
	pn1 := pn1^.next;
	pn2 := pn2^.next;
    end;
    error := error or (pn1 # nil) or (pn2 # nil) or
       (ActualType(proc^.procType^.funcType) # ActualType(procType^.funcType));
    if error then
	ErrorName(proc^.name,
	    'Redefinition of procedure $ not identical to original');
    end;
end CheckEqualProcType;

end Decls.
