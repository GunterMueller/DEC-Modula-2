implementation module porttab;

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

from MemLib import ALLOCATE;
from strings import Append;
from Strings import NewString, NewText, AddChar, CopyString, WriteString;
from io import writef, writec, output, writes;
from FileInfo import LastModifiedTime;
from symtab import PortNode, ModuleNode, ParamNode, FieldNode,
	Symbol, SymbolKind, DefineSymbol, NewScope, EnumNode, ParamKind,
	DataType, Scope, globalScope, ArrayKind, fileList;
from list import AddToList;
const
    MODULESCOPE = 1;

var
    dumpSymLetter : array SymbolKind of char;
    printSymKind : array SymbolKind of array [1..10] of char;
    ioModule, systemModule, storageModule, bitoperationsModule, memoryModule,
	globalModule : ModuleNode;

procedure ModuleError(const msg  : array of char;
		      const name : String;
		      const expMn, impMn : ModuleNode);
var
    fatal, impOOD, expOOD, impOld : boolean;
begin
    impOOD := false;
    expOOD := false;
    impOld := false;
    fatal := false;
    if impMn^.watchErrors then
	expOOD := true;
	if expMn^.watchErrors then  (* imp watch, exp watch *)
	    fatal := true;
	    impOOD := true;
	end;
    else
	impOOD := true;
	impOld := (expMn^.watchErrors) or 
		(impMn^.objectTime <= expMn^.defTime);
    end;
    if not expMn^.ignoreErrors or not impMn^.ignoreErrors then
	if fatal or logErrorsFlag then
	    if fatal then
		writef(output,"Could not correct inconsistency between ");
		WriteString(output, impMn^.name);
		writef(output, " and ");
		WriteString(output, expMn^.name);
		Writec(output, '\n');
	    end;
	    WriteString(output, expMn^.name);
	    Writec(output, '.');
	    WriteString(output, name);
	    Writef(output, ': %s ', msg);
	    WriteString(output, impMn^.name);
	    Writec(output, '\n');
	end;
	if not libraryCheckFlag or (impMn^.named and expMn^.named) then
	    impMn^.outOfDate := impMn^.outOfDate or impOOD;
	    impMn^.oldObject := impMn^.oldObject or impOld;
	    expMn^.outOfDate := expMn^.outOfDate or expOOD;
	    expMn^.oldObject := expMn^.oldObject or expOOD;
	    inc(errorCount);
	end;
	fatalErrorFlag := fatalErrorFlag or fatal;
    end;
end ModuleError;

procedure ModuleWarning(const msg  : array of char;
		        const name : String;
		        const expMn, impMn : ModuleNode);
begin
    if not expMn^.ignoreErrors or not impMn^.ignoreErrors then
	if (not autoMakeFlag) or logErrorsFlag then
	    Writes(output, '(Warning) ');
	    WriteString(output, expMn^.name);
	    Writec(output, '.');
	    WriteString(output, name);
	    Writef(output, ': %s ', msg);
	    WriteString(output, impMn^.name);
	    Writec(output, '\n');
	end;
    end;
end ModuleWarning;

procedure DefineModule(name : String; fileName : cardinal) : ModuleNode;
var
    mn : ModuleNode;
    sym : Symbol;

    procedure SetTimes(mn : ModuleNode; fileName : cardinal);
    var
	s : array [0..1023] of char;
    begin
	if fileName = 0 then
	    mn^.objectTime := 0;
	    mn^.defTime := last(integer);
	else
	    CopyString(fileList[fileName], s);
	    mn^.objectTime := LastModifiedTime(s, 0);
	    CopyString(mn^.name, s);
	    Append(s, '.def');
	    (* ||| This should cycle through MODPATH for better results *)
	    mn^.defTime := LastModifiedTime(s, last(integer));
	end;
    end SetTimes;

begin
    if DefineSymbol(sym,name,MODULESCOPE) then
	new(mn);
	mn^.name := name;
	mn^.scope := NewScope();
	mn^.exports := nil;
	mn^.defined := FALSE;
	mn^.outOfDate := FALSE;
	mn^.oldObject := FALSE;
	mn^.watchErrors := FALSE;
	mn^.examineErrors := FALSE;
	mn^.ignoreErrors := FALSE;
	mn^.builtin := FALSE;
	mn^.named := FALSE;
	mn^.fileName := fileName;
	moduleList := AddToList(moduleList,mn);
	sym^.kind := SYMMODULE;
	sym^.symModule := mn;
	SetTimes(mn, fileName);
    else
	mn := sym^.symModule;
	if (fileName <> 0) and (mn^.fileName = 0) then
	    mn^.fileName := fileName;
	    SetTimes(mn, fileName);
	end;
    end;
    return mn;
end DefineModule;

procedure DefinePort(mname, name : String; pn : PortNode;
	imported, unqual : boolean; extern : GlobalSymKind;
	refModule : ModuleNode);
var
    mn : ModuleNode;
    sym : Symbol;
    scope : Scope;
    qpn : PortNode;
begin
    pn^.name := name;
    pn^.refModule := refModule;
    pn^.extern := extern;
    mn := DefineModule(mname,0);
    if unqual then
	scope := globalScope;
    else
	scope := mn^.scope;
    end;
    if DefineSymbol(sym,pn^.name,scope) then
	sym^.kind := pn^.kind;
	sym^.homeModule := mn;
	sym^.imported := nil;
	sym^.exported := nil;
	mn^.exports := AddToList(mn^.exports,sym);
    end;
    if tracePorttab then
	if imported then
	    writef(output,"Define import ");
	else
	    writef(output,"Define export ");
	end;
	WriteString(output,sym^.name);
	writec(output, ' ');
	DumpPort(pn);
    end;
    if imported then
	sym^.imported := AddToList(sym^.imported,pn);
    elsif sym^.exported # nil then
	if sym^.exported^.refModule # pn^.refModule then
	    if unqual then
		ModuleError("identifier also exported unqualified from ",
		    sym^.name,sym^.exported^.refModule,pn^.refModule);
	    else
		ModuleError("identifier also exported from ",sym^.name,
		    sym^.exported^.refModule,pn^.refModule);
	    end;
	end;
    else
	sym^.exported := pn;
    end;
    if not imported and unqual then
	(* make unqualified export also be qualified export *)
	new(qpn);
	qpn^ := pn^;
	DefinePort(mname,name,qpn,imported,false,extern,refModule);
    end;
end DefinePort;

procedure WriteType(f : File; tn : TypeNode);
var
    en : EnumNode;
    pn : ParamNode;
    fn : FieldNode;
begin
    if tracePorttab then
	writef(f,"(%x)",integer(tn));
    end;
    if tn = nil then
	writef(f,"nil");
    else
	case tn^.kind of
	| DTINTEGER :
	    writef(f,"integer");
	| DTCARDINAL :
	    writef(f,"cardinal");
	| DTCARDINT :
	    writef(f,"cardint");
	| DTLONGREAL :
	    writef(f,"longreal");
	| DTREAL :
	    writef(f,"real");
	| DTCHAR :
	    writef(f,"char");
	| DTBOOLEAN :
	    writef(f,"boolean");
	| DTSET :
	    writef(f,"set{");
	    WriteType(f,tn^.setRange);
	    writef(f,"}");
	| DTENUMERATION :
	    writef(f,"(");
	    if tn^.enumList # nil then
		en := tn^.enumList^.first;
		while en # nil do
		    WriteString(f,en^.name);
		    en := en^.next;
		    if en # nil then
			writef(f,",");
		    end;
		end;
	    end;
	    writef(f,")");
	| DTPOINTER :
	    writef(f,"pointer to ");
	    (* That's all, to keep from recursing forever *)
	| DTRECORD :
	    writef(f,"record ");
	    if tn^.fieldList # nil then
		fn := tn^.fieldList^.first;
		while fn # nil do
		    WriteString(f,fn^.name);
		    writef(f,":");
		    WriteType(f,fn^.fieldType);
		    fn := fn^.next;
		    writef(f,";");
		end;
	    end;
	    writef(f," end");
	| DTARRAY :
	    writef(f,"array ");
	    WriteType(f,tn^.indexType);
	    writef(f," of ");
	    WriteType(f,tn^.elementType);
	| DTOPENARRAY :
	    case tn^.arrayKind of
	    | ARRAYOPEN : writef(f,"array ");
	    | ARRAYDYNAMIC : writef(f,"dynarray ");
	    | ARRAYSUBARRAY : writef(f,"subarray ");
	    end;
	    writef(f," [%d] of ",tn^.numDimensions);
	    WriteType(f,tn^.openElementType);
	| DTWORD :
	    writef(f,"word");
	| DTBYTE :
	    writef(f,"byte");
	| DTSUBRANGE :
	    WriteType(f,tn^.baseType);
	    writef(f,"[%d..%d]",tn^.subMinOrd,tn^.subMaxOrd);
	| DTPROC :
	    writef(f,"procedure(");
	    if tn^.paramList # nil then
		pn := tn^.paramList^.first;
		while pn # nil do
		    if (pn^.kind = PARAMVAR) or (pn^.kind = PARAMARRAYVAR) then
			writef(f,"var ");
		    elsif (pn^.kind = PARAMCONST) or
			    (pn^.kind = PARAMARRAYCONST)
		    then
			writef(f,"const ");
		    end;
		    WriteString(f,pn^.name);
		    writef(f,":");
		    WriteType(f,pn^.paramType);
		    pn := pn^.next;
		    if pn # nil then
			writef(f,";");
		    end;
		end;
	    end;
	    writef(f,")");
	    if tn^.retType # nil then
		writef(f,":");
		WriteType(f,tn^.retType);
	    end;
	    if tn^.inlineTimeStamp # 0 then
		writef(f," inline %d",tn^.inlineTimeStamp);
	    end;
	| DTOPAQUE :
	    writef(f,"opaque");
	else
	    writef(f,"unknown %d",ord(tn^.kind));
	end;
    end;
end WriteType;

procedure WriteConst(f : File; cn : ConstNode);
begin
    case cn^.kind of
    | DTSTRING:
	writef(f,"'");
	WriteString(f,cn^.strVal);
	writef(f,"'");
    | DTINTEGER, DTCARDINAL :
	writef(f,"%d",cn^.cardVal);
    | DTREAL, DTLONGREAL :
	WriteString(f,cn^.realVal);
    | DTCHAR :
	writef(f,"'");
	writef(f,"%c",chr(cn^.charVal));
	writef(f,"'");
    | DTBOOLEAN :
	writef(f,"%d",cn^.boolVal);
    | DTSET :
	writef(f,"{");
	WriteString(f,cn^.setVal^.value);
	writef(f,"}");
    | DTENUMERATION :
	WriteString(f,cn^.enumVal^.name);
    end;
end WriteConst;

procedure DumpPort(pn : PortNode);
begin
    writef(output,'Port module ');
    WriteString(output,pn^.refModule^.name);
    writef(output,' %c %x ',dumpSymLetter[pn^.kind],integer(pn));
    case pn^.kind of
    | SYMCONST :
	WriteConst(output,pn^.symConst);
    | SYMTYPE :
	WriteType(output,pn^.symType);
    | SYMVAR :
	WriteType(output,pn^.symVar);
    | SYMPROC :
	WriteType(output,pn^.symProc);
    end;
    writef(output,'\n');
end DumpPort;

procedure DumpExports();
var
    mn : ModuleNode;
    pn : PortNode;
    sym : Symbol;
begin
    writef(output,"DumpExports:\n");
    mn := moduleList^.first;
    while mn # nil do
	writef(output,"Module ");
	WriteString(output,mn^.name);
	writef(output," scope=%d\n",mn^.scope);
	if mn^.exports # nil then
	    sym := mn^.exports^.first;
	    while sym # nil do
		writef(output,'Symbol ');
		WriteString(output,sym^.name);
		writef(output,' %c\n',dumpSymLetter[sym^.kind]);
		if sym^.exported # nil then
		    writef(output,"Exported ");
		    DumpPort(sym^.exported);
		end;
		if sym^.imported # nil then
		    pn := sym^.imported^.first;
		    while pn # nil do
			writef(output,"Imported ");
			DumpPort(pn);
			pn := pn^.next;
		    end;
		end;
		sym := sym^.next;
	    end;
	end;
	mn := mn^.next;
    end;
end DumpExports;

procedure SameConst(ecn : ConstNode; var icn : ConstNode) : boolean;
var
    result : boolean;
begin
    if ecn = icn then
	return TRUE;
    elsif ecn^.kind # icn^.kind then
	return FALSE;
    else
	result := TRUE;
	case ecn^.kind of
	| DTSTRING :
	    result := ecn^.strVal = icn^.strVal;
	| DTCHAR :
	    result := ecn^.charVal = icn^.charVal;
	| DTINTEGER, DTCARDINAL :
	    result := ecn^.cardVal = icn^.cardVal;
	| DTBOOLEAN :
	    result := ecn^.boolVal = icn^.boolVal;
	| DTREAL, DTLONGREAL :
	    result := ecn^.realVal = icn^.realVal;
	| DTSET :
	    result := SameType(ecn^.setVal^.setType,icn^.setVal^.setType)
			and (ecn^.setVal^.value = icn^.setVal^.value)
			and (ecn^.setVal^.size = icn^.setVal^.size);
	| DTENUMERATION :
	    result := SameType(ecn^.enumVal^.enumType,icn^.enumVal^.enumType)
			and (ecn^.enumVal^.enumOrd = icn^.enumVal^.enumOrd);
	else
	    writef(output,"SameConst: unexpected kind?");
	    result := false;
	end;
    end;
    if result then
	(*icn := ecn;*)
    end;
    return result;
end SameConst;

const
    TYPESTACKSIZE = 100;
var
    etnStack, itnStack : array [1..TYPESTACKSIZE] of TypeNode;
    typeStackPtr : cardinal;

procedure SameType(etn : TypeNode; var itn : TypeNode) : boolean;
var
    result : boolean;
    epn, ipn : ParamNode;
    efn, ifn : FieldNode;
    een, ien : EnumNode;
    i : cardinal;
begin
    inc(numCompares);
    if etn = itn then
	inc(numEq);
	return TRUE;
    elsif (etn = nil) or (itn = nil) then
	return FALSE;
    elsif (etn^.canonicalType # nil) and
	    (etn^.canonicalType = itn^.canonicalType)
    then
	inc(numQuick);
	return TRUE;
    elsif etn^.kind # itn^.kind then
	return (etn^.kind = DTOPAQUE) or (itn^.kind = DTOPAQUE);
    elsif (etn^.size # itn^.size) or (etn^.alignment # itn^.alignment) then
	return FALSE;
    else
	result := TRUE;
	case etn^.kind of
	| DTNULL, DTINTEGER, DTBOOLEAN, DTCHAR, DTREAL, DTBYTE, DTCARDINAL,
		DTCARDINT, DTWORD, DTPROCESS, DTLONGREAL:
	    (* matching primitive types *)
	| DTPOINTER:
	    inc(numTypeStack);
	    for i := typeStackPtr to 1 by -1 do
		inc(numTypeStackLoop);
		if etnStack[i] = etn then
		    return itn = itnStack[i];
		end;
	    end;
	    inc(typeStackPtr);
	    etnStack[typeStackPtr] := etn;
	    itnStack[typeStackPtr] := itn;
	    result := (etn^.ptrKind = itn^.ptrKind) and
			SameType(etn^.toType,itn^.toType);
	    dec(typeStackPtr);
	| DTRECORD:
	    if etn^.fieldList # nil then
		efn := etn^.fieldList^.first;
	    else
		efn := nil;
	    end;
	    if itn^.fieldList # nil then
		ifn := itn^.fieldList^.first;
	    else
		ifn := nil;
	    end;
	    while result and (efn # nil) and (ifn # nil) do
		result := (efn^.name = ifn^.name)
			    and (efn^.size = ifn^.size)
			    and (efn^.offset = ifn^.offset)
			    and SameType(efn^.fieldType,ifn^.fieldType);
		efn := efn^.next;
		ifn := ifn^.next;
	    end;
	    result := result and (efn = nil) and (ifn = nil);
	| DTPROC:
	    result := SameType(etn^.retType,itn^.retType) and
			(etn^.inlineTimeStamp = itn^.inlineTimeStamp);
	    if etn^.paramList # nil then
		epn := etn^.paramList^.first;
	    else
		epn := nil;
	    end;
	    if itn^.paramList # nil then
		ipn := itn^.paramList^.first;
	    else
		ipn := nil;
	    end;
	    while result and (epn # nil) and (ipn # nil) do
		result := (epn^.name = ipn^.name)
			    and (epn^.kind = ipn^.kind)
			    and SameType(epn^.paramType,ipn^.paramType);
		epn := epn^.next;
		ipn := ipn^.next;
	    end;
	    result := result and (epn = nil) and (ipn = nil);
	| DTARRAY:
	    result := SameType(etn^.indexType,itn^.indexType)
			and SameType(etn^.elementType,itn^.elementType);
	| DTOPENARRAY:
	    result := (etn^.arrayKind = itn^.arrayKind) and
		    (etn^.dynArrayKind = itn^.dynArrayKind) and
		    (etn^.numDimensions = itn^.numDimensions) and
		    SameType(etn^.openElementType,itn^.openElementType);
	| DTSET:
	    result := SameType(etn^.setRange,itn^.setRange);
	| DTOPAQUE:
	    result := true;
	| DTSUBRANGE:
	    result := (etn^.subMinOrd = itn^.subMinOrd) and
			(etn^.subMaxOrd = itn^.subMaxOrd) and
			SameType(etn^.baseType,itn^.baseType);
	| DTENUMERATION:
	    result := etn^.enumCount = itn^.enumCount;
	    if etn^.enumList # nil then
		een := etn^.enumList^.first;
	    else
		een := nil;
	    end;
	    if itn^.enumList # nil then
		ien := itn^.enumList^.first;
	    else
		ien := nil;
	    end;
	    while result and (een # nil) and (ien # nil) do
		result := (een^.name = ien^.name) and
			    (een^.enumOrd = ien^.enumOrd);
		een := een^.next;
		ien := ien^.next;
	    end;
	    result := result and (een = nil) and (ien = nil);
	else
	    writef(output,"SameType: unexpected type?");
	    result := false;
	end;
    end;
    if result then
	if (etn^.canonicalType # nil) then
	    itn^.canonicalType := etn^.canonicalType;
	    inc(numetoi);
	elsif (itn^.canonicalType # nil) then
	    etn^.canonicalType := itn^.canonicalType;
	    inc(numitoe);
	else
	    etn^.canonicalType := itn;
	    itn^.canonicalType := etn^.canonicalType;
	    inc(numetoe);
	end;
    end;
    return result;
end SameType;

procedure CheckSame(exp, imp : PortNode);
begin
    if exp^.kind # imp^.kind then
	ModuleError("identifier is used as a different kind of object in",
	    exp^.name,exp^.refModule,imp^.refModule);
    elsif exp^.extern <> imp^.extern then
	ModuleError("global/external attribute differs with that in",
	    exp^.name,exp^.refModule,imp^.refModule);
    else
	inc(numCheck);
	case exp^.kind of
	| SYMCONST:
	    if not SameConst(exp^.symConst,imp^.symConst) then
		ModuleError("constant differs with that in",
		    exp^.name,exp^.refModule,imp^.refModule);
	    end;
	    inc(numCheckConst);
	| SYMPROC:
	    if not SameType(exp^.symProc,imp^.symProc) then
		ModuleError("procedure differs with that in",
		    exp^.name,exp^.refModule,imp^.refModule);
	    end;
	    inc(numCheckProc);
	| SYMTYPE:
	    if (exp^.symType # nil) and (exp^.symType^.canonicalType = nil) then
		exp^.symType^.canonicalType := exp^.symType;
		inc(numSetCanon);
	    end;
	    if not SameType(exp^.symType,imp^.symType) then
		ModuleError("type differs with that in",
		    exp^.name,exp^.refModule,imp^.refModule);
	    end;
	    inc(numCheckType);
	| SYMVAR:
	    if not SameType(exp^.symVar,imp^.symVar) then
		ModuleError("variable type differs with that in",
		    exp^.name,exp^.refModule,imp^.refModule);
	    end;
	    inc(numCheckVar);
	end;
    end;
end CheckSame;

procedure CheckExports();
var
    mn : ModuleNode;
    pn : PortNode;
    sym : Symbol;
begin
    if logErrorsFlag then
	writef(output,"CheckExports:\n");
    end;
    if moduleList = nil then
	return;
    end;
    mn := moduleList^.first;
    while mn # nil do
	if tracePorttab then
	    writef(output,"Module ");
	    WriteString(output,mn^.name);
	    writef(output," scope=%d\n",mn^.scope);
	end;
	if mn^.ignoreErrors then
	    (* don't worry about this module *)
	elsif mn^.exports # nil then
	    sym := mn^.exports^.first;
	    while sym # nil do
		if tracePorttab then
		    writef(output,'Symbol ');
		    WriteString(output,sym^.name);
		    writef(output,' %c\n',dumpSymLetter[sym^.kind]);
		end;
		if sym^.exported = nil then
		    if sym^.imported # nil then
			pn := sym^.imported^.first;
			if sym^.homeModule^.defined then
			    while pn # nil do
				ModuleError("not exported but imported by",
				    sym^.name,sym^.homeModule,pn^.refModule);
				pn := pn^.next;
			    end;
			else (* don't have any real data on exporter *)
			    while pn # nil do
				ModuleWarning("no .o file, but imported by",
				    sym^.name,sym^.homeModule,pn^.refModule);
				pn := pn^.next;
			    end;
			end;
		    end;
		else
		    if sym^.imported # nil then
			pn := sym^.imported^.first;
			while pn # nil do
			    if tracePorttab then
				writef(output,"Imported ");
				DumpPort(pn);
			    end;
			    CheckSame(sym^.exported,pn);
			    pn := pn^.next;
			end;
		    end;
		end;
		sym := sym^.next;
	    end;
	end;
	mn := mn^.next;
    end;
end CheckExports;

procedure WatchModule(const arg : array of char);
var
    i : cardinal;
    mn : ModuleNode;
begin
    i := 2;	(* skip -M *)
    while (i < number(arg)) and (arg[i] # 0C) do
	while (i < number(arg)) and (arg[i] # 0C) and (arg[i] # ',') do
	    AddChar(arg[i]);
	    inc(i);
	end;
	mn := DefineModule(NewString(),0);
	mn^.watchErrors := TRUE;
	inc(i);
    end;
end WatchModule;

procedure ExamineModule(const arg : array of char);
var
    i : cardinal;
    mn : ModuleNode;
begin
    i := 2;	(* skip -X *)
    while (i < number(arg)) and (arg[i] # 0C) do
	while (i < number(arg)) and (arg[i] # 0C) and (arg[i] # ',') do
	    AddChar(arg[i]);
	    inc(i);
	end;
	mn := DefineModule(NewString(),0);
	mn^.examineErrors := TRUE;
	inc(i);
    end;
end ExamineModule;

procedure IgnoreModule(const arg : array of char);
var
    i : cardinal;
    mn : ModuleNode;
begin
    i := 2;	(* skip -N *)
    while (i < number(arg)) and (arg[i] # 0C) do
	while (i < number(arg)) and (arg[i] # 0C) and (arg[i] # ',') do
	    AddChar(arg[i]);
	    inc(i);
	end;
	mn := DefineModule(NewString(),0);
	mn^.ignoreErrors := TRUE;
	inc(i);
    end;
end IgnoreModule;

begin
    moduleList := nil;
    typeStackPtr := 0;
    errorCount := 0;
    standardCardinalFlag := FALSE;
    standardKeywordFlag := FALSE;
    standardStringFlag := FALSE;
    dumpSymLetter[SYMNULL] := '?';
    dumpSymLetter[SYMMODULE] := 'M';
    dumpSymLetter[SYMVAR] := 'V';
    dumpSymLetter[SYMPROC] := 'P';
    dumpSymLetter[SYMCONST] := 'C';
    dumpSymLetter[SYMTYPE] := 'T';
    printSymKind[SYMNULL] := 'unknown';
    printSymKind[SYMMODULE] := 'module';
    printSymKind[SYMVAR] := 'variable';
    printSymKind[SYMPROC] := 'procedure';
    printSymKind[SYMCONST] := 'constant';
    printSymKind[SYMTYPE] := 'type';
    globalModule := DefineModule(NewText("$GLOBAL$"),0);
    globalModule^.scope := globalScope;
    ioModule := DefineModule(NewText("IO"),0);
    ioModule^.ignoreErrors := TRUE;
    ioModule^.builtin := TRUE;
    systemModule := DefineModule(NewText("SYSTEM"),0);
    systemModule^.ignoreErrors := TRUE;
    systemModule^.builtin := TRUE;
    memoryModule := DefineModule(NewText("MEMORY"),0);
    memoryModule^.ignoreErrors := TRUE;
    memoryModule^.builtin := TRUE;
    storageModule := DefineModule(NewText("Storage"),0);
    storageModule^.ignoreErrors := TRUE;
    storageModule^.builtin := TRUE;
    bitoperationsModule := DefineModule(NewText("BITOPERATIONS"),0);
    bitoperationsModule^.ignoreErrors := TRUE;
    bitoperationsModule^.builtin := TRUE;
end porttab.
