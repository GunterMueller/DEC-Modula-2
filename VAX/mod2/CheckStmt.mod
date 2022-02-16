implementation module CheckStmt;

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

from Machine import
    HugeInteger, MAXINT;

from Tokens import
    TKATNOCHECK, TKNOT;

from Globals import
    TraceNstmt, DEBUG, target, TARGETVAX, standardCardinalFlag;
    
from Symbols import
    EvalMode, CHECKRANGE, CHECKPTRASSIGN, GSNORMAL, Symbol, TypeNode,
    VarNode, ProcNode, ExprNode, StmtNode, StmtList, CaseTreeNode, ExprSetNode,
    MemoryType, DataType, DataTypeSet, stringDataType, VarKind, indexableTypes,
    StmtKind, booleanTypeNode, cardIntTypeNode, anyTypeNode, 
    currScope, currProc, exitStatus, currLine, currFile, OpenScope,
    LookUpSymbol, CaseNode, AllocationArray, BIPASSERT,
    EXPRSYM, EXPRCONST, EXPRFUNC,
    withQualList, WithQualNode, SYMVAR, EndScope;

from Errors import
    ExprError, ExprErrorName, StmtError, StmtWarning, StmtErrorName,
    ProcErrorName;

from Consts import
    OrdOf;

from TypeDefs import
    PointerType;

from TypeInfo import
    BaseType, ActualType, StoredType, LowerBoundOf, UpperBoundOf;

from Decls import
    DefineVar;

from BuildExpr import
    BuildExprUnOp, SameExprLine, AddToExprList;

from BuildStmt import
    SameStmtLine;

from Compatible import
    Assignable;

from CheckExpr import
    IsBadExpr, IsAddressableExpr, CheckFuncProc, CheckExpr, 
    CheckExprFunction, ValueOrAddr, MakeExprCheck, MakeExprConst,
    MakeExprVar, MakeExprVal, CheckBadParams;

from Inline import
    ExpandProcInline;

$if modula2 then
from Machine import WORDSIZE;
from Symbols import CHECKDYNARRAY, CHECKSUBSCROPEN, addressTypeNode, 
    cardinalTypeNode, StmtKindSet, ArrayKindSet, ARRAYNOCOUNT,
    ARRAYOPEN, EXPRVAR, EXPRCHECK;
from CheckExpr import RefOpenArray, MakeExprSave;

$else

from CheckExpr import SetSetType;
from Symbols import jmpBufTypeNode, LabelNode, LabelList;
from BuildStmt import BuildStmtAssign;
from Strings import NonHashText;
from Decls import DefineVarInProc;
$end
    
procedure BadStmt(stn : StmtNode);
begin
    stn^.bad := true;
end BadStmt;

$if pascal then
procedure MakeStmtAssign(const lhs : ExprNode; const lhstn : TypeNode;
			 const rhs : ExprNode) : StmtNode;
var
    stn : StmtNode;
begin
    stn := BuildStmtAssign(lhs, rhs);
    SameStmtLine(stn, lhs);
    stn^.lhsType := lhstn;
    if lhs^.doCheck and ((lhstn = cardIntTypeNode) or (lhstn^.kind in
	    DataTypeSet{DTSUBRANGE,DTCHAR,DTBOOLEAN,DTENUMERATION})) then
	stn^.rhs := MakeExprCheck(stn^.rhs,CHECKRANGE,nil,nil,lhstn,
		trunc(LowerBoundOf(lhstn)),trunc(UpperBoundOf(lhstn)),nil);
    end;
    return stn;
end MakeStmtAssign;
$end

procedure CheckStmtAssign(stn : StmtNode);
var
    lhstn, rhstn, tn : TypeNode;
    aen : ExprNode;
    doSave : boolean;
    lengthen : ExprNode;
    arrayDesc : VarNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtAssign\n');
    end;
    rhstn := CheckExpr(stn^.rhs,EVALGET);
    lhstn := CheckExpr(stn^.lhs,EVALPUT);
    if IsBadExpr(stn^.lhs) or IsBadExpr(stn^.rhs) then
	BadStmt(stn);
	return;
    elsif not IsAddressableExpr(stn^.lhs) then
	StmtError(stn,'Cannot assign to left hand side');
	BadStmt(stn);
	return;
    end;
    tn := Assignable(lhstn, rhstn, stn^.rhs);
    if tn = nil then
	StmtError(stn,'Expression type cannot be assigned to variable');
	BadStmt(stn);
	return;
    end;
    stn^.lhsType := lhstn;
    lhstn := ActualType(lhstn);
$if pascal then
    if tn^.kind = DTSET then     (* May be typeless EXPRSETs in tree *)
	SetSetType(stn^.rhs, lhstn);
    elsif tn^.containsFiles then
	StmtError(stn, 'Files cannot be assigned');
    end;
$end
(* ||| why is this here? *)
    if stn^.rhs^.kind = EXPRCONST then
	stn^.rhs^.exprType := rhstn;
    end;
$if modula2 then
    if (lhstn^.kind = DTARRAY) and
	(lhstn^.arrayKind in ArrayKindSet{ARRAYNOCOUNT, ARRAYOPEN}) then
	aen := stn^.lhs;
	if (lhstn^.arrayKind = ARRAYOPEN) and (aen^.doCheck) then
	    (* Put in size check *)
	    if aen^.kind = EXPRVAR then
		(* Use open array descriptor directly *)
		arrayDesc := aen^.exprVar;
		doSave := false;
	    elsif (aen^.kind = EXPRCHECK) and 
		    (aen^.exprCheck = CHECKDYNARRAY) and
		    (aen^.checkExpr^.kind = EXPRVAR) then
		arrayDesc := aen^.checkExpr^.exprVar;
		doSave := false;
	    else
		arrayDesc :=
		    DefineVar(nil,addressTypeNode,MEMFAST, GSNORMAL, nil);
		aen := MakeExprSave(aen,arrayDesc);
		doSave := true;
	    end;
	    lengthen := 
		MakeExprConst(cardinalTypeNode,	longfloat(tn^.stringLength));
	    lengthen^.doCheck := true;
	    stn^.assignSizeCheck := MakeExprCheck(lengthen, CHECKSUBSCROPEN,
		arrayDesc, nil, cardinalTypeNode, WORDSIZE, ord(doSave), nil);
	end (* if put in size check *);
	RefOpenArray(stn^.lhs, lhstn);
	stn^.lhsType := tn;
    end;
$end
    if stn^.lhs^.doCheck then
	if ((lhstn = cardIntTypeNode) or (lhstn^.kind in
		DataTypeSet{DTSUBRANGE,DTCHAR,DTBOOLEAN,DTENUMERATION})) then
	    stn^.rhs := MakeExprCheck(stn^.rhs,CHECKRANGE,nil,nil,lhstn,
		trunc(LowerBoundOf(lhstn)),trunc(UpperBoundOf(lhstn)),nil);
	elsif (((lhstn^.kind = DTINTEGER) and (rhstn^.kind = DTCARDINAL)) or
		((lhstn^.kind = DTCARDINAL) and (rhstn^.kind = DTINTEGER)))
	      and (standardCardinalFlag) then
	    stn^.rhs := MakeExprCheck(stn^.rhs,CHECKRANGE,nil,nil,lhstn,
		0, trunc(MAXINT), nil);
	end;
    end;
    if stn^.lhs^.doPtrAssignCheck then
	if (lhstn^.kind = DTPOINTER) and (stn^.rhs^.kind # EXPRCONST) then
	    stn^.rhs := MakeExprCheck(stn^.rhs, CHECKPTRASSIGN, nil, nil, nil,
		0, 0, nil);
	    stn^.rhs^.checkPtr := lhstn^.ptrKind;
	end;
    end;
end CheckStmtAssign;

procedure CheckStmtProc(stn : StmtNode);
var
    retType, tn : TypeNode;
    proc : ProcNode;
    en : ExprNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtProc\n');
    end;
    (* If any parameters, parsing has "eaten" them into an EXPRFUNC *)
    if stn^.proc^.kind = EXPRFUNC then
	stn^.params := stn^.proc^.params;
	stn^.proc := stn^.proc^.func;
	SameStmtLine(stn, stn^.proc);
    end;
    tn := CheckExprFunction(stn^.proc,EVALPOINT);
    if IsBadExpr(stn^.proc) then
	BadStmt(stn);
	CheckBadParams(stn^.params);
    elsif stn^.proc^.kind = EXPRCONST then
	proc := stn^.proc^.exprConst^.procVal;
        if not CheckFuncProc(stn,nil,stn^.proc,stn^.params,retType, EVALGET) then
            BadStmt(stn);
        elsif proc^.inlineProc and not currProc^.inlineProc then
            ExpandProcInline(proc,stn);
        end;
    elsif stn^.proc^.kind = EXPRSYM then
	(* Statement is ProcTypeName(value), with no parameters.  Make this
	   look like ProcTypeName(value)() *)
	(* ||| will baseVar, basePtrType ever be different in en, stn^.en? *)
	new(en);
	en^ := stn^.proc^;
	en^.kind := EXPRFUNC;
	en^.selected := false;
	en^.func := stn^.proc;
	en^.params := AddToExprList(nil, nil);
	stn^.proc := en;
	stn^.params := en^.params;
	if not CheckFuncProc(stn,nil,stn^.proc,stn^.params,retType, EVALGET) then
            BadStmt(stn);
        end;
    else
	if stn^.proc^.kind # EXPRFUNC then
	    ValueOrAddr(stn^.proc,tn,EVALGET);
	end;
	if not CheckFuncProc(stn,nil,stn^.proc,stn^.params,retType, EVALGET) then
	    BadStmt(stn);
	end;
    end;
end CheckStmtProc;

procedure CheckStmtIf(stn : StmtNode);
   var tn : TypeNode;
       stmts : StmtList;
       en : ExprNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtIf\n');
    end;
    tn := CheckExpr(stn^.ifCond,EVALGET);
    if IsBadExpr(stn^.ifCond) then
	BadStmt(stn);
    elsif Assignable(booleanTypeNode, tn, stn^.ifCond) = nil
    then
	ExprErrorName(stn^.ifCond,stringDataType[stn^.ifCond^.exprType^.kind],
		'If condition is type $, not boolean');
	BadStmt(stn);
    end;
    CheckStmtList(stn^.thenList);
    CheckStmtList(stn^.elseList);
    if stn^.ifCond # nil then (* try some optimizations *)
	if stn^.ifCond^.kind = EXPRCONST then
	    (* eliminate dead code *)
	    if OrdOf(stn^.ifCond^.exprConst) # 0.0 then
		stmts := stn^.thenList;
	    else
		stmts := stn^.elseList;
	    end;
	    stn^.kind := STMTSTMTS;
	    stn^.stmts := stmts;
	elsif stn^.thenList^.first = nil then
	    (* negate test, switch then and else parts *)
	    en := BuildExprUnOp(TKNOT, stn^.ifCond);
	    SameExprLine(en, stn^.ifCond);
	    en^.unOperType := booleanTypeNode;
	    en^.exprType := booleanTypeNode;
	    en^.basePtrType := stn^.ifCond^.basePtrType;
	    en^.baseVar := stn^.ifCond^.baseVar;
	    stn^.ifCond := en;
	    stmts := stn^.thenList;
	    stn^.thenList := stn^.elseList;
	    stn^.elseList := stmts;
	end;
    end;
end CheckStmtIf;

procedure AddToCaseTree(var tree : CaseTreeNode; first, last : HugeInteger;
    caseNode : CaseNode) : boolean;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'AddToCaseTree %1.0F %1.0F\n', first, last);
    end;
    if tree = nil then
	new(tree);
	tree^.first := first;
	tree^.last := last;
	tree^.caseNode := caseNode;
	tree^.higher := nil;
	tree^.lower := nil;
	return true;
    elsif last < tree^.first then
	return AddToCaseTree(tree^.lower,first,last,caseNode);
    elsif first > tree^.last then
	return AddToCaseTree(tree^.higher,first,last,caseNode);
    else
	return false;
    end;
end AddToCaseTree;

procedure CheckCase(stn : StmtNode; caseNode : CaseNode; tn : TypeNode)
	: boolean;
var
    labels : ExprSetNode;
    tree : CaseTreeNode;
    error : boolean;
    labeltn : TypeNode;
    upper, lower : HugeInteger;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckCase\n');
    end;
    error := false;
    if (tn # nil) and (caseNode^.labels # nil) then
	labels := caseNode^.labels^.first;
	while labels # nil do
	    labeltn := CheckExpr(labels^.lower,EVALGET);
	    if IsBadExpr(labels^.lower) then
		error := true;
	    elsif labels^.lower^.kind # EXPRCONST then
		ExprError(labels^.lower,'Case labels must be constants');
		error := true;
	    elsif Assignable(tn,labeltn,nil) = nil then
		ExprError(labels^.lower,
			    'Case label incompatible with selector');
		    error := true;
	    else
		lower := OrdOf(labels^.lower^.exprConst);
		if labels^.upper = nil then
		    upper := lower;
		else
		    labeltn := CheckExpr(labels^.upper,EVALGET);
		    if IsBadExpr(labels^.upper) then
			error := true;
		    elsif labels^.upper^.kind # EXPRCONST then
			ExprError(labels^.upper,
			    'Case labels must be constants');
			error := true;
		    elsif Assignable(tn,labeltn,nil) = nil then
			ExprError(labels^.upper,
			    'Case label incompatible with selector');
			error := true;
		    else
			upper := OrdOf(labels^.upper^.exprConst);
		    end;
		end;
	    end;
	    if not error then
		if not AddToCaseTree(stn^.caseTree, lower,upper,caseNode) then
		    ExprError(labels^.lower,
			'Case label value occurs more than once');
		    error := true;
		end;
	    end;
	    labels := labels^.next;
	end;
    end (* if need to check labels *);
    CheckStmtList(caseNode^.stmts);
    return not error;
end CheckCase;

procedure CheckStmtCase(stn : StmtNode);
var
    bt : TypeNode;
    caseNode : CaseNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtCase\n');
    end;
    bt := BaseType(CheckExpr(stn^.caseSel,EVALGET));
    if IsBadExpr(stn^.caseSel) or (bt^.kind = DTANY) then
	bt := anyTypeNode;
	BadStmt(stn);
    elsif not (bt^.kind in indexableTypes) then
	StmtErrorName(stn,stringDataType[bt^.kind],
	    'Invalid expression type $ for case selector');
	bt := anyTypeNode;
	BadStmt(stn);
    end;
    if stn^.cases # nil then
	caseNode := stn^.cases^.first;
	while caseNode # nil do
	    if not CheckCase(stn,caseNode,bt) then
		BadStmt(stn);
	    end;
	    caseNode := caseNode^.next;
	end;
    end;
    CheckStmtList(stn^.caseElse);
end CheckStmtCase;

procedure CheckStmtWhile(stn : StmtNode);
var
    tn : TypeNode;
    save : boolean;
    saveLoop : StmtKind;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtWhile\n');
    end;
    tn := CheckExpr(stn^.whileCond,EVALGET);
    if IsBadExpr(stn^.whileCond) then
	BadStmt(stn);
    elsif Assignable(booleanTypeNode,tn,stn^.whileCond) = nil
    then
	ExprErrorName(stn^.whileCond,
	    stringDataType[stn^.whileCond^.exprType^.kind],
	    'While condition is type $, not boolean');
	BadStmt(stn);
    end;
    save := exitStatus.whileActive;
    exitStatus.whileActive := true;
    saveLoop := exitStatus.innermostLoop;
    exitStatus.innermostLoop := STMTWHILE;
    CheckStmtList(stn^.whileBody);
    exitStatus.whileActive := save;
    exitStatus.innermostLoop := saveLoop;
end CheckStmtWhile;

procedure CheckStmtRepeat(stn : StmtNode);
var
    tn : TypeNode;
    save : boolean;
    saveLoop : StmtKind;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtRepeat\n');
    end;
    tn := CheckExpr(stn^.repeatCond,EVALGET);
    if IsBadExpr(stn^.repeatCond) then
	BadStmt(stn);
    elsif Assignable(booleanTypeNode,tn,stn^.repeatCond) = nil
    then
	ExprErrorName(stn^.repeatCond,
	    stringDataType[stn^.repeatCond^.exprType^.kind],
	    'Repeat condition is type $, not boolean');
	BadStmt(stn);
    end;
    save := exitStatus.repeatActive;
    exitStatus.repeatActive := true;
    saveLoop := exitStatus.innermostLoop;
    exitStatus.innermostLoop := STMTREPEAT;
    CheckStmtList(stn^.repeatBody);
    exitStatus.repeatActive := save;
    exitStatus.innermostLoop := saveLoop;
end CheckStmtRepeat;

procedure CheckStmtLoop(stn : StmtNode);
var
    save : boolean;
    saveLoop : StmtKind;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtLoop\n');
    end;
    save := exitStatus.loopActive;
    exitStatus.loopActive := true;
    saveLoop := exitStatus.innermostLoop;
    exitStatus.innermostLoop := STMTLOOP;
    CheckStmtList(stn^.loopBody);
    exitStatus.loopActive := save;
    exitStatus.innermostLoop := saveLoop;
end CheckStmtLoop;

procedure CheckStmtStmts(stn : StmtNode);
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtStmts\n');
    end;
    CheckStmtList(stn^.stmts);
end CheckStmtStmts;

procedure CheckStmtFor(stn : StmtNode);
var
    sym : Symbol;
    bt, tn, at, totn, fromtn, bytn, junktn : TypeNode;
    error : boolean;
    saveIndexVar : VarNode;
    fromVal, toVal, byVal, lowerBound, upperBound : HugeInteger;
    save : boolean;
    saveLoop : StmtKind;
    vn : VarNode;
    forLimitExpr : ExprNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtFor\n');
    end;
    stn^.forWillExecute := false;
    fromtn := CheckExpr(stn^.forFrom,EVALGET);
    totn := CheckExpr(stn^.forTo,EVALGET);
    if stn^.forBy = nil then
	stn^.forBy := MakeExprConst(cardIntTypeNode, 1.0);
    else
	bytn := CheckExpr(stn^.forBy,EVALGET);
    end;
    error := true;
    if IsBadExpr(stn^.forFrom) or IsBadExpr(stn^.forTo) or
	    IsBadExpr(stn^.forBy) then
	(* do nothing *)
    else
	sym := LookUpSymbol(stn^.forIndexName,currScope);
	if sym = nil then
	    StmtErrorName(stn,stn^.forIndexName,'For index $ not declared');
	    vn := DefineVar(stn^.forIndexName, fromtn, MEMNORMAL, GSNORMAL,nil);
	elsif sym^.kind # SYMVAR then
	    StmtErrorName(stn,sym^.name,'Loop index $ must be a variable');
$if modula2 then
	elsif sym^.symVar^.address.kind = MEMPARAM then
	    StmtErrorName(stn,sym^.name,'Loop index $ must not be a parameter');
	elsif sym^.symVar^.kind = VARCONSTPARAM then
	    StmtErrorName(stn,sym^.symVar^.name,
		'Cannot use CONST parameter $ as a FOR index');
$end
	elsif sym^.symVar^.kind = VARFORINDEX then
	    StmtErrorName(stn,sym^.symVar^.name,
		'Cannot use FOR index $ as a nested FOR index');
	else
	    tn := StoredType(sym^.symVar^.varType);
	    at := ActualType(tn);
	    bt := BaseType(at);
(* ||| Mike says loop induction requires tn = at, but my code seems to work
 	    if tn # at then
		StmtErrorName(stn, sym^.name,
		    'For loop index $ must use default size and alignment');
	    els*)if not (bt^.kind in indexableTypes) then
		StmtError(stn,'For loop index variable type invalid');
	    else
		error := false;
		if Assignable(bt,fromtn,stn^.forFrom) = nil then
		    StmtError(stn,
			'For loop "from" value not assignable to index');
		    error := true;
		end;
		if Assignable(bt,totn,stn^.forTo) = nil then
		    StmtError(stn,
			'For loop "to" value not assignable to index');
		    error := true;
		end;
	    end;
	end;
	if stn^.forBy^.kind # EXPRCONST then
	    StmtError(stn,'For loop "by" value must be constant');
	    error := true;
	elsif not (stn^.forBy^.exprConst^.kind in
		DataTypeSet{DTINTEGER,DTCARDINAL})
	then
	    StmtError(stn,'For loop "by" value must be integer');
	    error := true;
	elsif stn^.forBy^.exprConst^.cardVal = 0.0 then
	    StmtError(stn,'For loop "by" value must not be zero');
	    error := true;
	end;
    end;
    if error then
	BadStmt(stn);
    else
	stn^.forIndexVar := sym^.symVar;
	stn^.forIndexType := sym^.symVar^.varType;
	if (stn^.forTo^.kind # EXPRCONST) or (target # TARGETVAX) then
	    stn^.forLimitVar := DefineVar(nil,bt,MEMFAST,GSNORMAL, nil);
	end;
	if stn^.forBy^.kind # EXPRCONST then
	    stn^.forIncVar := DefineVar(nil,bt,MEMFAST,GSNORMAL, nil);
	end;
	sym^.symVar^.kind := VARFORINDEX;
    end;
    save := exitStatus.forActive;
    exitStatus.forActive := true;
    saveLoop := exitStatus.innermostLoop;
    exitStatus.innermostLoop := STMTFOR;
    CheckStmtList(stn^.forBody);
    exitStatus.forActive := save;
    exitStatus.innermostLoop := saveLoop;
    if not error then
	sym^.symVar^.kind := VARNORMAL;
	if currProc^.inlineProc then
	    (* Don't do any of this checking stuff yet.  Do it when the actual 
	       arguments are substituted on call expansion. *)
	    return;
	end;
	lowerBound := LowerBoundOf(at);
	upperBound := UpperBoundOf(at);
	if (stn^.forFrom^.kind = EXPRCONST) then
	    fromVal := OrdOf(stn^.forFrom^.exprConst);
	    (* Do range-checking of starting value now *)
	    if stn^.forFrom^.doCheck and
		    ((fromVal < lowerBound) or (fromVal > upperBound)) then
		StmtError(stn, 
		    'For loop lower limit exceeds index variable bounds');
	    end;
	end;
	if (stn^.forTo^.kind = EXPRCONST) then
	    toVal := OrdOf(stn^.forTo^.exprConst);
	    if (stn^.forFrom^.kind = EXPRCONST) and 
		    (stn^.forBy^.kind = EXPRCONST) then
		byVal := OrdOf(stn^.forBy^.exprConst);
		if (fromVal = toVal) or ((byVal > 0.0) = (fromVal < toVal)) then
		    stn^.forWillExecute := true;
		    if stn^.forTo^.doCheck and
			    ((toVal < lowerBound) or (toVal > upperBound)) then
			StmtError(stn, 
			  'For loop upper limit exceeds index variable bounds');
		    end;
		else    
		    (* 99% of these will be errors *)
		    StmtWarning(stn, 'For loop body will never be executed');
		end;
	    end;		
	elsif stn^.forFrom^.doCheck and ((at = cardIntTypeNode) or (at^.kind in
		DataTypeSet{DTSUBRANGE,DTCHAR,DTBOOLEAN,DTENUMERATION})) then
	    stn^.forFrom := MakeExprCheck(stn^.forFrom, CHECKRANGE, nil, nil,
	        at, trunc(lowerBound), trunc(upperBound), nil);
	end;
	if (stn^.forTo^.kind # EXPRCONST) and stn^.forTo^.doCheck and 
		((at = cardIntTypeNode) or (at^.kind in 
		   DataTypeSet{DTSUBRANGE,DTCHAR,DTBOOLEAN,DTENUMERATION})) then
	    forLimitExpr :=
		MakeExprVal(at, MakeExprVar(stn^.forLimitVar, stn^.forTo));
	    stn^.forLimitCheck := MakeExprCheck(forLimitExpr, CHECKRANGE, 
		    nil, nil, at, trunc(lowerBound), trunc(upperBound), nil);
	end;
    end;
end CheckStmtFor;

procedure CheckStmtWith(stn : StmtNode);
var
    ptrToRec, rectn, recbtn : TypeNode;
    implQual : VarNode;
    wqn : WithQualNode;
    error : boolean;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtWith\n');
    end;
    error := true;
    rectn := CheckExpr(stn^.withQual,EVALPOINT);
    recbtn := ActualType(rectn);
    if IsBadExpr(stn^.withQual) or (rectn^.kind = DTANY) then
	(* do nothing *)
    elsif recbtn^.kind # DTRECORD then
	StmtError(stn,'With designator must be of type record');
    else
	error := false;
	(* save allocation; allocate a temporary for pointer to rec *)
	ptrToRec := PointerType(rectn,TKATNOCHECK);
	implQual := DefineVar(nil,ptrToRec,MEMFAST,GSNORMAL, nil);

	(* push a with qualifier record on the stack *)
	new(wqn);
	wqn^.implQual := implQual;
	wqn^.recType := rectn;
	wqn^.baseVar := stn^.withQual^.baseVar;
	wqn^.basePtrType := stn^.withQual^.basePtrType;
	wqn^.next := withQualList;
	withQualList := wqn;
	stn^.withQualNode := wqn;
	stn^.withPtrVar := implQual;

	(* open record scope to allow field names to be accessed *)
	OpenScope(recbtn^.recScope);
    end;
    CheckStmtList(stn^.withBody);
    if not error then
	EndScope;
	withQualList := withQualList^.next;
    end;
end CheckStmtWith;

procedure CheckStmtReturn(stn : StmtNode);
var
    tn, ftn : TypeNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtReturn\n');
    end;
    currProc^.returnSeen := true;
    (* NOTE: returnVal = nil is not an error in a procedure (non-function) *)
    if stn^.returnVal = nil then
	if currProc^.procType^.funcType # nil then
	    StmtError(stn,'Return statement in a function requires a value');
	    BadStmt(stn);
	end;
    elsif currProc^.procType^.funcType = nil then
	StmtError(stn,'Return statement in a non-function cannot have a value');
	BadStmt(stn);
    else
	ftn := currProc^.procType^.funcType;
(*  ||| This is the way it should be, but VAX p-code translator can't handle
    it and I'm not about to screw around any more right now.  So 4-byte arrays
    are returned by value, not reference.

	if ReferenceByPoint(ftn) then
	    tn := CheckExpr(stn^.returnVal, EVALPOINT);
	else
	    tn := CheckExpr(stn^.returnVal,EVALGET);
	end;
*)
	tn := CheckExpr(stn^.returnVal, EVALGET);
	if Assignable(ftn,tn,stn^.returnVal) = nil then
	    if tn # nil then
		StmtError(stn,'Return value not assignable to function result');
	    end;
	    BadStmt(stn);
	else
	    ftn := ActualType(ftn);
	    if stn^.returnVal^.doCheck and 
		((ftn = cardIntTypeNode) or (ftn^.kind in
		    DataTypeSet{DTSUBRANGE,DTCHAR,DTBOOLEAN,DTENUMERATION}))then
		stn^.returnVal := MakeExprCheck(stn^.returnVal,CHECKRANGE,
			nil,nil,tn,trunc(LowerBoundOf(ftn)),
			trunc(UpperBoundOf(ftn)),nil);
	    end;
	end;
    end;
    if stn^.next # nil then
	StmtWarning(stn, 'Unreachable code after RETURN');
    end;
end CheckStmtReturn;

procedure CheckStmtExit(stn : StmtNode);
begin
    if DEBUG and TraceNstmt then
	Writef(output,'CheckStmtExit\n');
    end;
    case stn^.exitKind of
    | STMTLOOP:
	if not exitStatus.loopActive then
	    StmtError(stn,'EXIT statement is not contained in a loop');
	    BadStmt(stn);
	end;
    
    | STMTFOR:
	if not exitStatus.forActive then
	    StmtError(stn,'EXIT FOR is not contained in a FOR loop');
	    BadStmt(stn);
	end;
    
    | STMTWHILE:
	if not exitStatus.whileActive then
	    StmtError(stn,'EXIT WHILE is not contained in a WHILE loop');
	    BadStmt(stn);
	end;
    
    | STMTREPEAT:
	if not exitStatus.repeatActive then
	    StmtError(stn,'EXIT REPEAT is not contained in a REPEAT loop');
	    BadStmt(stn);
	end;
	
    end;
    if exitStatus.innermostLoop # stn^.exitKind then
	StmtError(stn,'EXIT can exit only the innermost loop');
	BadStmt(stn);
    end;
    if stn^.next # nil then
	StmtWarning(stn, 'Unreachable code after EXIT');
    end;
end CheckStmtExit;

$if pascal then
procedure AddToLabelList(list         : LabelList; 
			 const newOne : LabelNode) : LabelList;
begin
    newOne^.next := nil;
    if list = nil then
	new(list);
	list^.first := newOne;
    else
	list^.last^.next := newOne;
    end;
    list^.last:= newOne;
    return list;
end AddToLabelList;

procedure CheckStmtGoto(stn : StmtNode);
    var sym : Symbol;
	vn  : VarNode;
	targetProc : ProcNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output, 'CheckStmtGoto\n');
    end;
    sym := LookUpSymbol(stn^.targetLabel^.labelName, currScope);
    if sym = nil then
	StmtErrorName(stn, stn^.targetLabel^.labelName, 
	    'Goto target label $ not declared');
	BadStmt(stn);
    else
	stn^.targetLabel := sym^.symLabel;
	stn^.OOB := (stn^.targetLabel^.proc # currProc);
	if stn^.OOB then
	    targetProc := stn^.targetLabel^.proc;
	    if stn^.targetLabel^.OOBIndex = 0 then
		(* Add this label to OOBLabelList, give it an OOBIndex *)
		inc(targetProc^.nextOOBIndex);
		stn^.targetLabel^.OOBIndex := targetProc^.nextOOBIndex;
		targetProc^.OOBLabelList := AddToLabelList(
		    targetProc^.OOBLabelList, stn^.targetLabel);
	    end;
	    if (target # TARGETVAX) and (targetProc^.jmpBuf = nil) then
		targetProc^.jmpBuf :=
		    DefineVarInProc(NonHashText("'jmpBuf"), jmpBufTypeNode,
			MEMNORMAL, GSNORMAL, nil, targetProc);
	    end;
	end;
    end;
    if (stn^.next # nil) and (stn^.next^.kind # STMTLABEL) then
	StmtWarning(stn, 'Unreachable code after GOTO');
    end;
end CheckStmtGoto;
	    
procedure CheckStmtLabel(stn : StmtNode);
    var sym : Symbol;
begin
    if DEBUG and TraceNstmt then
	Writef(output, 'CheckStmtLabel\n');
    end;
    sym := LookUpSymbol(stn^.label^.labelName, currScope);
    if (sym = nil) or (sym^.block # currProc^.block) then
	StmtErrorName(stn, stn^.label^.labelName, 
	    'Label $ not declared in current procedure/function');
	BadStmt(stn);
    else
	stn^.label := sym^.symLabel;
    end;
end CheckStmtLabel;
$end

procedure CheckStmt(stn : StmtNode);
    var saveCurrent : AllocationArray;
begin
    if stn^.bad then
	StmtError(stn,'CheckStmt: stmt already bad?');
    else
	currLine := stn^.lineNumber;
	currFile := stn^.fileName;
	saveCurrent := currProc^.mem^.current;
	case stn^.kind of
$if pascal then
	| STMTLABEL     :   CheckStmtLabel(stn);
	| STMTGOTO      :   CheckStmtGoto(stn);
$end
	| STMTASSIGN    :   CheckStmtAssign(stn);
	| STMTPROC      :   CheckStmtProc(stn);
	| STMTIF	:   CheckStmtIf(stn);
	| STMTWHILE     :   CheckStmtWhile(stn);
	| STMTREPEAT    :   CheckStmtRepeat(stn);
	| STMTLOOP      :   CheckStmtLoop(stn);
	| STMTFOR       :   CheckStmtFor(stn);
	| STMTWITH      :   CheckStmtWith(stn);
	| STMTEXIT      :   CheckStmtExit(stn);
	| STMTRETURN    :   CheckStmtReturn(stn);
	| STMTCASE      :   CheckStmtCase(stn);
	| STMTSTMTS     :   CheckStmtStmts(stn);
	end;
	(* Let Mahler take care of merging Titan memory *)
	if target = TARGETVAX then
	    currProc^.mem^.current := saveCurrent;
	end;
    end;
end CheckStmt;

procedure CheckStmtList(stl : StmtList);
var
    stn : StmtNode;
begin
    if stl = nil then
	(* do nothing *)
    else
	stn := stl^.first;
	while stn # nil do
	    CheckStmt(stn);
	    stn := stn^.next;
	end;
    end;
end CheckStmtList;

procedure CheckReturn(proc : ProcNode);
    procedure LastStatementIsFalseAssertion(stn : StmtNode) : boolean;
    begin
    return  (stn^.kind = STMTPROC) and (stn^.proc^.kind = EXPRCONST) and
	    (stn^.proc^.exprConst^.procVal^.builtin = BIPASSERT) and
	    (stn^.params # nil) and (stn^.params^.first # nil) and
	    (stn^.params^.first^.kind = EXPRCONST) and
	    (OrdOf(stn^.params^.first^.exprConst) = 0.0);
    end LastStatementIsFalseAssertion;

begin
$if modula2 then
    if proc^.body = nil then
(* |||
    ProcErrorName(proc, proc^.name,
	    'Function $ never defined (and so has no return statement');
*)
    elsif not proc^.returnSeen then 
   	ProcErrorName(proc, proc^.name,
	    'Function $ does not contain a return statement');
    elsif LastStatementIsFalseAssertion(proc^.body^.last) then
	(* No error *)
    elsif proc^.body^.last^.kind in StmtKindSet{STMTASSIGN, STMTPROC} then
	StmtErrorName(proc^.body^.last, proc^.name, 
	    'Function $ does not end with a return statement');
    end;
$else (* pascal *)
    if (proc^.body = nil) or (proc^.body^.first = nil) then
	ProcErrorName(proc, proc^.name,
	    'Function $ has no body (and no assignment to the function)');
    elsif not proc^.funcAssigned then
	ProcErrorName(proc, proc^.name,
	    'Function $ does not contain an assignment to the function');
    end;
$end
end CheckReturn;
end CheckStmt.
