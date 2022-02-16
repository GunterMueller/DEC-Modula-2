implementation module Inline;

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
    WORDSIZE, HugeInteger;

from Globals import
    debugSet, DEBUG;

from Strings import
    WriteString, String;

from Tokens import
    TKNOT;

from Symbols import
    MemoryType, MemoryTypeSet, GSNORMAL, AllocationArray, TypeNode, 
    VarNode, VarList, ParamKind, ParamNode, ProcNode, ConstNode,
    ExprKind, ExprNode, ExprList, EVALGET, ExprSetNode, ExprSetList,
    ConstSetNode, CaseTreeNode, CaseNode, CaseList, CheckKindSet,
    CheckKind, StmtKind, StmtNode, StmtList,
    InlineValueKind, InlineValueKindSet, InlineParamNode, InlineParamList,
    currProc, addressTypeNode, booleanTypeNode, cardIntTypeNode,
    DataTypeSet, DataType;

from Errors import
    Error, ErrorName, ExprErrorName, ExprErrorConst, StmtErrorName,
    StmtError, ErrorNumber, ExprErrorNumber, numberOfErrors;

from Consts import 
    OrdOf, ConstUnOp, AddToConstSetList;

from BuildExpr import
    BuildExprUnOp, SameExprLine, AddToExprSetList;

from TypeInfo import
    SizeOf, LowerBoundOf, UpperBoundOf, ActualType;

from Alloc import
    AllocateMemory;

from Decls import
    AddToVarList, DefineVar, CheckProc;

from BuildExpr import
    NewExprNode, AddToExprList;

from BuildStmt import
    NewStmtNode, AddToStmtList;

from CheckExpr import
    ValueOrAddr, EvalConstExprBinOp, MakeExprCheck, MakeExprVal, MakeExprVar,
    ConstType;

(* Inline calls get special error messages.  To make it easy to figure where
   something went wrong, the complete inline call stack is printed with the
   error message. *)

const 
    MAXINLINECALLDEPTH = 50;
var
    inlineCallDepth :  [0..MAXINLINECALLDEPTH];
    inlineCallStack : array [1..MAXINLINECALLDEPTH] of record
	procName    : String;
	fileName    : String;
	lineNumber  : cardinal;
    end (* inlineCallStack *);

type
    InlineKind = (INLINEFUNC, INLINEPROC);
    InlineInfo = record
	varList      : VarList;
	proc	     : ProcNode;
	case kind    : InlineKind of
	| INLINEPROC : inlineStmt : StmtNode;
	| INLINEFUNC : inlineExpr : ExprNode; 
		       result     : VarNode;
	end;
    end;

procedure PushInlineCall(const procName   : String;
			 const fileName   : String;
			 const lineNumber : cardinal);
begin
    if inlineCallDepth = MAXINLINECALLDEPTH then
	ErrorNumber('Inline call depth of % exceeded?', MAXINLINECALLDEPTH);
    else
	inc(inlineCallDepth);
	inlineCallStack[inlineCallDepth].procName   := procName;
	inlineCallStack[inlineCallDepth].fileName   := fileName;
	inlineCallStack[inlineCallDepth].lineNumber := lineNumber;
    end;
end PushInlineCall;

procedure PopInlineCall();
begin
    if inlineCallDepth > 0 then
	dec(inlineCallDepth);
    end;
end PopInlineCall;


procedure PrintInlineCallStack();
    var i : [1..MAXINLINECALLDEPTH];
begin
    for i := inlineCallDepth to 1 by -1 do
	Writef(output, '    (Inline procedure ');
	WriteString(output, inlineCallStack[i].procName);
	Writef(output, ' called from file ');
	WriteString(output, inlineCallStack[i].fileName);
	Writef(output, ', line %d)\n', inlineCallStack[i].lineNumber);
    end;
end PrintInlineCallStack;


procedure InlineExprList(el : ExprList; var inl : InlineInfo) : ExprList;
var
    iel : ExprList;
    en, ien : ExprNode;
begin
    iel := AddToExprList(nil, nil);
    if el # nil then
	en := el^.first;
	while en # nil do
	    ien := InlineExpr(en,inl);
	    iel := AddToExprList(iel,ien);
	    en := en^.next;
	end;
    end;
    return iel;
end InlineExprList;

procedure InlineStmtList(stl : StmtList; var inl : InlineInfo) : StmtList;
var
    istl : StmtList;
    stn, istn : StmtNode;
begin
    istl := AddToStmtList(nil,nil);
    if stl # nil then
	stn := stl^.first;
	while stn # nil do
	    istn := InlineStmt(stn,inl);
	    istl := AddToStmtList(istl,istn);
	    stn := stn^.next;
	end;
    end;
    return istl;
end InlineStmtList;

procedure InlineCaseTree(ctn : CaseTreeNode) : CaseTreeNode;
var
    ictn : CaseTreeNode;
begin
    if ctn = nil then
	return nil;
    else
	new(ictn);
	ictn^ := ctn^;
	ictn^.higher := InlineCaseTree(ctn^.higher);
	ictn^.lower := InlineCaseTree(ctn^.lower);
	(* new caseNode is successor to old *)
	ictn^.caseNode := ctn^.caseNode^.next;
    end;
    return ictn;
end InlineCaseTree;

procedure InlineStmtCase(stn, istn : StmtNode; var inl : InlineInfo);
var
    cn, icn : CaseNode;
    icl     : CaseList;
    selectcn: ConstNode;
    selector: HugeInteger;
    tree    : CaseTreeNode;
    stmts   : StmtList;
    
begin
    istn^.caseSel := InlineExpr(stn^.caseSel, inl);
    if (istn^.caseSel # nil) and (istn^.caseSel^.kind = EXPRCONST) then
	(* Don't need to test at runtime: just substitute in correct case *)
	selectcn := istn^.caseSel^.exprConst;
	selector := OrdOf(selectcn);
	tree := stn^.caseTree;
	while tree # nil do
	    if selector < tree^.first then
		tree := tree^.lower;
	    elsif selector > tree^.last then
		tree := tree^.higher;
	    else
		(* Found it *)
		stmts := tree^.caseNode^.stmts;
		exit while;
	    end;
	end;
	if tree = nil then
	    (* Matching case branch not found, use caseElse *)
	    stmts := stn^.caseElse;
	    if stmts = nil then
		ExprErrorConst(istn^.caseSel,
		    'Case selector $ has no matching case branch', selectcn);
		PrintInlineCallStack();
	    end;
	end;
	istn^.kind := STMTSTMTS;
	istn^.stmts := InlineStmtList(stmts, inl);

    else  (* Non-constant selector *)
	(* duplicate caseTree binary tree and cases linked list *)
	cn := stn^.cases^.first;
	(* duplicate each node in the linked list *)
	while cn # nil do
	    new(icn);
	    icn^ := cn^;
	    (* labels isn't used later, so no need to copy it *)
	    icn^.stmts := InlineStmtList(cn^.stmts,inl);
	    cn^.next := icn;
	    cn := icn^.next;
	end;
	(* duplicate the binary sort tree *)
	istn^.caseTree := InlineCaseTree(stn^.caseTree);
	(* split the linked list into two lists *)
	icl := nil;
	cn := stn^.cases^.first;
	while cn # nil do
	    (* remove second node *)
	    icn := cn^.next;
	    cn^.next := icn^.next;
	    (* add it to the new case list *)
	    icn^.next := nil;
	    if icl = nil then
		new(icl);
		icl^.first := icn;
	    else
		icl^.last^.next := icn;
	    end;
	    icl^.last := icn;
	    cn := cn^.next;
	end;
	istn^.cases := icl;
	istn^.caseElse := InlineStmtList(stn^.caseElse, inl);
    end;
end InlineStmtCase;

procedure InlineStmt(stn : StmtNode; var inl : InlineInfo) : StmtNode;
var
    istn : StmtNode;
    sal : AllocationArray;
    proc : ProcNode;
    stmts : StmtList;
    en, forLimitExpr    : ExprNode;
    lowerBound, upperBound, fromVal, toVal, byVal : HugeInteger;
    at : TypeNode;
begin
    istn := NewStmtNode(stn^.kind);
    istn^ := stn^;
    sal := currProc^.mem^.current;
    case stn^.kind of
    | STMTNONE :    (* nothing *);

    | STMTASSIGN :
	istn^.rhs := InlineExpr(stn^.rhs,inl);
	istn^.lhs := InlineExpr(stn^.lhs,inl);
(* |||	istn^.assignSizeCheck := MakeExprCheck(assignSizeExpr, CHECKRANGE, 
		    nil, nil, at, trunc(lowerBound), trunc(upperBound), nil);
    *)

    | STMTPROC :
	istn^.proc := InlineExpr(stn^.proc,inl);
	istn^.params := InlineExprList(stn^.params,inl);
	if stn^.proc^.kind = EXPRCONST then
	    proc := stn^.proc^.exprConst^.procVal;
	    if proc^.inlineProc then
		ExpandProcInline(proc,istn);
	    end;
	end;
    
    | STMTIF :
	istn^.ifCond := InlineExpr(stn^.ifCond,inl);
	istn^.thenList := InlineStmtList(stn^.thenList,inl);
	istn^.elseList := InlineStmtList(stn^.elseList,inl);
	if istn^.ifCond # nil then (* try some optimizations *)
	    if istn^.ifCond^.kind = EXPRCONST then
		(* Don't need to test, just substitute then part or else part *)
		if OrdOf(istn^.ifCond^.exprConst) # 0.0 then
		    stmts := istn^.thenList;
		else
		    stmts := istn^.elseList;
		end;
		istn^.kind := STMTSTMTS;
		istn^.stmts := stmts;
	    elsif istn^.thenList^.first = nil then
		(* negate test, switch then and else parts *)
		en := BuildExprUnOp(TKNOT, istn^.ifCond);
		SameExprLine(en, istn^.ifCond);
		en^.unOperType := booleanTypeNode;
		en^.exprType := booleanTypeNode;
		en^.basePtrType := istn^.ifCond^.basePtrType;
		en^.baseVar := istn^.ifCond^.baseVar;
		istn^.ifCond := en;
		stmts := istn^.thenList;
		istn^.thenList := istn^.elseList;
		istn^.elseList := stmts;
	    end;
	end;

    | STMTWHILE :
	istn^.whileCond := InlineExpr(stn^.whileCond,inl);
	istn^.whileBody := InlineStmtList(stn^.whileBody,inl);
    
    | STMTREPEAT :
	istn^.repeatBody := InlineStmtList(stn^.repeatBody,inl);
	istn^.repeatCond := InlineExpr(stn^.repeatCond,inl);
    
    | STMTLOOP :
	istn^.loopBody := InlineStmtList(stn^.loopBody,inl);

    | STMTSTMTS :
	istn^.stmts := InlineStmtList(stn^.stmts,inl);

    | STMTFOR :
	assert(stn^.forIndexVar^.name # nil, 'Anonymous for loop index');
	istn^.forIndexVar := InlineVar(stn^.forIndexVar);
	if stn^.forLimitVar # nil then
	    istn^.forLimitVar := DefineInlineVar(stn^.forLimitVar,
				    stn^.forLimitVar^.varType,MEMFAST,inl);
	    stn^.forLimitVar^.address.inlineVal.kind := ILVAR;
	    stn^.forLimitVar^.address.inlineVal.ilVar := istn^.forLimitVar;
	end;
	if stn^.forIncVar # nil then
	    istn^.forIncVar := DefineInlineVar(stn^.forIncVar,
				    stn^.forIncVar^.varType,MEMFAST,inl);
	    stn^.forIncVar^.address.inlineVal.kind := ILVAR;
	    stn^.forIncVar^.address.inlineVal.ilVar := istn^.forIncVar;
	end;
	istn^.forFrom := InlineExpr(stn^.forFrom,inl);
	istn^.forTo := InlineExpr(stn^.forTo,inl);
	istn^.forBy := InlineExpr(stn^.forBy,inl);
	istn^.forBody := InlineStmtList(stn^.forBody,inl);
	at := ActualType(stn^.forIndexVar^.varType);
	lowerBound := LowerBoundOf(at);
	upperBound := UpperBoundOf(at);
	if (istn^.forFrom^.kind = EXPRCONST) then
	    fromVal := OrdOf(istn^.forFrom^.exprConst);
	    (* Do range-checking of starting value now *)
	    if istn^.forFrom^.doCheck and
		    ((fromVal < lowerBound) or (fromVal > upperBound)) then
		StmtError(stn, 
		    'For loop lower limit exceeds index variable bounds');
		PrintInlineCallStack();
	    end;
	end;
	if (istn^.forTo^.kind = EXPRCONST) then
	    toVal := OrdOf(istn^.forTo^.exprConst);
	    if (istn^.forFrom^.kind = EXPRCONST) and 
		    (istn^.forBy^.kind = EXPRCONST) then
		byVal := OrdOf(istn^.forBy^.exprConst);
		if (fromVal = toVal) or ((byVal > 0.0) = (fromVal < toVal)) then
		    istn^.forWillExecute := true;
		    if istn^.forTo^.doCheck and
			    ((toVal < lowerBound) or (toVal > upperBound)) then
			StmtError(stn, 
			    'For loop upper limit exceeds index variable bounds');
			PrintInlineCallStack();
		    end;
		else    
		    (* 99% of these will be errors *)
		    StmtError(istn, 'For loop body will never be executed');
		    PrintInlineCallStack();
		end;
	    end;		
	elsif istn^.forFrom^.doCheck and ((at = cardIntTypeNode) or (at^.kind in
		DataTypeSet{DTSUBRANGE,DTCHAR,DTBOOLEAN,DTENUMERATION})) then
	    istn^.forFrom :=
		MakeExprCheck(istn^.forFrom, CHECKRANGE, nil, nil, at,
		trunc(lowerBound), trunc(upperBound), nil);
	end;
	if (istn^.forTo^.kind # EXPRCONST) and istn^.forTo^.doCheck and 
		((at = cardIntTypeNode) or (at^.kind in 
		    DataTypeSet{DTSUBRANGE,DTCHAR,DTBOOLEAN,DTENUMERATION}))then
	    forLimitExpr := MakeExprVal(at, 
				MakeExprVar(istn^.forLimitVar, istn^.forTo));
	    istn^.forLimitCheck := MakeExprCheck(forLimitExpr, CHECKRANGE, 
		    nil, nil, at, trunc(lowerBound), trunc(upperBound), nil);
	end;
    
    | STMTINLINE :
	StmtErrorName(stn,inl.proc^.name,'Inline procedure $ expanded too soon?');

    | STMTWITH :
	istn^.withQual := InlineExpr(stn^.withQual,inl);
	istn^.withPtrVar := DefineInlineVar(stn^.withPtrVar,
				stn^.withPtrVar^.varType,MEMFAST,inl);
	stn^.withPtrVar^.address.inlineVal.kind := ILVAR;
	stn^.withPtrVar^.address.inlineVal.ilVar := istn^.withPtrVar;
	istn^.withBody := InlineStmtList(stn^.withBody,inl);
    
    | STMTRETURN :
	if stn^.returnVal = nil then
	    istn^.inlineStmt := inl.inlineStmt;
	else
	    istn^.returnVal := InlineExpr(stn^.returnVal,inl);
	    istn^.inlineExpr := inl.inlineExpr;
	    istn^.inlineVarExpr := 
		MakeExprVar(inl.inlineExpr^.inlineResultVar, istn^.returnVal);
	end;
    
    | STMTEXIT :
	(* nothing to do *)
    
    | STMTCASE :
	InlineStmtCase(stn,istn,inl);
    
    end;
    currProc^.mem^.current := sal;
    return istn;
end InlineStmt;

procedure DefineInlineVar(    vn  : VarNode; tn : TypeNode; mt : MemoryType; 
			  var inl : InlineInfo) : VarNode;
var
    ivn : VarNode;
begin
    assert(vn^.address.kind = MEMINLINE);
    new(ivn);
    ivn^ := vn^;
    if mt in MemoryTypeSet{MEMPARAM,MEMNORMAL} then
	if SizeOf(tn) <= WORDSIZE then
	    mt := MEMFAST;
	else
	    mt := MEMNORMAL;
	end;
    end;
    AllocateMemory(currProc^.mem,mt,SizeOf(tn),WORDSIZE,currProc,ivn^.address);
    if DEBUG and ('z' in debugSet) then
	Writef(output,'DefineInlineVar: ');
	WriteString(output,vn^.name);
	Writef(output,' kind = %n offset = %d\n', ivn^.address.kind,
		ivn^.address.offset);
    end;
    if vn^.name # nil then
	inl.varList := AddToVarList(inl.varList,ivn);
    end;
    return ivn;
end DefineInlineVar;

procedure InlineVar(vn : VarNode) : VarNode;
var
    ivn : VarNode;
begin
    if vn = nil then
	return nil;
    elsif vn^.address.kind = MEMINLINE then
	case vn^.address.inlineVal.kind of
	| ILVAR     :   return vn^.address.inlineVal.ilVar;
	| IPVAR     :   return vn^.address.inlineVal.ipVar;
	| IPCONST   :   Error('InlineVar: const?');
	| IPPARAM   :   return vn^.address.inlineVal.ipParam;
	end;
    else
	return vn;
    end;
    return ivn;
end InlineVar;

procedure InlineExpr(en : ExprNode; var inl : InlineInfo) : ExprNode;
var
    ien  : ExprNode;
    proc : ProcNode;
    con  : ConstNode;
    iesl : ExprSetList;
    esn, iesn : ExprSetNode;
    icsn : ConstSetNode;
    lower, upper : ExprNode;
    constVal : integer;

begin
    ien := NewExprNode(en^.kind);
    ien^ := en^;
    case en^.kind of
    | EXPRSYM, EXPRNAME, EXPRBAD :
	(* nothing to do *)
    
    | EXPRBINOP :
	ien^.opnd1 := InlineExpr(en^.opnd1,inl);
	ien^.opnd2 := InlineExpr(en^.opnd2,inl);
	if ien^.opnd1^.baseVar # nil then
	    ien^.baseVar := ien^.opnd1^.baseVar;
	else
	    ien^.baseVar := ien^.opnd2^.baseVar;
	end;
	if ien^.opnd1^.basePtrType # nil then
	    ien^.basePtrType := ien^.opnd1^.basePtrType;
	else
	    ien^.basePtrType := ien^.opnd2^.basePtrType;
	end;
	EvalConstExprBinOp(ien);

    | EXPRUNOP :
	ien^.opnd := InlineExpr(en^.opnd,inl);
	ien^.baseVar := ien^.opnd^.baseVar;
	ien^.basePtrType := ien^.opnd^.basePtrType;
	if ien^.opnd^.kind = EXPRCONST then
	    con := ConstUnOp(ien^.exprUnOp,ien^.opnd^.exprConst);
	    ien^.kind := EXPRCONST;
	    ien^.exprConst := con;
	    if en^.exprType^.kind in DataTypeSet{DTINTEGER, DTCARDINAL} then
		ien^.exprType := ConstType(con);
	    end;
	    ien^.constType := ien^.exprType;
	end;
    
    | EXPRVAR :
	if en^.exprVar^.address.kind # MEMINLINE then
	    ien^.exprVar := en^.exprVar;
	    if en^.exprVar^.address.kind = MEMGLOBAL then
		en^.exprVar^.address.gvn^.used := true;
	    end;
	elsif en^.exprVar^.address.inlineVal.kind = IPCONST then
	    ien^.kind := EXPRCONST;
	    ien^.exprConst := en^.exprVar^.address.inlineVal.ipConst;
	    if en^.exprType^.kind in DataTypeSet{DTINTEGER, DTCARDINAL} then
		ien^.exprType := ConstType(ien^.exprConst);
	    end;
	    ien^.constType := ien^.exprType;
	    ien^.baseVar := nil;
	    ien^.basePtrType := nil;
	elsif (en^.exprVar^.address.inlineVal.kind = IPPARAM) and
		en^.exprVar^.indirect then
	    (* reference parameter, need indirection *)
	    ien^.exprVar := en^.exprVar^.address.inlineVal.ipParam;
	    ien^.baseVar := ien^.exprVar;
	    ValueOrAddr(ien,ien^.exprType,EVALGET);
	    ien^.baseVar := en^.exprVar^.address.inlineVal.ipParamBaseVar;
	    ien^.basePtrType :=
		en^.exprVar^.address.inlineVal.ipParamBasePtrType;
	else
	    ien^.exprVar := InlineVar(en^.exprVar);
	    ien^.exprVar := InlineVar(en^.baseVar);
	end;
    
    | EXPRVAL :
	if en^.exprVal^.kind = EXPRVAR then
	    (* check for inline variable reference *)
	    if en^.exprVal^.exprVar^.address.kind # MEMINLINE then
		ien^.exprVal := InlineExpr(en^.exprVal,inl);
		ien^.baseVar := InlineVar(en^.baseVar);
	    elsif en^.exprVal^.exprVar^.address.inlineVal.kind = IPCONST then
		ien^.kind := EXPRCONST;
		ien^.exprConst :=
		    en^.exprVal^.exprVar^.address.inlineVal.ipConst;
		if en^.exprType^.kind in DataTypeSet{DTINTEGER, DTCARDINAL} then
		    ien^.exprType := ConstType(ien^.exprConst);
		end;
		ien^.constType := ien^.exprType;
	    else
		ien^.exprVal := InlineExpr(en^.exprVal,inl);
		ien^.dependVar := ien^.exprVal^.baseVar;
		ien^.dependPtrType := ien^.exprVal^.basePtrType;
	    end;
	else
	    ien^.exprVal := InlineExpr(en^.exprVal,inl);
	    ien^.dependVar := ien^.exprVal^.baseVar;
	    ien^.dependPtrType := ien^.exprVal^.basePtrType;
	end;
    
    | EXPRCONST :
	(* nothing necessary *)

    | EXPRFUNC :
	ien^.func := InlineExpr(en^.func,inl);
	ien^.params := InlineExprList(en^.params,inl);
	if en^.func^.kind = EXPRCONST then
	    proc := en^.func^.exprConst^.procVal;
	    if proc^.inlineProc then
		ExpandFuncInline(proc,ien);
	    end;
	end;
    
    | EXPRINLINE :
	ExprErrorName(en,inl.proc^.name,'Inline function $ expanded too soon?');

    | EXPRDESCRIPTOR :
	ien^.descripBase := InlineExpr(en^.descripBase, inl);
	ien^.descrips := InlineExprList(en^.descrips, inl);

    | EXPRSAVE :
	(* don't try to deallocate storage for variable, since it extends *)
	(*  outside this expression *)
	ien^.exprSaveVar := DefineInlineVar(ien^.exprSaveVar,
				ien^.exprSaveVar^.varType,MEMFAST,inl);
	en^.exprSaveVar^.address.inlineVal.kind := ILVAR;
	en^.exprSaveVar^.address.inlineVal.ilVar := ien^.exprSaveVar;
	ien^.exprSave := InlineExpr(en^.exprSave,inl);
	ien^.baseVar := ien^.exprSave^.baseVar;
	ien^.basePtrType := ien^.exprSave^.basePtrType;

    | EXPRCHECK :
	ien^.checkExpr := InlineExpr(en^.checkExpr,inl);
	if (ien^.exprCheck in CheckKindSet{CHECKSUBSCR,CHECKRANGE}) and
		(ien^.checkExpr^.kind = EXPRCONST) then
	    (* Do the check now *)
	    constVal := trunc(OrdOf(ien^.checkExpr^.exprConst));
	    if (ien^.checkType^.kind = DTINTEGER) and 
		    ((constVal < ien^.checkLower) or 
		    (constVal > ien^.checkUpper)) then
		ExprErrorNumber(ien^.checkExpr, 
			'Constant value % exceeds range bounds', constVal);
		PrintInlineCallStack();
	    elsif (unsigned(constVal) < unsigned(ien^.checkLower)) or 
		    (unsigned(constVal) > unsigned(ien^.checkUpper)) then
		ExprErrorNumber(ien^.checkExpr, 
			'Constant value % exceeds range bounds', constVal);
		PrintInlineCallStack();
	    end;
	    ien := ien^.checkExpr;
	else
	    if en^.checkVar # nil then
		ien^.checkVar := InlineVar(en^.checkVar);
	    end;
	    if en^.arrVar # nil then
		ien^.arrVar := InlineVar(en^.arrVar);
	    end;
	    ien^.baseVar := ien^.checkExpr^.baseVar;
	    ien^.basePtrType := ien^.checkExpr^.basePtrType;
	end;

    | EXPRSET :
	iesl := nil;
	if en^.setExpr # nil then
	    esn := en^.setExpr^.first;
	    (* Scan through "expr" part...add any CONST ranges to the
	       setConst list, non-const ranges to exprConst list *)
	    while esn # nil do
		lower := InlineExpr(esn^.lower, inl);
		if esn^.upper = nil then
		    if lower^.kind = EXPRCONST then
			new(icsn);
			icsn^.lower := lower^.exprConst;
			icsn^.upper := nil;
			ien^.setConst := AddToConstSetList(ien^.setConst, icsn);
		    else
			new(iesn);
			iesn^.lower := lower;
			iesn^.upper := nil;
			iesl := AddToExprSetList(iesl, iesn);
		    end;
		else
		    upper := InlineExpr(esn^.upper, inl);
		    if (lower^.kind = EXPRCONST) and 
			    (upper^.kind = EXPRCONST) then
			new(icsn);
			icsn^.lower := lower^.exprConst;
			icsn^.upper := upper^.exprConst;
			ien^.setConst := AddToConstSetList(ien^.setConst, icsn);
		    else
			new(iesn);
			iesn^.lower := lower;
			iesn^.upper := upper;
			iesl := AddToExprSetList(iesl, iesn);
		    end;
		end;
		esn := esn^.next;
	    end (* while *);
	end (* if *);
	ien^.setExpr := iesl;
    end (* case *);

    (* fix up baseVar (note: ien on rhs in case already fixed up) *)
    ien^.baseVar := InlineVar(ien^.baseVar);
    return ien;
end InlineExpr;

procedure InlineParams(proc : ProcNode; params : ExprList; var inl : InlineInfo)
		: InlineParamList;
var
    pn      : ParamNode;
    pen     : ExprNode;
    vn      : VarNode;
    ipn     : InlineParamNode;
    ipl     : InlineParamList;
    done    : boolean;
    atn     : TypeNode;
    i       : cardinal;
begin
    ipl := nil;
    (* allocate variables for parameters as necessary *)
    (* for const formals with const actuals, substitute constant *)
    (* for const or var formals with var actuals, substitute var *)
    (* otherwise, allocate var for parameter *)
    if (proc^.procType^.paramList # nil) and (params # nil) then
	pn := proc^.procType^.paramList^.first;
	pen := params^.first;
	while pn # nil do
	    new(ipn);
	    case pn^.kind of
	    | PARAMCONST :
		done := false;
		if pen^.kind = EXPRCONST then
		    (* constant as const parameter, just substitute *)
		    ipn^.kind := IPCONST;
		    pn^.paramVar^.address.inlineVal.kind := IPCONST;
		    pn^.paramVar^.address.inlineVal.ipConst := pen^.exprConst;
		    done := true;
		elsif pn^.reference then
		    (* reference const parameter, allocate address *)
		    vn := DefineInlineVar(pn^.paramVar,
				addressTypeNode,MEMPARAM,inl);
		    vn^.indirect := true;
		    ipn^.kind := IPPARAM;
		    ipn^.formal := vn;
		    pn^.paramVar^.address.inlineVal.kind := IPPARAM;
		    pn^.paramVar^.address.inlineVal.ipParam := vn;
		    pn^.paramVar^.address.inlineVal.ipParamBaseVar :=
					pen^.baseVar;
		    pn^.paramVar^.address.inlineVal.ipParamBasePtrType :=
					pen^.basePtrType;
		    done := true;
		elsif pen^.kind = EXPRVAL then
		    if pen^.exprVal^.kind = EXPRVAR then
			(* variable as const parameter, just substitute *)
			ipn^.kind := IPVAR;
			pn^.paramVar^.address.inlineVal.kind := IPVAR;
			pn^.paramVar^.address.inlineVal.ipVar := 
				pen^.exprVal^.exprVar;
			done := true;
		    end;
		end;
		if not done then
		    (* value const parameter, allocate value *)
		    vn := DefineInlineVar(pn^.paramVar,
				pn^.paramVar^.varType,MEMPARAM,inl);
		    ipn^.kind := IPPARAM;
		    ipn^.formal := vn;
		    pn^.paramVar^.address.inlineVal.kind := IPPARAM;
		    pn^.paramVar^.address.inlineVal.ipParam := vn;
		    pn^.paramVar^.address.inlineVal.ipParamBaseVar :=
					pen^.baseVar;
		    pn^.paramVar^.address.inlineVal.ipParamBasePtrType :=
					pen^.basePtrType;
		end;
	
	    | PARAMVALUE :
		if pn^.reference then
		    (* multiword parameter: allocate address *)
		    vn := DefineInlineVar(pn^.paramVar,addressTypeNode,
			    MEMPARAM,inl);
		    vn^.varType := pn^.paramType;
		    vn^.indirect := true;
		else
		    vn := DefineInlineVar(pn^.paramVar,pn^.paramType,
			    MEMPARAM,inl);
		end;
		ipn^.kind := IPPARAM;
		ipn^.formal := vn;
		pn^.paramVar^.address.inlineVal.kind := IPPARAM;
		pn^.paramVar^.address.inlineVal.ipParam := vn;
		pn^.paramVar^.address.inlineVal.ipParamBaseVar :=
				    pen^.baseVar;
		pn^.paramVar^.address.inlineVal.ipParamBasePtrType :=
				    pen^.basePtrType;
	    
	    | PARAMVAR :
		if pen^.kind = EXPRVAR then
		    (* variable as var parameter, just substitute *)
		    ipn^.kind := IPVAR;
		    pn^.paramVar^.address.inlineVal.kind := IPVAR;
		    pn^.paramVar^.address.inlineVal.ipVar := pen^.exprVar;
		else
		    (* other var parameter, allocate address *)
		    vn := DefineInlineVar(pn^.paramVar,addressTypeNode,MEMPARAM,inl);
		    vn^.indirect := true;
		    ipn^.kind := IPPARAM;
		    ipn^.formal := vn;
		    pn^.paramVar^.address.inlineVal.kind := IPPARAM;
		    pn^.paramVar^.address.inlineVal.ipParam := vn;
		    pn^.paramVar^.address.inlineVal.ipParamBaseVar :=
					pen^.baseVar;
		    pn^.paramVar^.address.inlineVal.ipParamBasePtrType :=
					pen^.basePtrType;
		end;
	    
	    | PARAMARRAYVAR, PARAMARRAYVALUE, PARAMARRAYCONST :
		vn := DefineInlineVar(pn^.paramVar,pn^.paramType,MEMPARAM,inl);
		ipn^.kind := IPPARAM;
		ipn^.formal := vn;
		pn^.paramVar^.address.inlineVal.kind := IPPARAM;
		pn^.paramVar^.address.inlineVal.ipParam := vn;
		pn^.paramVar^.address.inlineVal.ipParamBaseVar :=
				    pen^.baseVar;
		pn^.paramVar^.address.inlineVal.ipParamBasePtrType :=
				    pen^.basePtrType;
	    end (* case *);
	    ipl := AddToInlineParamList(ipl,ipn);
	    pn := pn^.next;
	    pen := pen^.next;
	end;
    end;
    return ipl;
end InlineParams;

procedure InlineVarsParams(varList : VarList; var inl : InlineInfo);
var
    vn : VarNode;
begin
    if varList # nil then
	vn := varList^.first;
	while vn # nil do
	    if vn^.address.inlineVal.kind in
		    InlineValueKindSet{ILVAR,IPPARAM} then
		(* allocate a variable for it *)
		vn^.address.inlineVal.ilVar :=
		    DefineInlineVar(vn,vn^.varType,MEMNORMAL,inl);
	    end;
	    vn := vn^.next;
	end;
    end;
end InlineVarsParams;

procedure InlineVars(varList : VarList; var inl : InlineInfo);
var
    vn : VarNode;
begin
    if varList # nil then
	vn := varList^.first;
	while vn # nil do
	    if vn^.address.inlineVal.kind = ILVAR then
		(* allocate a variable for it *)
		vn^.address.inlineVal.ilVar :=
			DefineInlineVar(vn,vn^.varType,MEMNORMAL,inl);
	    end;
	    vn := vn^.next;
	end;
    end;
end InlineVars;

procedure ExpandFuncInline(proc : ProcNode; en : ExprNode);
var
    sal     : AllocationArray;
    istl    : StmtList;
    inl     : InlineInfo;
    params  : ExprList;
    vn      : VarNode;
    saveNumberOfErrors : cardinal;
begin
    saveNumberOfErrors := numberOfErrors;
    PushInlineCall(proc^.name, en^.fileName, en^.lineNumber);

    (* first ensure that procedure has been type-checked *)
    if not proc^.checked then
	if proc^.beingChecked then
	    ErrorName(proc^.name,'Inline procedure $ may not be recursive');
	    PrintInlineCallStack();
	elsif proc^.body = nil then
	    ErrorName(proc^.name, 'Inline procedure $ was never defined');
	    PrintInlineCallStack();
	else
	    CheckProc(proc);
	end;
    end;
    if proc^.inlineBeingExpanded then
	ErrorName(proc^.name,'Inline procedure $ may not be recursive');
	PrintInlineCallStack();
    end;
    if saveNumberOfErrors = numberOfErrors then
	proc^.inlineBeingExpanded := true;
	params := en^.params;
	en^.kind := EXPRINLINE;
	en^.inlineProc := proc;
	en^.inlineParams := params;
	inl.proc := proc;
	inl.kind := INLINEFUNC;
	inl.inlineExpr := en;
	inl.varList := nil;
    
	(* allocate variable for return value outside save/restore *)
	vn := DefineVar(nil,proc^.procType^.funcType,MEMNORMAL,GSNORMAL,nil);
	en^.inlineResultVar := vn;
	en^.inlineResult := MakeExprVal(vn^.varType, MakeExprVar(vn, en));
    
	(* allocate parameters *)
	en^.inlineFormals := InlineParams(proc,params,inl);
    
	(* allocate vars *)
	InlineVars(proc^.varList,inl);
    
	en^.inlineVars := inl.varList;
    
	(* copy procedure body inline *)
	istl := InlineStmtList(proc^.body,inl);
	en^.inlineBody := istl;
    
	proc^.inlineBeingExpanded := false;
    end;

    PopInlineCall();
end ExpandFuncInline;


procedure ExpandProcInline(proc : ProcNode; stn : StmtNode);
var
    sal     : AllocationArray;
    istl    : StmtList;
    inl     : InlineInfo;
    params  : ExprList;
    saveNumberOfErrors : cardinal;
begin
    saveNumberOfErrors := numberOfErrors;
    PushInlineCall(proc^.name, stn^.fileName, stn^.lineNumber);

    (* first ensure that procedure has been type-checked *)
    if not proc^.checked then
	if proc^.beingChecked then
	    ErrorName(proc^.name,'Inline procedure $ may not be recursive');
	    PrintInlineCallStack();
	elsif proc^.body = nil then
	    ErrorName(proc^.name, 'Inline procedure $ was never defined');
	    PrintInlineCallStack();
	else
	    CheckProc(proc);
	end;
    end;
    if proc^.inlineBeingExpanded then
	ErrorName(proc^.name,'Inline procedure $ may not be recursive');
	PrintInlineCallStack();
    end;
    if saveNumberOfErrors = numberOfErrors then
	proc^.inlineBeingExpanded := true;
	params := stn^.params;
	stn^.kind := STMTINLINE;
	stn^.inlineProc := proc;
	stn^.inlineParams := params;
	(* ensure procedure is OK before expanding *)
	inl.proc := proc;
	inl.kind := INLINEPROC;
	inl.inlineStmt := stn;
	inl.varList := nil;
    
	(* allocate parameters *)
	stn^.inlineFormals := InlineParams(proc,params,inl);
    
	(* allocate vars *)
	InlineVars(proc^.varList,inl);
    
	stn^.inlineVars := inl.varList;
    
	(* copy procedure body inline *)
	istl := InlineStmtList(proc^.body,inl);
	stn^.inlineBody := istl;
    
	proc^.inlineBeingExpanded := false;
    end;

    PopInlineCall();
end ExpandProcInline;


procedure AddToInlineParamList(list : InlineParamList; newOne : InlineParamNode) 
		: InlineParamList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
	list^.last := nil;
    end;
    if newOne # nil then
        newOne^.next := nil;
        if list^.last = nil then
            list^.first := newOne;
        else
            list^.last^.next := newOne;
        end;
        list^.last:= newOne;
    end;
    return list;
end AddToInlineParamList;


begin (* Inline *)
    inlineCallDepth := 0;
end Inline.
