implementation module OTree;

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
    output, Writef, Writec;

from Globals import
    debugSet, DEBUG, TraceOptim, TraceCount, OptNind;

from Tokens import
    TKAND, TKAMPERSAND, TKOR, TKPLUS, TKASTERISK, TokenSet;

from Strings import
    WriteString;

from Symbols import
    OptTime, EvalMode, SYMTYPE, Symbol,
    MemoryType,MemoryTypeSet, DTPROC, TypeNode, VarNode, ModuleNode, 
    ParamKind, ParamKindSet, ParamNode, IPPARAM, InlineParamNode, ProcNode, 
    ParamList, InlineParamList,
    ExprKind, ExprNode, ExprList, StmtNode, StmtList, CaseNode, ExprSetNode, 
    CodeNode, BIPNOTBIP, StmtKind, OptNode, addressTypeNode, globalModule,
    MODDEFINITION;
    
from BuildExpr import
    NewExprNode, SameExprLine, WriteExpr;

from BuildStmt import
    PrintStmtList;

from OptBuiltin import
    OptBuiltin;

from Errors import
    Error, ExprError, StmtError;

from OCount import
    ReduceExprNeededCounts, CountProc;
    
from Optim import
    optTime, markAllOptTime, SaveOptLevel, StartOptProc, EndOptProc,
    currOptProc, StartOptSplit, NextOptSplit, EndOptSplit, 
    StartOptLoop, EndOptLoop, OptRecursionProc, OptRecursionReturn, 
    AnalyzeForLoop, MarkOptExpr, MarkOptSave, MarkOptProcCall,
    EnterExpr, DumpOptExprs;

$if pascal then    
from Optim import MarkOptAll;
$end

var
    exprDepth : integer;  (* to suppress reordering of top level expressions *)

(* compute latest time, preserving sign encoding of mark *)
procedure Latest(a,b : OptTime) : OptTime;
begin
    if (a < 0) or (b < 0) then
	if abs(a) > abs(b) then
	    return -abs(a);
	else
	    return -abs(b);
	end;
    else
	if a > b then
	    return a;
	else
	    return b;
	end;
    end;
end Latest;

procedure OptVarNode(const vn : VarNode) : OptTime;
var
    tn  : TypeNode;
begin
    if (vn^.address.kind in MemoryTypeSet{MEMNORMAL,MEMFAST,MEMPARAM}) and
	    (vn^.address.proc # currOptProc) then
	(* Up-level reference to this variable *)
	vn^.address.upLevelAddr := true;
    end;

    tn := vn^.varType;
    if vn^.markTime.proc = currOptProc then
	if tn^.markTime.proc = currOptProc then
	    return Latest(Latest(vn^.markTime.time, markAllOptTime),
			  tn^.markTime.time);
	else
	    return Latest(vn^.markTime.time, markAllOptTime);
	end;
    else
	if tn^.markTime.proc = currOptProc then
	    return Latest(markAllOptTime, tn^.markTime.time);
	else
	    return markAllOptTime;
	end;
    end;
end OptVarNode;

procedure OptExprVar(en : ExprNode) : OptTime;
begin
    return OptVarNode(en^.exprVar);
end OptExprVar;

procedure OptInline(inlineParams  : ExprList;
                    inlineFormals : InlineParamList;
                    paramList     : ParamList;
		    parent        : ExprNode);
var
    ipn       : InlineParamNode;
    pen, ipen : ExprNode;
    pn        : ParamNode;
    ignore    : OptTime;
begin
    if (inlineParams = nil) or (paramList = nil) then
	(* do nothing *)
    else
	(* first allow expressions to be referenced *)
	pen := inlineParams^.first;
	pn := paramList^.first;
	ipn := inlineFormals^.first;
	while pen # nil do
	    if ipn^.kind = IPPARAM then
		ignore := OptExpr(pen, parent);
		ipen := NewExprNode(EXPRVAR);
		SameExprLine(ipen, pen);
		ipen^.exprType := ipn^.formal^.varType;
		ipen^.exprVar := ipn^.formal;
		ipen^.baseVar := ipn^.formal;
		ignore := OptExpr(ipen, nil);
		(* mark it to indicate assignment at start of procedure *)
		MarkOptExpr(ipen);
		(* reduce counts on phony var expr *)
		ReduceExprNeededCounts(ipen, 1, false);
	    end;
	    pen := pen^.next;
	    pn := pn^.next;
	    ipn := ipn^.next;
	end (* while pen # nil *);
    end;
end OptInline;

procedure OptExprInline(en : ExprNode): OptTime;
var ignore : OptTime;
begin
    OptInline(en^.inlineParams, en^.inlineFormals,
	en^.inlineProc^.procType^.paramList, en);
    OptStmtList(en^.inlineBody);
    ignore := OptExpr(en^.inlineResult, en);
    (* mark result to indicate assignment at end of function *)
    MarkOptExpr(en^.inlineResult^.exprVal);
    return -optTime;
end OptExprInline;

procedure OptExprSet(en : ExprNode):OptTime;
var
    time : OptTime;
    esn   : ExprSetNode;
begin
    time := 0;
    if en^.setExpr # nil then
	esn := en^.setExpr^.first;
	while esn # nil do
	    time := Latest(time, OptExpr(esn^.lower,en));
	    if esn^.upper # nil then
		time := Latest(time, OptExpr(esn^.upper,en));
	    end;
	    esn := esn^.next;
	end;
    end;
    return time;
end OptExprSet;

procedure OptExprReorder(en : ExprNode) : OptTime;
(* Called to restructure expressions trees for operators that are both
   associative and commutative ("+" and "*").  Such expressions should
   be rearranged as far as possible to satisfy the property 
   left.defineTime <= right.defineTime. *)
	
var
    opnd1, opnd2, opnd11, opnd12, opnd21, opnd22 : ExprNode;
    opndon1, opndon2, opndon11, opndon12, opndon21, opndon22 : OptNode;
    isbin1, isbin2 : boolean;
    time, time1, time2, time11, time12, time21, time22 : OptTime;
begin
    if en^.optNoReorder then
	(* EXPRSAVE node below causes opnd2 to depend on opnd1 being first *)
	return Latest(OptExpr(en^.opnd1,en), OptExpr(en^.opnd2,en))
    end;

    opnd1 := en^.opnd1;
    isbin1 := (opnd1^.kind = EXPRBINOP) and (opnd1^.exprBinOp = en^.exprBinOp)
		and (not opnd1^.optNoReorder);
    opnd2 := en^.opnd2;
    isbin2 := (opnd2^.kind = EXPRBINOP) and (opnd2^.exprBinOp = en^.exprBinOp)
		and (not opnd2^.optNoReorder);
    if DEBUG and TraceOptim then
	Writef(output,'Reorder ');
	WriteExpr(en);
    end;
    if not isbin1 and not isbin2 then
	(* Cannot further rearrange the internals of the immediate 
	   subexpressions; swap left and right sides so that
	   left.defineTime <= right.defineTime. *)
	time1 := OptExpr(opnd1,en);
	time2 := OptExpr(opnd2,en);
	opndon1 := opnd1^.opt;
	opndon2 := opnd2^.opt;
	if DEBUG and TraceOptim then
	    Writef(output,' 1=%d 2=%d\n', opndon1^.rootCongruent^.defineTime,
		    opndon2^.rootCongruent^.defineTime);
	end;
	if opndon1^.rootCongruent^.defineTime >
		opndon2^.rootCongruent^.defineTime then
	    en^.opnd1 := opnd2;
	    en^.opnd2 := opnd1;
	end;
    elsif isbin1 and isbin2 then
	(* Both left and right subtrees look like us (e.g. (ll+lr)+(rl+rr))
	   so can swap operands between the two subexpressions. *)
	time1 := OptExprReorder(opnd1);
	time2 := OptExprReorder(opnd2);
(* ||| Should swap opnd1 and opnd2 if needed? *)
	opnd11 := opnd1^.opnd1;
	opndon11 := opnd11^.opt;
	opnd12 := opnd1^.opnd2;
	opndon12 := opnd12^.opt;
	opnd21 := opnd2^.opnd1;
	opndon21 := opnd21^.opt;
	opnd22 := opnd2^.opnd2;
	opndon22 := opnd22^.opt;
	if DEBUG and TraceOptim then
	    Writef(output,' 12=%d 21=%d\n', 
		    opndon12^.rootCongruent^.defineTime,
		    opndon21^.rootCongruent^.defineTime);
	end;
	(* opnd1.left.time <= opnd1.right.time *)
	(* opnd2.left.time <= opnd2.right.time *)
	(* Establish also that opnd1.right.time <= opnd2.left.time *)
	(* ||| But this doesn't insure that opndx.left <= opndx.right *)
	(* after the swap! Should additionally check 11 and 12 after swap *)
	if opndon12^.rootCongruent^.defineTime >
		opndon21^.rootCongruent^.defineTime then
	    opnd2^.opnd1 := opnd12;
	    opnd1^.opnd2 := opnd21;
	    opndon12^.parent := opnd2;
	    opndon21^.parent := opnd1;
	    time1 := Latest(opndon11^.rootCongruent^.defineTime,
				opndon21^.rootCongruent^.defineTime);
	    time2 := Latest(opndon12^.rootCongruent^.defineTime,
				opndon22^.rootCongruent^.defineTime);
	end;
	time1 := EnterExpr(opnd1,en,time1);
	time2 := EnterExpr(opnd2,en,time2);
    elsif isbin1 then
	(* Only left subexpression can be rearranged.  *)
	time1 := OptExprReorder(opnd1);
	time2 := OptExpr(opnd2,en);
	opnd11 := opnd1^.opnd1;
	opndon11 := opnd11^.opt;
	opnd12 := opnd1^.opnd2;
	opndon12 := opnd12^.opt;
	opndon2 := opnd2^.opt;
	if DEBUG and TraceOptim then
	    Writef(output,' 12=%d 2=%d\n', opndon12^.rootCongruent^.defineTime,
		    opndon2^.rootCongruent^.defineTime);
	end;
	(* ||| This does not guaranteed opnd1.left <= opnd1.right *)
	(* leave constants near the top right *)
	if (opnd2^.kind # EXPRCONST) and (opndon12^.rootCongruent^.defineTime >
		opndon2^.rootCongruent^.defineTime)
	then
	    opnd1^.opnd2 := opnd2;
	    opndon2^.parent := opnd1;
	    en^.opnd2 := opnd12;
	    opndon12^.parent := en;
	    time1 := Latest(opndon11^.rootCongruent^.defineTime,
				opndon2^.rootCongruent^.defineTime);
	    time2 := opndon12^.rootCongruent^.defineTime;
	end;
	time1 := EnterExpr(opnd1,en,time1);
    elsif isbin2 then
	(* Only right subexpression can be rearranged *)
	time1 := OptExpr(opnd1,en);
	time2 := OptExprReorder(opnd2);
	opnd21 := opnd2^.opnd1;
	opndon21 := opnd21^.opt;
	opnd22 := opnd2^.opnd2;
	opndon22 := opnd22^.opt;
	opndon1 := opnd1^.opt;
	if DEBUG and TraceOptim then
	    Writef(output,' 1=%d 21=%d\n', opndon1^.rootCongruent^.defineTime,
		    opndon21^.rootCongruent^.defineTime);
	end;
(* ||| Again, I don't think this quite suffices *)
	if opndon21^.rootCongruent^.defineTime >
		opndon1^.rootCongruent^.defineTime then
	    opnd2^.opnd1 := opnd1;
	    opndon1^.parent := opnd2;
	    en^.opnd1 := opnd21;
	    opndon21^.parent := en;
	    time1 := opndon21^.rootCongruent^.defineTime;
	    time2 := Latest(opndon1^.rootCongruent^.defineTime,
				opndon22^.rootCongruent^.defineTime);
	end;
	time2 := EnterExpr(opnd2,en,time2);
    end;
    return Latest(time1,time2);
end OptExprReorder;

procedure OptExprBinOp(en : ExprNode):OptTime;
var
    sol : SaveOptLevel;
    time1, time2 : OptTime;
begin
    if en^.exprBinOp in TokenSet{TKAND,TKOR,TKAMPERSAND} then
	(* conditional expression evaluation *)
	time1 := OptExpr(en^.opnd1,en);
	StartOptSplit(sol);
	time2 := OptExpr(en^.opnd2,en);
	EndOptSplit(sol);
	return Latest(time1,time2);
    else
	if not en^.optNoReorder and (exprDepth > 0) and 
		(en^.exprBinOp in TokenSet{TKPLUS,TKASTERISK}) then
	    return OptExprReorder(en);
	else
	    return Latest(OptExpr(en^.opnd1,en), OptExpr(en^.opnd2,en));
	end;
    end;
end OptExprBinOp;

procedure OptExprSave(const en : ExprNode) : OptTime;
    var time : OptTime;
begin
    time := OptExpr(en^.exprSave, en);
    MarkOptSave(en);  (* Mark everyone depending on exprSaveVar *)
    return -optTime;
end OptExprSave;

procedure OptExprCheck(const en : ExprNode) : OptTime;
    var time : OptTime;
begin
    time := 0;
    if en^.checkVar # NIL then
	time := OptVarNode(en^.checkVar);
    end;
    if en^.arrVar # NIL then
	time := Latest(time, OptVarNode(en^.arrVar));
    end;
    time := Latest(time, OptExpr(en^.checkExpr, en));
    return time;
end OptExprCheck;

procedure OptExprDescriptor(en : ExprNode) : OptTime;
var
    time : OptTime;
    den  : ExprNode;
begin
    time := OptExpr(en^.descripBase, en);
    den := en^.descrips^.first;
    while den # nil do
	time := Latest(time, OptExpr(den, en));
	den := den^.next;
    end;
    return time;
end OptExprDescriptor;
    
procedure OptFuncProc(procExpr : ExprNode; 
		      params   : ExprList; 
		      parent   : ExprNode) : OptTime;
var
    pexp    : ExprNode;
    pn      : ParamNode;
    proc    : ProcNode;
    procType: TypeNode;
    ignore  : OptTime;
begin
    procType := procExpr^.exprType;

    (* beware: type names can be used as both types and funcs *)
    if (procExpr^.kind = EXPRSYM) and
	    (procExpr^.exprSym^.kind = SYMTYPE) then
	return OptExpr(params^.first,parent);

    (* check for builtin function (must be a constant) *)
    elsif (procExpr^.kind = EXPRCONST) and
	    (procExpr^.exprConst^.kind = DTPROC) then
	proc := procExpr^.exprConst^.procVal;
	if proc^.builtin # BIPNOTBIP then
	    return OptBuiltin(parent,proc,params);
	end;
    end;

    (* normal user procedure *)
    if (params = nil) or (procType^.paramList = nil) then
	(* do nothing *)
    else
	(* first allow expressions to be referenced *)
	pexp := params^.first;
	while pexp # nil do
	    ignore := OptExpr(pexp, parent);
	    pexp := pexp^.next;
	end (* while *);

	(* then mark var parameters as modified *)
	pexp := params^.first;
	pn := procType^.paramList^.first;
	while pexp # nil do
	    if pn^.kind in ParamKindSet{PARAMVAR, PARAMARRAYVAR} then
		MarkOptExpr(pexp);
	    end;
	    pexp := pexp^.next;
	    pn := pn^.next;
	end;
    end;
    (* Mark everything that might be changed by procedure call *)
    MarkOptProcCall();
    return -optTime;
end OptFuncProc;

procedure OptExpr(en : ExprNode; parent : ExprNode) : OptTime;
var
    time : OptTime;
begin
    time := 0;
    if parent = nil then
	exprDepth := 0;
    else
	exprDepth := exprDepth + 1;
    end;
    if en = nil then
	Error('OptExpr: nil expression?');
    elsif en^.exprType = nil then
	ExprError(en,'OptExpr: exprType = nil?');
$if pascal then (* from write/read statments reusing expressions *)
    elsif en^.opt # nil then
	ExprError(en, 'OptExpr: opt # nil?');
$end (* pascal *)
    else
	case en^.kind of
	| EXPRBAD       : ExprError(en,'OptExpr: found EXPRBAD?')
	| EXPRNAME      :	
	| EXPRSYM       :	
	| EXPRCONST     :	
	| EXPRVAR       : time := OptExprVar(en);
	| EXPRUNOP      : time := OptExpr(en^.opnd, en);
	| EXPRBINOP     : time := OptExprBinOp(en);
	| EXPRFUNC      : time := Latest(OptExpr(en^.func, en),
				         OptFuncProc(en^.func, en^.params, en));
	| EXPRVAL       : time := OptExpr(en^.exprVal, en);
	| EXPRCHECK     : time := OptExprCheck(en);
	| EXPRSAVE      : time := OptExprSave(en);
	| EXPRSET       : time := OptExprSet(en);
	| EXPRDESCRIPTOR: time := OptExprDescriptor(en);
	| EXPRINLINE    : time := OptExprInline(en);
	end;
	time := EnterExpr(en, parent, time);
    end;
    return time;
end OptExpr;

procedure @inline OptExprS(const en : ExprNode);
var
    ignore : OptTime;
begin
    ignore := OptExpr(en,nil);
end OptExprS;

procedure OptStmtAssign(stn : StmtNode);
begin
    if stn^.assignSizeCheck # nil then
	OptExprS(stn^.assignSizeCheck);
    end;
    OptExprS(stn^.rhs);
    OptExprS(stn^.lhs);
    MarkOptExpr(stn^.lhs);
end OptStmtAssign;

procedure OptStmtProc(stn : StmtNode);
var
    ignore : OptTime;
begin
    OptExprS(stn^.proc);
    ignore := OptFuncProc(stn^.proc,stn^.params,nil);
end OptStmtProc;

procedure OptStmtIf(stn : StmtNode);
var
    sol : SaveOptLevel;
begin
    OptExprS(stn^.ifCond);
    StartOptSplit(sol);
    OptStmtList(stn^.thenList);
    NextOptSplit(sol);
    OptStmtList(stn^.elseList);
    EndOptSplit(sol);
end OptStmtIf;

procedure OptStmtCase(stn : StmtNode);
var
    caseNode : CaseNode;
    sol : SaveOptLevel;
begin
    OptExprS(stn^.caseSel);
    StartOptSplit(sol);
    if stn^.cases # nil then
	caseNode := stn^.cases^.first;
	while caseNode # nil do
	    OptStmtList(caseNode^.stmts);
	    caseNode := caseNode^.next;
	    NextOptSplit(sol);
	end;
    end;
    OptStmtList(stn^.caseElse);
    EndOptSplit(sol);
end OptStmtCase;

procedure OptStmtWhile(stn : StmtNode);
var
    sol : SaveOptLevel;
begin
    StartOptLoop(sol);
    OptExprS(stn^.whileCond);
    OptStmtList(stn^.whileBody);
    EndOptLoop(sol,stn^.whilePreEval,false);
end OptStmtWhile;

procedure OptStmtRepeat(stn : StmtNode);
var
    sol : SaveOptLevel;
begin
    StartOptLoop(sol);
    OptStmtList(stn^.repeatBody);
    OptExprS(stn^.repeatCond);
    EndOptLoop(sol,stn^.repeatPreEval,true);
end OptStmtRepeat;

procedure OptStmtLoop(stn : StmtNode);
var
    sol : SaveOptLevel;
begin
    StartOptLoop(sol);
    OptStmtList(stn^.loopBody);
    EndOptLoop(sol,stn^.loopPreEval,true);
end OptStmtLoop;

procedure OptStmtFor(stn : StmtNode);
var
    sym : Symbol;
    bt : TypeNode;
    error : boolean;
    ien : ExprNode;
    sol : SaveOptLevel;
    ion : OptNode;
begin
    OptExprS(stn^.forFrom);
    OptExprS(stn^.forTo);
    if stn^.forBy # nil then
	OptExprS(stn^.forBy);
    end;
    if stn^.forLimitCheck # nil then
	OptExprS(stn^.forLimitCheck);
    end;
    (* introduce expr node for loop index *)
    ien := NewExprNode(EXPRVAR);
    SameExprLine(ien,stn^.forFrom);
    ien^.exprType := stn^.forIndexVar^.varType;
    ien^.exprVar := stn^.forIndexVar;
    ien^.baseVar := stn^.forIndexVar;
    OptExprS(ien);
    (* mark it to indicate assignment at start of loop *)
    MarkOptExpr(ien);
    StartOptLoop(sol);
    OptStmtList(stn^.forBody);
    (* look for induction expressions *)
    if not OptNind and not currOptProc^.containsProcs then
	AnalyzeForLoop(ien,stn,sol);
    end;
    (* mark it to indicate increment at end of loop *)
    MarkOptExpr(ien);
    (* reduce counts on phony var expr *)
    ReduceExprNeededCounts(ien,1,false);
    EndOptLoop(sol,stn^.forPreEval,stn^.forWillExecute);
end OptStmtFor;

procedure OptStmtWith(stn : StmtNode);
var
    qen : ExprNode;
begin
    OptExprS(stn^.withQual);
    (* introduce expr node for implicit qualifier *)
    qen := NewExprNode(EXPRVAR);
    SameExprLine(qen,stn^.withQual);
    qen^.exprType := addressTypeNode;
    qen^.exprVar := stn^.withPtrVar;
    qen^.baseVar := stn^.withPtrVar;
    OptExprS(qen);
    (* mark it to indicate assignment at start of block *)
    MarkOptExpr(qen);
    (* reduce counts on phony var expr *)
    ReduceExprNeededCounts(qen,1,false);
    OptStmtList(stn^.withBody);
end OptStmtWith;

procedure OptStmtReturn(stn : StmtNode);
begin
    if stn^.returnVal # nil then
	OptExprS(stn^.returnVal);
	if stn^.inlineVarExpr # nil then
	    OptExprS(stn^.inlineVarExpr);
	else
	    OptRecursionReturn(currOptProc,stn);
	end;
    end;
end OptStmtReturn;

procedure OptStmtInline(stn : StmtNode);
begin
    OptInline(stn^.inlineParams, stn^.inlineFormals,
	stn^.inlineProc^.procType^.paramList, nil);
    OptStmtList(stn^.inlineBody);
end OptStmtInline;

$if pascal then
procedure OptStmtLabel(stn : StmtNode);
begin
    (* Cannot guarantee anything about where we've come from *)
    MarkOptAll();
end OptStmtLabel;
$end

procedure OptStmt(stn : StmtNode);
begin
    if stn^.bad then
	StmtError(stn,'OptStmt: stmt bad?');
    else
	case stn^.kind of
	| STMTNONE      :   (* nothing *);
	| STMTASSIGN    :   OptStmtAssign(stn);
	| STMTPROC      :   OptStmtProc(stn);
	| STMTIF	:   OptStmtIf(stn);
	| STMTWHILE     :   OptStmtWhile(stn);
	| STMTREPEAT    :   OptStmtRepeat(stn);
	| STMTLOOP      :   OptStmtLoop(stn);
	| STMTFOR       :   OptStmtFor(stn);
	| STMTWITH      :   OptStmtWith(stn);
	| STMTEXIT      :   (* nothing *);
	| STMTRETURN    :   OptStmtReturn(stn);
	| STMTCASE      :   OptStmtCase(stn);
	| STMTINLINE    :   OptStmtInline(stn);
	| STMTSTMTS     :   OptStmtList(stn^.stmts);
$if pascal then
	| STMTGOTO      :   (* nothing *);
	| STMTLABEL     :   OptStmtLabel(stn);
$end
	end;
    end;
end OptStmt;

procedure OptStmtList(stl : StmtList);
var
    stn : StmtNode;
begin
    if stl # nil then
	stn := stl^.first;
	while stn # nil do
	    OptStmt(stn);
	    stn := stn^.next;
	end;
    end;
end OptStmtList;

procedure OptProc(pn : ProcNode);
var
    code : CodeNode;
begin
    currOptProc := pn;
    if (pn^.code # nil) and not pn^.inlineProc then
	if DEBUG and TraceOptim then
	    Writef(output,'OptProc ');
	    WriteString(output,pn^.name);
	    Writec(output, '\n');
	end;
	StartOptProc();
	code := pn^.code^.first;
	if code # nil then
	    while code # nil do
		OptStmtList(code^.stmts);
		code := code^.next;
	    end;
	    OptRecursionProc(pn,pn^.code^.last^.stmts);
	end;
	EndOptProc();
	if DEBUG and ('o' in debugSet) then
	    Writef(output,'Optimized statements for procedure ');
	    WriteString(output,pn^.name);
	    Writec(output, '\n');
	    PrintStmtList(pn^.body,0);
	end;
	if DEBUG and (TraceOptim or TraceCount) then
	    DumpOptExprs();
	end;
	CountProc(pn);
	if DEBUG and ('q' in debugSet) then
	    Writef(output,'OptNodes after CountProc for procedure ');
	    WriteString(output,pn^.name);
	    Writec(output, '\n');
	    DumpOptExprs;
	end;
    end;
end OptProc;

procedure OptModule(mn : ModuleNode);
var
    submn : ModuleNode;
    pn : ProcNode;
begin
    if mn^.kind # MODDEFINITION then
	if DEBUG and TraceOptim then
	    Writef(output,'OptModule ');
	    WriteString(output,mn^.name);
	    Writec(output, '\n');
	end;
	submn := mn^.modules^.first;
	while submn # nil do
	    OptModule(submn);
	    submn := submn^.next;
	end;
	pn := mn^.procs^.first;
	while pn # nil do
	    OptProc(pn);
	    pn := pn^.next;
	end;
    (******** Experiment: code should be optimized by OptProc(globalProc);
	if mn^.body # nil then begin
	    StartOptProc;
	    OptStmtList(mn^.body);
	    EndOptProc;
	    if DEBUG and ('o' in debugSet) then begin
		Writef(output,'Optimized statements for module ');
		WriteString(output,mn^.name);
		Writec(output, '\n');
		PrintStmtList(mn^.body,0);
	    end;
	end;
    *********)
    end;
end OptModule;


procedure Optimize();
begin
    OptModule(globalModule);
end Optimize;

end OTree.
