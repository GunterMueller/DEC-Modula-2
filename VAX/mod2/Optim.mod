implementation module Optim;

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
    Writef, Writec, output;

from MemLib import
    ALLOCATE;

from Machine import
    WORDSIZE, UNITSIZE, HugeInteger;

from Tokens import
    Token, TokenSet;

from Globals import
    TargetMachine, target, 
    TraceOpt, TraceOptim, TraceCount, TraceActions, TraceMark, DEBUG,
    OptNloop, OptNtail;

from Strings import
    WriteString;

from Symbols import
    OptTime, CheckKind, CheckKindSet, MEMFAST, GSNORMAL,
    ConstNode, DataType, TypeNode, MemoryType, MemoryTypeSet, VarNode,
    ParamKind, ParamKindSet, ParamNode, ProcNode, CaseNode, 
    ExprKind, ExprKindSet, ExprNode, ExprList, StmtKind, StmtNode, StmtList, 
    ConstSetNode, ExprSetNode, ExprSetList, ArrayKind, 
    addressTypeNode, OptCost, OptUsage, OptCost;
    
from Consts import
    OrdOf, CardinalConst;

from TypeInfo import
    SizeOf;
    
from BuildExpr import
    NewExprNode, SameExprLine, AddToExprList, AddToExprSetList, WriteExpr;

from InitBuiltin import
    pureFunctions;

from Errors import
    Error, ExprError;

from Decls import
    DefineVarInProc;

from OCount import
    ReduceExprNeededCounts, ReduceNeededCounts;
    
from OTree import
    Latest;

var
    generateUniqueId : integer;
    blockOptTime : integer;
    cseRootUnOp  : array Token of OptNode;
    cseRootBinOp : array Token of OptNode;

    optBlockLevel, optBlockCeiling, optBlockFloor : OptBlockLevel;
    activeExprs : OptNode;	(* dummy root for all non-purged expressions *)
    markItCount : integer;

(* Set up new one expecting no matches *)
procedure NewOptNode(en, parent : ExprNode) : OptNode;
var
    newOne : OptNode;
begin
    new(newOne);
    newOne^.uniqueId := generateUniqueId;
    en^.opt := newOne;		(* "backward pointer" *)
    generateUniqueId := generateUniqueId + 1;
    newOne^.nextClass := newOne;
    newOne^.prevClass := newOne;
    newOne^.nextCongruent := newOne;
    newOne^.prevCongruent := newOne;
    newOne^.rootCongruent := newOne;
    newOne^.nextEqual := newOne;
    newOne^.prevEqual := newOne;
    newOne^.rootEqual := newOne;
    newOne^.createLevel := optBlockLevel;
    newOne^.markLevel := 0;
    newOne^.joinMarkLevel := 0;
    newOne^.marked := false;
    newOne^.joinMark := false;
    newOne^.purged := false;
    newOne^.removed := false;
    newOne^.expr := en;
    newOne^.parent := parent;
    newOne^.tempNumber := NULLTEMP;
    newOne^.usage := OUSEINDIVIDUAL;
    newOne^.cost := COSTNONE;
    newOne^.eligible := false;
    newOne^.ineligible := false;
    newOne^.containedNonTrivial := nil;
    newOne^.tailRecursion := false;
    newOne^.loopConsidered := false;
    newOne^.neededCount := 1;
    if DEBUG and TraceCount then
	Writef(output,'New: %d 1 : ',newOne^.uniqueId);
	WriteExpr(en);
	Writec(output, '\n');
    end;
    newOne^.referencedCount := 1;
    newOne^.loopNest := optLoopNest;
    newOne^.usedCount := 0;
    newOne^.defineTime := 0;
    AddToAllList(allExprs^.prevAll,newOne);
    newOne^.nextActive := nil;
    newOne^.prevActive := nil;
    newOne^.inductionVar := nil;
    return newOne;
end NewOptNode;


procedure ResetOptimizer();
var
    ek      : ExprKind;
    token   : Token;
    i       : integer;
begin
    for ek := EXPRNAME to EXPRBAD do
	cseRootExpr[ek] := nil;
    end;
    for token := first(Token) to last(Token) do
	cseRootUnOp[token] := nil;
	cseRootBinOp[token] := nil;
    end;
    new(activeExprs);
    activeExprs^.nextActive := activeExprs;
    activeExprs^.prevActive := activeExprs;
    new(allExprs);
    allExprs^.nextAll := allExprs;
    allExprs^.prevAll := allExprs;
    optTime := 1;
    markAllOptTime := 0;
end ResetOptimizer;

procedure OptJoin();
var
    ron, on : OptNode;
begin
    if DEBUG and TraceOpt then
	Writef(output,'Join %d\n',optBlockLevel);
    end;
    on := activeExprs^.nextActive;
    markItCount := 0;
    while on # activeExprs do
	(* move out expressions that were marked below this level *)
	(* join is done after going back up *)
	if (on^.markLevel > optBlockLevel) or 
		(on^.joinMarkLevel > optBlockLevel) then
	    on^.marked := true;
	    on^.markLevel := optBlockLevel;
	    on^.joinMark := false;
	    on^.joinMarkLevel := 0;
	    if DEBUG and TraceMark then
		Writef(output,'<+%d>',on^.uniqueId);
		markItCount := markItCount + 1;
		if markItCount > 20 then
		    Writec(output, '\n');
		    markItCount := 0;
		end;
	    end;
	end;
	on := on^.nextActive;
    end;
end OptJoin;

procedure OptRefresh();
var
    on, nextActive : OptNode;
begin
    if DEBUG and TraceOpt then
	Writef(output,'Refresh %d\n',optBlockLevel);
    end;
    markItCount := 0;
    on := activeExprs^.nextActive;
    while on # activeExprs do
	nextActive := on^.nextActive;
	(* refresh is done at level of split *)
	(* purge expressions created at or below this level *)
	if on^.createLevel >= optBlockCeiling then
	    (* purge expression *)
	    on^.purged := true;
	    RemoveFromActiveList(on);
	    if DEBUG and TraceMark then
		Writef(output,'<X%d>',on^.uniqueId);
		markItCount := markItCount + 1;
		if markItCount > 20 then
		    Writec(output, '\n');
		    markItCount := 0;
		end;
	    end;
	elsif on^.markLevel >= optBlockCeiling then
	    (* remove marks of expressions marked at this level or below *)
	    on^.joinMark := on^.joinMark or on^.marked;
	    on^.joinMarkLevel := optBlockLevel;
	    on^.marked := false;
	    on^.markLevel := 0;
	    if DEBUG and TraceMark then
		Writef(output,'<-%d>',on^.uniqueId);
		markItCount := markItCount + 1;
		if markItCount > 20 then
		    Writec(output, '\n');
		    markItCount := 0;
		end;
	    end;
	end;
	on := nextActive;
    end;
end OptRefresh;

(* CopyExpr: duplicate the expression tree (including associated OptNodes)*)
(*  Make the expressions be at the specified level *)
procedure CopyExpr(en : ExprNode; sol : SaveOptLevel; parent : ExprNode)
	: ExprNode;
var
    on, eon, non, ron, search : OptNode;
    nen, pen, npen  : ExprNode;
    nesl      : ExprSetList;
    esn, nesn : ExprSetNode;
    found     : boolean;

    procedure CopyExprList(enl    : ExprList;
			   sol    : SaveOptLevel;
			   parent : ExprNode) : ExprList;
    var
	npl : ExprList;
	en  : ExprNode;
    begin
	npl := AddToExprList(nil,nil);
	en := enl^.first;
	while en # nil do
	    npl := AddToExprList(npl, CopyExpr(en, sol, parent));
	    en := en^.next;
	end;
	return npl;
    end CopyExprList;

begin (* CopyExpr *)
    nen := NewExprNode(en^.kind);
    nen^ := en^;	(* copy miscellaneous fields *)
    case en^.kind of    (* copy pointer fields       *)
    | EXPRCONST, EXPRVAR, EXPRSYM :
	(* Leaves; no need for further copying *)
    | EXPRNAME :
	ExprError(en,'CopyExpr: unexpected expr')
    | EXPRUNOP :
	nen^.opnd := CopyExpr(en^.opnd,sol,nen);
    
    | EXPRBINOP :
	nen^.opnd1 := CopyExpr(en^.opnd1,sol,nen);
	nen^.opnd2 := CopyExpr(en^.opnd2,sol,nen);
    
    | EXPRFUNC :
	nen^.func := CopyExpr(en^.func,sol,nen);
	nen^.params := CopyExprList(en^.params, sol, nen);

    | EXPRVAL :
	nen^.exprVal := CopyExpr(en^.exprVal,sol,nen);
    
    | EXPRCHECK :
	nen^.checkExpr := CopyExpr(en^.checkExpr,sol,nen);
    
    | EXPRDESCRIPTOR :
	nen^.descripBase := CopyExpr(en^.descripBase, sol, nen);
	nen^.descrips := CopyExprList(en^.descrips, sol, nen);

    | EXPRSET :
	nesl := nil;
	if en^.setExpr # nil then
	    esn := en^.setExpr^.first;
	    while esn # nil do
		new(nesn);
		nesn^.lower := CopyExpr(esn^.lower,sol,nen);
		if esn^.upper = nil then
		    nesn^.upper := nil;
		else
		    nesn^.upper := CopyExpr(esn^.upper,sol,nen);
		end;
		nesl := AddToExprSetList(nesl,nesn);
		esn := esn^.next;
	    end;
	end;
	nen^.setExpr := nesl;
    
    | EXPRSAVE :
	nen^.exprSave := CopyExpr(en^.exprSave,sol,nen);
    
    | EXPRINLINE :
	ExprError(en,'CopyExpr: INLINE expr?');
	
    end;
    on := en^.opt;
    non := NewOptNode(nen,nil);
    (* make it look as if it were created outside the loop *)
    ron := on^.rootEqual;
    if ron^.createLevel > sol.level then
        non^.createLevel := sol.level;
    else
        non^.createLevel := ron^.createLevel;
    end;
    non^.cost := on^.cost;
    non^.containedNonTrivial := on^.containedNonTrivial;
    non^.parent := parent;
    non^.eligible := ron^.eligible;
    non^.ineligible := ron^.ineligible;
    if DEBUG and TraceOptim then
	Writef(output,'CopyExpr: id=%d/%d:%d create=%d level=%d\n',
		on^.uniqueId, ron^.uniqueId, non^.uniqueId, ron^.createLevel,
		sol.level);
    end;
    (* search for an equal expression in the enclosing block *)
    search := ron^.rootCongruent;
    found := false;
    loop
	if (search^.rootEqual # search) or search^.marked or search^.purged
		or (search^.createLevel > sol.ceiling)
		or (search^.createLevel < sol.floor)
	then
	    (* not equal *)
	    search := search^.nextCongruent;
	    if search = ron^.rootCongruent then
		exit;
	    end;
	else
	    (* equal *)
	    found := true;
	    exit;
	end;
    end (* loop *);
    if found then
	(* expression is already in outer block, make this another reference *)
	AddToEqualList(search,non);
	if search # ron then
	    (* Consolidate this equal list with the outer one *)
	    if DEBUG and TraceOptim then
		Writef(output,'CopyExpr: adding to list\n');
		(*
		PrintOptExpr(search);
		PrintOptExpr(ron);
		PrintOptExpr(non);
		*)
	    end;
	    (* Add in counts from inner block expression *)
	    if DEBUG and TraceCount then
		Writef(output,'CopyExpr: %d/%d %d+%d found match on move\n',
			search^.uniqueId, search^.rootEqual^.uniqueId,
			search^.neededCount, ron^.neededCount);
	    end;
	    search^.neededCount := search^.neededCount + ron^.neededCount;
	    search^.referencedCount := search^.referencedCount
					    + ron^.referencedCount;
	    search^.eligible := search^.eligible or ron^.eligible;
	    search^.ineligible := search^.ineligible or ron^.ineligible;
	    (* append the inner equal list to the outer one *)
	    (* fix up root pointer *)
	    eon := ron;
	    repeat
		eon^.rootEqual := search;
		eon := eon^.nextEqual;
	    until eon = ron;
	    (* append lists *)
	    search^.prevEqual^.nextEqual := ron;
	    ron^.prevEqual^.nextEqual := search;
	    eon := search^.prevEqual;
	    search^.prevEqual := ron^.prevEqual;
	    ron^.prevEqual := eon;
	end;
    else
	if DEBUG and TraceCount then
	    Writef(output,'CopyExpr: %d/%d %d+1 : copied out %d\n',
		    non^.uniqueId, non^.rootEqual^.uniqueId,
		    ron^.neededCount, ron^.uniqueId);
	end;
	(* expression is needed once by pre-evaluation *)
	non^.neededCount := ron^.neededCount + 1;
	(* consider pre-evaluation another reference *)
	non^.referencedCount := ron^.referencedCount + 1;
	(* make this be the root equal expression *)
	eon := ron;
	repeat
	    eon^.rootEqual := non;
	    eon := eon^.nextEqual;
	until eon = ron;
	(* insert new OptNode in front of rootEqual *)
	non^.prevEqual := ron^.prevEqual;
	non^.nextEqual := ron;
	ron^.prevEqual^.nextEqual := non;
	ron^.prevEqual := non;
	AddToCongruentList(ron^.rootCongruent^.prevCongruent,non);
    end;
    return nen;
end CopyExpr;

procedure InductionExpr(indexen, patternen : ExprNode; var opnden : ExprNode;
    sol : SaveOptLevel; opnd1 : boolean);
var
    newen, otheren : ExprNode;
    newon, opndon, otheron : OptNode;
    time     : OptTime;
    constant : boolean;
    constVal : HugeInteger;
    intVal   : integer;
begin
    time := 0;
    opndon := opnden^.opt;
    if patternen^.kind = EXPRCHECK then
	constant := false;
	if (opnden^.kind = EXPRCONST) and
	    (patternen^.exprCheck in CheckKindSet{CHECKSUBSCR,CHECKRANGE}) then
	    (* This end of FOR range is constant.  If constant value is OK, 
	       eliminate test.  If not OK, must check at runtime, as this
	       code may not actually be reached, as in the case
	       start := FIRST(iType);
	       FOR i := start to FIRST(iType)-1 DO ... *)
	    intVal := trunc(OrdOf(opnden^.exprConst));
	    if patternen^.checkLower <= patternen^.checkUpper then
		if (intVal >= patternen^.checkLower) and
			(intVal <= patternen^.checkUpper) then
		    constant := true;
		end;
	    else
		if (unsigned(intVal) >= unsigned(patternen^.checkLower)) and
		     (unsigned(intVal) <= unsigned(patternen^.checkUpper)) then
		    constant := true;
		end;
	    end;
	end;
	if not constant then
	    (* put check expression over limit *)
	    newen := NewExprNode(EXPRCHECK);
	    newen^ := patternen^;
	    newen^.checkExpr := opnden;
	    opndon^.parent := newen;
	    (* install new expression at outer level *)
	    time := EnterExprAtLevel(newen, nil, time, sol);
	    opnden := newen;
	end;
    else
	constant := false;
	if opnd1 and (patternen^.opnd2^.kind = EXPRCONST)
		and (opnden^.kind = EXPRCONST) then
	    constant := true;
	    constVal := OrdOf(patternen^.opnd2^.exprConst);
	elsif (patternen^.opnd1^.kind = EXPRCONST) and 
		(opnden^.kind = EXPRCONST) then
	    constant := true;
	    constVal := OrdOf(patternen^.opnd1^.exprConst);
	end;
	if constant then
	    case patternen^.exprBinOp of
	    | TKPLUS     : constVal := constVal + OrdOf(opnden^.exprConst)
	    | TKMINUS    : constVal := -constVal + OrdOf(opnden^.exprConst)
	    | TKASTERISK : constVal := constVal * OrdOf(opnden^.exprConst)
	    end;
	    (* decommision old expression *)
	    ReduceExprNeededCounts(opnden,1,true);
	    RemoveFromActiveList(opndon);
	    (* make new expression node *)
	    newen := NewExprNode(EXPRCONST);
	    SameExprLine(newen, opnden);
	    newen^.kind := EXPRCONST;
	    newen^.exprConst := CardinalConst(constVal);
	    newen^.exprType := patternen^.exprType;
	    newen^.constType := patternen^.exprType;
	    (* enter expression outside loop *)
	    time := EnterExprAtLevel(newen,nil,time,sol);
	else
	    (* non-constant: put induction expression above expression *)
	    newen := NewExprNode(EXPRBINOP);
	    newen^ := patternen^;
	    if opnd1 then
		newen^.opnd2 := CopyExpr(patternen^.opnd2,sol,newen);
		otheren := newen^.opnd2;
		newen^.opnd1 := opnden;
	    else
		newen^.opnd1 := CopyExpr(patternen^.opnd1,sol,newen);
		otheren := newen^.opnd1;
		newen^.opnd2 := opnden;
	    end;
	    otheron := otheren^.opt;
	    opndon^.parent := newen;
	    time := Latest(opndon^.rootCongruent^.defineTime,
			   otheron^.rootCongruent^.defineTime);
	    if opndon^.marked then
		time := -time;
	    end;
	    (* enter expression outside loop *)
	    time := EnterExprAtLevel(newen,nil,time,sol);
	end;
	opnden := newen;
    end;
end InductionExpr;

procedure AnalyzeForLoop(ien : ExprNode; stn : StmtNode; sol : SaveOptLevel);
var
    ion, on, pon, patternon, parenton, vion, oon : OptNode;
    pen, en : ExprNode;
    possible, continue : boolean;
    opnd1 : boolean;
    time : OptTime;
    itercount : integer;
begin
    if DEBUG and TraceOptim then
	Writef(output,'AnalyzeForLoop\n');
    end;
    itercount := 0;
    (* index opt node *)
    ion := ien^.opt;
    (* To do induction variable elimination, must find all references to *)
    (* the loop index (except ien/ion) the same expression.  First look  *)
    (* for EXPRVAL of index						 *)
    possible := false;
    if SizeOf(stn^.forIndexType) # WORDSIZE then
	(* may change type, must be a word *)
    elsif ion^.nextCongruent # ion then		    (* find another refrence *)
	if ion^.nextCongruent^.parent # nil then    (* must have a parent *)
	    if ion^.nextCongruent^.parent^.kind = EXPRVAL then
		(* found EXPRVAL of ien *)
		vion := ion^.nextCongruent^.parent^.opt;
		possible := true;
		patternon := vion;
	    end;
	end;
    end;
    (* The following loop repeatedly searches through the expressions that *)
    (* use the index.  It starts with patternon pointing to the val.  Each *)
    (* time it looks to see if all uses of patternon are the same and meet *)
    (* the criteria for linear induction. If it gets through the expressions *)
    (* successfully, it fixes up the loop limits and increment, then moves *)
    (* patternon to the parent node and repeats. *)
    continue := true;
    while possible and continue do
	if DEBUG and TraceOptim then
	    if itercount > 0 then
		Writef(output,'**** One More Time\n');
	    end;
	end;
	itercount := itercount + 1;
	on := patternon;
	parenton := nil;
	repeat
	    if on^.uniqueId < ion^.uniqueId then
		if DEBUG and TraceOptim then
		    Writef(output,'before loop\n');
		    PrintOptExpr(on);
		end;
	    elsif on^.removed then
		Error('AnalyzeForLoop: removed?');
		possible := false;
		if DEBUG and TraceOptim then
		    Writef(output,'removed\n');
		    PrintOptExpr(on);
		end;
	    elsif on^.parent = nil then
		continue := false;
		if DEBUG and TraceOptim then
		    Writef(output,'nil parent\n');
		    PrintOptExpr(on);
		end;
	    else
		(* get parent expression *)
		pon := on^.parent^.opt;
		if parenton # nil then
		    (* already have a candidate *)
		    if parenton^.rootCongruent # pon^.rootCongruent then
			(* two different uses *)
			continue := false;
		    end;
		(* check out candidate expression *)
		elsif pon^.ineligible then
		    (* expression ineligible for optimization *)
		    continue := false;
		elsif (pon^.expr^.kind = EXPRCHECK) and stn^.forWillExecute then
		    (* allow some check expressions *)
		    if pon^.expr^.exprCheck in
			CheckKindSet{CHECKRANGE, CHECKSUBSCR, CHECKSUBSCROPEN}
		    then
			(* a winner! *)
			parenton := pon;
		    else
			continue := false;
		    end;
		elsif pon^.expr^.kind # EXPRBINOP then
		    (* other candidates must be bin op *)
		    continue := false;
		elsif not (pon^.expr^.exprBinOp in
			    TokenSet{TKPLUS, TKASTERISK, TKMINUS})
		then
		    (* only linear arithmetic operations are allowed *)
		    continue := false;
		elsif (pon^.expr^.exprBinOp = TKASTERISK) and 
			(pon^.expr^.opnd1^.kind # EXPRCONST) and
			(pon^.expr^.opnd2^.kind # EXPRCONST) then
		    continue := false;
		(* ||| Var multiplication used to be here, and should be added
		   again someday.  BUT, it generated bogus loop test code,
		   and can only be reenabled when induction variables are
		   kept mostly as separate entities, not as a replacement
		   for the original index variables. *)
		else
		    (* opnd1 is true if index is opnd1 of binop *)
		    opnd1 := pon^.expr^.opnd1 = on^.expr;
		    if opnd1 then
			oon := pon^.expr^.opnd2^.opt;
		    else
			oon := pon^.expr^.opnd1^.opt;
		    end;
		    (* Other opnd must be constant over the loop body, i.e. *)
		    (* it cannot be marked, and is either calculated before *)
		    (* loop or a constant *)
		    continue := not oon^.marked and
			((oon^.rootCongruent^.defineTime <= blockOptTime) or
			 (oon^.expr^.kind in ExprKindSet{EXPRVAR,EXPRCONST}))
			and ((pon^.expr^.exprBinOp # TKMINUS) or opnd1);
		    if continue then
			(* a winner! *)
			parenton := pon;
		    end;
		end;
		if DEBUG and TraceOptim then
		    if continue then
			Writef(output,'potential %d:%d\n',
				pon^.rootCongruent^.defineTime,	blockOptTime);
		    else
			Writef(output,'ruining %d:%d\n',
				pon^.rootCongruent^.defineTime,	blockOptTime);
		    end;
		    PrintOptExpr(pon);
		end;
	    end;
	    on := on^.nextCongruent;
	until not possible or not continue or (on = patternon);

	(* if continue and parenton # nil, found an eligible parent expr *)
	if possible and continue and (parenton # nil) then
	    if DEBUG and (TraceOptim or TraceCount) then
		Writef(output,'AnalyzeForLoop: found induction expression\n');
		PrintOptExpr(parenton);
	    end;
	    (* Found a good induction expression. Apply induction expression *)
	    (* to loop limits and, if multiply, increment.  Then continue    *)
	    (* and look for an enclosing induction expression. *)
	    pen := parenton^.expr;
	    (* make index operation use new expression type *)
	    stn^.forIndexType := pen^.exprType;
	    InductionExpr(ien,pen,stn^.forFrom,sol,opnd1);
	    InductionExpr(ien,pen,stn^.forTo,sol,opnd1);
	    if (stn^.forTo^.kind # EXPRCONST) and (stn^.forLimitVar = nil) then
		currOptProc^.mem^.current := currOptProc^.mem^.maximum;
		stn^.forLimitVar := DefineVarInProc(nil, stn^.forIndexType,
		    MEMFAST, GSNORMAL, nil, currOptProc);
	    end;
	    if (pen^.kind = EXPRBINOP) and (pen^.exprBinOp = TKASTERISK) then
		InductionExpr(ien,pen,stn^.forBy,sol,opnd1);
		if (stn^.forBy^.kind # EXPRCONST)
			and (stn^.forIncVar = nil) then
		    currOptProc^.mem^.current := currOptProc^.mem^.maximum;
		    stn^.forIncVar := DefineVarInProc(nil, stn^.forIndexType,
			MEMFAST, GSNORMAL, nil, currOptProc);
		end;
	    end;
	    if DEBUG and TraceOptim then
		Writef(output,'from ');
		WriteExpr(stn^.forFrom);
		Writef(output,' to ');
		WriteExpr(stn^.forTo);
		Writef(output,' by ');
		WriteExpr(stn^.forBy);
		Writec(output, '\n');
	    end;
	    (* Continue looking for an enclosing induction expression *)  
	    patternon := parenton;
	else
	    (* if parenton=nil, continue, and possible, we didn't find *)
	    (* any uses of the index.  Give up. *)
	    continue := false;
	end;
    end;
    (* if patternon has been moved up, then we found an induction expression *)
    if possible and (patternon # vion) then
	(* Done all we can.  Fix up expression, reducing the count on the *)
	(* induction expr and marking the OptNodes with the index variable. *)
	on := patternon;
	repeat
	    if on^.uniqueId < ion^.uniqueId then
		(* ignore old expressions *)
	    elsif on # on^.rootEqual then
		(* only look at rootEqual expressions *)
	    else
		(* We've eliminated all uses of this expr.  Decrement to 0. *)
		ReduceNeededCounts(on,on^.referencedCount,false);
		(* Set node to use loop index *)
		on^.inductionVar := ien^.exprVar;
		on^.eligible := false;
		on^.ineligible := true;
		on^.cost := COSTNONE;
		if DEBUG and (TraceCount or TraceOptim) then
		    Writef(output,'Update induction expression\n');
		    PrintOptExpr(on);
		end;
	    end;
	    on := on^.nextCongruent;
	until on = patternon;
    end;
    if DEBUG and TraceCount then
	Writef(output,'AnalyzeForLoop: done\n');
    end;
end AnalyzeForLoop;

procedure ConsiderMove(    on      : OptNode; 
		       var preEval : ExprList; 
			   sol     : SaveOptLevel) : boolean;
var
    pon, non : OptNode;
    moveParent, moveMe : boolean;
    en, pen, ien : ExprNode;

    procedure OKToMove(on : OptNode) : boolean;
    begin
	(* candidates for moving out are not marked, non-trivial, *)
	(*  depend only on things outside the loop, *)
	(*  and were calculated in this block *)
	if on^.marked  then
	    (* marked *)
	elsif on^.cost = COSTNONE then
	    (* not worth moving *)
	elsif (on^.rootCongruent^.defineTime >= blockOptTime) and
		    not (on^.expr^.kind in ExprKindSet{EXPRVAR,EXPRCONST})
	then
	    (* not known before loop *)
	elsif on^.createLevel < optBlockCeiling then
	    (* not accessible from loop *)
	elsif (SizeOf(on^.expr^.exprType) > WORDSIZE)
(* |||		and (on^.expr^.exprType^.kind # DTLONGREAL) *) then
	    (* too large *)
	elsif on^.expr^.kind = EXPRSAVE then
	    (* can't move because of variable allocation *)
	elsif on^.expr^.kind = EXPRFUNC then
	    if on^.expr^.func^.kind = EXPRCONST then
		(* builtin OK if pure function *)
		return on^.expr^.func^.exprConst^.procVal^.builtin in
		    pureFunctions;
	    end;
	else
	    return true;
	end;
	return false;
    end OKToMove;

begin (* ConsiderMove *)
    moveParent := false;
    moveMe := false;
    on := on^.rootEqual;
    if OptNloop or on^.loopConsidered then
	(* do nothing *)
    elsif OKToMove(on) then
	(* check for parent being moved out of loop first *)
	(* move this expression anyway if used more than parent *)
	pen := on^.parent;
	while not moveMe and (pen # nil) do
	    pon := pen^.opt;
	    pon := pon^.rootEqual;
	    if OKToMove(pon) then
		moveParent := true;
		if pon^.referencedCount < on^.referencedCount then
		    moveMe := true;
		else
		    pen := pon^.parent;
		end;
	    else
		pen := nil;
	    end;
	end;
	(* if no parent to move or need this one anyhow, move it *)
	if moveMe or not moveParent then
	    if DEBUG and (TraceActions or TraceCount) then
		Writef(output,'Move out of loop %d\n', blockOptTime);
		PrintOptExpr(on);
	    end;
	    en := on^.expr;
	    ien := CopyExpr(en,sol,nil);
	    (* expression should be preserved through whole block *)
	    if DEBUG and TraceCount then
		Writef(output,'ConsiderMove: %d/%d %d+1 : post\n',
			on^.uniqueId, on^.rootEqual^.uniqueId, 
			on^.rootEqual^.neededCount);
	    end;
	    non := ien^.opt;
	    if non^.rootEqual # on^.rootEqual then
		ExprError(en,'ConsiderMove: copy not the same as original?');
	    end;
	    on^.rootEqual^.neededCount := on^.rootEqual^.neededCount + 1;
	    on^.rootEqual^.eligible := true;
	    preEval := AddToExprList(preEval,ien);
	end;
	if moveParent then
	    (* now move parent *)
	    moveParent := ConsiderMove(pon^.rootEqual,preEval,sol);
	end;
    end;
    on^.loopConsidered := true;
    return moveMe or moveParent;
end ConsiderMove;

procedure OptFinishLoop(var preEval : ExprList; 
			    invariantSol, loopSol : SaveOptLevel);
var
    on, nextActive : OptNode;
begin
    preEval := AddToExprList(nil,nil);
    on := activeExprs^.nextActive;
    (* traverse list *)
    while on # activeExprs do
	nextActive := on^.nextActive;
	if on^.rootEqual # on then
	    (* not a rootEqual expression *)
	    if DEBUG and TraceOpt then
		Writef(output,'OptFinishLoop: active but not rootEqual %d\n',
			on^.uniqueId);
	    end;
	elsif ConsiderMove(on,preEval,invariantSol) then
	    (* moved it out of loop *)
	end;
	on := nextActive;
    end;
    on := activeExprs^.nextActive;
    while on # activeExprs do
	nextActive := on^.nextActive;
	if on^.createLevel >= loopSol.ceiling then
	    (* purge expression that's no longer valid *)
	    on^.purged := true;
	    RemoveFromActiveList(on);
	    if DEBUG and TraceMark then
		Writef(output,'<X%d>', on^.uniqueId);
		markItCount := markItCount + 1;
		if markItCount > 20 then
		    Writec(output, '\n');
		    markItCount := 0;
		end;
	    end;
	end;
	on := nextActive;
    end;
end OptFinishLoop;

procedure StartOptProc();
begin
    ResetOptimizer();
    optBlockLevel := 1;
    optBlockCeiling := 1;
    optBlockFloor := 1;
end StartOptProc;

procedure EndOptProc();
begin
end EndOptProc;

procedure StartOptSplit(var sol : SaveOptLevel);
begin
    sol.level := optBlockLevel;
    sol.floor := optBlockFloor;
    sol.ceiling := optBlockCeiling;
    sol.blockTime := blockOptTime;
    blockOptTime := optTime;
    optBlockLevel := optBlockLevel + 1;
    optBlockCeiling := optBlockLevel;
end StartOptSplit;

procedure NextOptSplit(var sol : SaveOptLevel);
begin
    OptRefresh();
end NextOptSplit;

procedure EndOptSplit(var sol : SaveOptLevel);
begin
    OptRefresh();
    optBlockLevel := sol.level;
    optBlockFloor := sol.floor;
    optBlockCeiling := sol.ceiling;
    blockOptTime := sol.blockTime;
    OptJoin();
end EndOptSplit;

procedure StartOptLoop(var sol : SaveOptLevel);
begin
    (* increase level by 2, 1 for invariant level, 1 for loop *)
    sol.level := optBlockLevel;
    sol.floor := optBlockFloor;
    sol.ceiling := optBlockCeiling;
    sol.blockTime := blockOptTime;
    blockOptTime := optTime;
    optBlockLevel := optBlockLevel + 1;
    optBlockFloor := optBlockLevel;
    optBlockLevel := optBlockLevel + 1;
    optBlockCeiling := optBlockLevel;
    optLoopNest := optLoopNest * LOOPBIAS;
end StartOptLoop;

procedure EndOptLoop(var sol : SaveOptLevel; var preEval : ExprList;
	alwaysExecuted : boolean);
var
    loopSol : SaveOptLevel;
begin
    optLoopNest := optLoopNest div LOOPBIAS;
    loopSol.level := optBlockLevel - 1;
    loopSol.ceiling := optBlockLevel - 1;
    loopSol.floor := optBlockLevel - 1;
    loopSol.blockTime := blockOptTime;
    if alwaysExecuted then
	(* if always executed, move invariants all the way out *)
	OptFinishLoop(preEval,sol,loopSol);
    else
	(* if not always executed, move invariants to invariant block *)
	OptFinishLoop(preEval,loopSol,loopSol);
    end;
    optBlockLevel := sol.level;
    optBlockFloor := sol.floor;
    optBlockCeiling := sol.ceiling;
    blockOptTime := sol.blockTime;
    OptJoin;
end EndOptLoop;

procedure OptRecursionFuncProc(proc     : ProcNode;
			       procExpr : ExprNode;
			       params   : ExprList);
var
    ok  : boolean;
    pen : ExprNode;
    pn  : ParamNode;
begin
    if (procExpr^.kind = EXPRCONST) and (procExpr^.exprConst^.kind = DTPROC) and
	    (proc = procExpr^.exprConst^.procVal) then
	if DEBUG and TraceOptim then
	    Writef(output,'OptRecursionFuncProc ');
	    WriteString(output, proc^.name);
	    Writec(output, '\n');
	end;
	ok := true;
	if proc^.procType^.paramList # nil then
	    pen := params^.first;
	    pn := proc^.procType^.paramList^.first;
	    while ok and (pn # nil) do
		if DEBUG and TraceOptim then
		    Writef(output, '%n ', pn^.kind);
		    WriteExpr(pen);
		end;
		if pn^.kind in ParamKindSet{PARAMARRAYVALUE,
				    PARAMARRAYVAR,PARAMARRAYCONST} then
		    ok := false;
		elsif pn^.reference then
		    if pen^.baseVar = nil then
			if DEBUG and TraceOptim then
			    Writef(output,' baseVar=nil');
			end;
		    elsif pen^.baseVar^.address.kind in
			    MemoryTypeSet{MEMNORMAL,MEMFAST} then
			(* Don't allow reference to local space...we'll be
			   pointing at local data which may be changing,
			   rather than pointing to data in an older stack
			   frame. *)
			ok := proc # pen^.baseVar^.address.proc;
			if DEBUG and TraceOptim then
			    Writef(output, ' block=%d',ord(ok));
			end;
		    elsif pen^.baseVar^.address.kind = MEMPARAM then
			(* See comment above. *)
			ok := pen^.baseVar^.indirect or
				(proc #	pen^.baseVar^.address.proc);
			if DEBUG and TraceOptim then
			    Writef(output, ' param=%d', ord(ok));
			end;
		    end;
		end;
		if DEBUG and TraceOptim then
		    Writec(output, '\n');
		end;
		pen := pen^.next;
		pn := pn^.next;
	    end;
	end;
	if ok then
	    procExpr^.opt^.tailRecursion := true;
	    proc^.tailRecursion := true;
	end;
    end;
end OptRecursionFuncProc;

procedure OptRecursionReturn(proc : ProcNode; stn : StmtNode);
begin
    if (not OptNtail) and (stn^.returnVal^.kind = EXPRFUNC) then
	OptRecursionFuncProc(proc, stn^.returnVal^.func,stn^.returnVal^.params);
    end;
end OptRecursionReturn;

procedure OptRecursionProc(proc : ProcNode; stl : StmtList);
var
    stn     : StmtNode;
    cn      : CaseNode;
begin
    if OptNtail or (stl = nil) or (stl^.first = nil) then
    else
	stn := stl^.last;
	case stn^.kind of
	| STMTPROC :
	    OptRecursionFuncProc(proc, stn^.proc, stn^.params);
	| STMTIF :
	    OptRecursionProc(proc, stn^.thenList);
	    OptRecursionProc(proc, stn^.elseList);
	| STMTWITH :
	    OptRecursionProc(proc, stn^.withBody);
	| STMTCASE :
	    cn := stn^.cases^.first;
	    while cn # nil do
		OptRecursionProc(proc, cn^.stmts);
		cn := cn^.next;
	    end;
	    OptRecursionProc(proc, stn^.caseElse);
	| STMTSTMTS :
	    OptRecursionProc(proc, stn^.stmts);
	| else
	    (* nothing we can do *)
	end;
    end;
end OptRecursionProc;

procedure MarkIt(on : OptNode);
begin
    (* mark expression *)
    on^.marked := true;
    (* record outermost (smallest) marked level *)
    if (on^.markLevel > optBlockLevel) or (on^.markLevel = 0) then
	on^.markLevel := optBlockLevel;
    end;
    on^.rootCongruent^.defineTime := optTime;
    if DEBUG and TraceMark then
	Writef(output,'<%d>', on^.uniqueId);
	if on # on^.rootEqual then
	    Writec(output,'$');
	end;
	markItCount := markItCount + 1;
	if markItCount > 20 then
	    Writec(output, '\n');
	    markItCount := 0;
	end;
    end;
end MarkIt;

procedure MarkUp(on : OptNode);
var
    pen : ExprNode;
begin
    (* mark expression and parents *)
    (* ||| Can't we stop when we hit guy marked with 
       non-zero markLevel <= optBlockLevel? *)
    MarkIt(on);
    pen := on^.parent;
    while (pen # nil) and (pen^.opt # nil) do
	MarkIt(pen^.opt);
	pen := pen^.opt^.parent;
    end;
end MarkUp;

(* MarkOptProcCall: Called from the site of a procedure call.
   Mark (1) all CSEs that use non-local variables; (2) all
   CSEs that use pointers; (3) all CSEs that use local variables
   that are uplevel addressed.  (The CSEs that use VAR parameters
   to the procedure call have been invalidated previously.) 
   (Code courtesy of Chris Hanna at SRC) *)
PROCEDURE MarkOptProcCall();
    VAR con, mon: OptNode; 

    PROCEDURE MightBeInvalidated(CONST dependVar     : VarNode;
				 CONST dependPtrType : TypeNode) : BOOLEAN;
	VAR markIt : BOOLEAN;
    BEGIN
	markIt := TRUE;
	IF dependVar # NIL THEN
	    IF (dependVar^.address.kind in 
			MemoryTypeSet{MEMNORMAL,MEMFAST,MEMPARAM}) and
(* ||| "local" could be expanded to mean ancestor vars if this is an 
   up-level call *)
		    (dependVar^.address.proc = currOptProc) THEN
		(* Local variable, but mark it if uplevel addressed *)
		markIt := dependVar^.address.upLevelAddr;
	    END;
	ELSIF dependPtrType # NIL THEN
	    (* Pointer dereference, assume pointee changes *)
	ELSE
	    (* this expr cannot change, no need to invalidate it *)
	    markIt := FALSE;
	END;
	RETURN markIt;
    END MightBeInvalidated;

BEGIN
    optTime := optTime + 1;
    (* ||| Somehow Chris Hanna's code doesn't need to set markAllOptTime *)
    markAllOptTime := optTime;
    con := cseRootExpr[EXPRVAL];
    IF con # NIL THEN
	REPEAT
	    IF MightBeInvalidated(con^.expr^.dependVar,
				  con^.expr^.dependPtrType) THEN
		mon := con;
		REPEAT
		    MarkUp(mon);
		    mon := mon^.nextCongruent;
		UNTIL mon = con;
	    END;
	    con := con^.nextClass;
	UNTIL con = cseRootExpr[EXPRVAL];
    END;

(* We really also need to mark any EXPRCHECK's that do an implicit EXPRVAL *)
    con := cseRootExpr[EXPRCHECK];
    IF con # NIL THEN
	REPEAT
	    IF (* ||| I doubt this check is needed in most programs
		((con^.expr^.exprCheck = CHECKPTRREF) AND
		    (con^.expr^.checkPtr = PTRMODULA)) OR ||| *)
		((con^.expr^.exprCheck = CHECKVARIANT) and
		    MightBeInvalidated(con^.expr^.baseVar,
				       con^.expr^.basePtrType)) THEN
		mon := con;
		REPEAT
		    (* We actually shouldn't need to invalidate parents (which
		       may be merely address computations), but must or else
		       the rest of the optimizer barfs. *)
		    MarkUp(mon);
		    mon := mon^.nextCongruent;
		UNTIL mon = con;
	    END;
	    con := con^.nextClass;
	UNTIL con = cseRootExpr[EXPRCHECK];
    END;
END MarkOptProcCall;

(* MarkOptAll: Mark all values *)
procedure MarkOptAll;
var
    con, mon : OptNode;
begin
    if DEBUG and TraceOptim then
	Writef(output,'MarkOptAll\n');
    end;
    optTime := optTime + 1;
    markAllOptTime := optTime;
    markItCount := 0;
    con := cseRootExpr[EXPRVAL];
    if con # nil then
	repeat
	    mon := con;
	    repeat
		if (mon^.expr^.dependVar = nil) then
		    if DEBUG and TraceMark then
			Writef(output,'MarkOptAll: skip var param\n');
			PrintOptExpr(mon);
		    end;
		else
		    MarkUp(mon);
		end;
		mon := mon^.nextCongruent;
	    until mon = con;
	    con := con^.nextClass;
	until con = cseRootExpr[EXPRVAL];
    end;
end MarkOptAll;

(* MarkOptVarParam: Mark all non-local and var param values *)
procedure MarkOptVarParam;
var
    con, mon : OptNode;
    nonlocal : boolean;
begin
    if DEBUG and TraceOptim then
	Writef(output,'MarkOptVarParam\n');
    end;
    optTime := optTime + 1;
    markItCount := 0;
    con := cseRootExpr[EXPRVAL];
    if con # nil then
	repeat
	    nonlocal := true;
	    if con^.expr^.dependVar # nil then
		if con^.expr^.dependVar^.varParam then
		elsif con^.expr^.dependVar^.address.kind in
			MemoryTypeSet{MEMPARAM,MEMNORMAL,MEMFAST} then
		    if con^.expr^.dependVar^.address.proc = currOptProc then
			nonlocal := false;
		    end;
		end;
	    elsif con^.expr^.dependPtrType = nil then
		(* for indirect addresses, no need to invalidate them *)
		nonlocal := false;
	    end;
	    if nonlocal then
		mon := con;
		repeat
		    MarkUp(mon);
		    mon := mon^.nextCongruent;
		until mon = con;
	    end;
	    con := con^.nextClass;
	until con = cseRootExpr[EXPRVAL];
    end;
end MarkOptVarParam;

(* MarkOptGlobal: Mark all var parameter values *)
procedure MarkOptGlobal;
var
    con, mon : OptNode;
    varParam : boolean;
begin
    if DEBUG and TraceOptim then
	Writef(output,'MarkOptGlobal\n');
    end;
    optTime := optTime + 1;
    markItCount := 0;
    con := cseRootExpr[EXPRVAL];
    if con # nil then
	repeat
	    varParam := false;
	    if con^.expr^.dependVar # nil then
		varParam :=  con^.expr^.dependVar^.varParam;
	    end;
	    if varParam then
		mon := con;
		repeat
		    MarkUp(mon);
		    mon := mon^.nextCongruent;
		until mon = con;
	    end;
	    con := con^.nextClass;
	until con = cseRootExpr[EXPRVAL];
    end;
end MarkOptGlobal;

(*
    Marking:
    Expressions are marked when they might no longer be valid due to an
    assignment to one of their parts.

    Strategy:
    All address expressions (including VAR, DOT, and SUBSCR) based on local or
    global variables are identified with the variable (baseVar) that they are
    part of.  All address expressions based on pointers (DEREF) are identified
    with the type (basePtrType) of the record they are part of [The assumption
    is that pointers to different types point to different objects].

    These dependencies are moved into the value expressions as dependVar and
    dependPtrType.  Only value expressions are marked directly, since addresses
    are not changed by assignment.  The marks are propagated upward in the
    expression tree until all expressions containing the value are marked.

    Special cases:
	With statements:  The with statement is marked has depending on a
	    variable or pointer, according to the expression.  Even though they
	    involve a derefence, implicit qualifiers are marked as if they were
	    written out.
	VAR parameters:  Marking VAR parameters marks values based on all
	non-local variables.  Marking a non-local variable marks values based
	on all VAR parameters.
	Call nested procedure that makes uplevel references: Mark all values.
	Call non-nested procedure: Mark all non-local values.
	Note: for now, mark everyting on procedure call

*)

procedure MarkOptSave(const en : ExprNode);
begin
    (* A save expression does an implicit assignment to exprSaveVar, and so
       we must mark all references to that variable. *)
    optTime := optTime + 1;
    MarkVar(en^.exprSaveVar);
end MarkOptSave;

procedure MarkVar(const baseVar : VarNode);
var
    mon, con : OptNode;
begin
    (* Update mark time for variable *)
    baseVar^.markTime.proc := currOptProc;
    baseVar^.markTime.time := optTime;

    (* update defineTime in this var's expressions *)
    if cseRootExpr[EXPRVAR] # nil then
	 con := cseRootExpr[EXPRVAR];
	 repeat
	    if con^.expr^.exprVar = baseVar then
		con^.rootCongruent^.defineTime := optTime;
	    end;
	    con := con^.nextClass;
	 until (con = cseRootExpr[EXPRVAR]);
    end;

    (* Look for values that depend on this variable and mark them *)
    con := cseRootExpr[EXPRVAL];
    if con # nil then
	repeat
	    mon := con;
	    repeat
		(* if value depends on this var, mark it *)
		if mon^.expr^.dependVar = baseVar then
		    (* mark all containing expressions *)
		    MarkUp(mon);
		end;
		mon := mon^.nextCongruent;
	    until mon = con;
	    con := con^.nextClass;
	until con = cseRootExpr[EXPRVAL];
    end;
end MarkVar;

procedure MarkOptExpr(en : ExprNode);
var
    baseVar : VarNode;
    basePtrType : TypeNode;
    con, mon : OptNode;
    global, varParam : boolean;
begin
    if DEBUG and TraceMark then
	Writef(output,'MarkOptExpr:');
	WriteExpr(en);
	Writec(output, '\n');
    end;
    markItCount := 0;
    optTime := optTime + 1;
    if en^.baseVar # nil then
	baseVar := en^.baseVar;
	varParam := baseVar^.varParam;
	global := baseVar^.address.kind = MEMGLOBAL;
	if DEBUG and TraceMark then
	    Writef(output,'Mark base var ');
	    WriteString(output,baseVar^.name);
	    if varParam then
		Writef(output,'(var param)');
	    elsif global then
		Writef(output,'(global)');
	    end;
	    Writec(output, '\n');
	end;
	(* update mark time *)
	MarkVar(baseVar);
	if varParam then
	    (* var param, mark all non-locals *)
	    MarkOptVarParam();
	end;
	if global then
	    (* global, mark all var params *)
	    MarkOptGlobal();
	end;
	if en^.basePtrType # nil then
	    ExprError(en,'MarkOptExpr: both not nil?');
	    if DEBUG and TraceOptim then
		Writef(output,'MarkOptExpr: both not nil ');
		WriteExpr(en);
		Writec(output, '\n');
	    end;
	end;
    elsif en^.basePtrType = nil then
	ExprError(en,'MarkOptExpr: both nil?');
	if DEBUG and TraceOptim then
	    Writef(output,'MarkOptExpr: both nil ');
	    WriteExpr(en);
	    Writec(output, '\n');
	end;
    else
	(* mark a pointer type *)
	if DEBUG and TraceMark then
	    Writef(output,'Mark type ');
	    WriteString(output,en^.basePtrType^.name);
	    Writec(output, '\n');
	end;
	basePtrType := en^.basePtrType;
	(* update mark time *)
	basePtrType^.markTime.proc := currOptProc;
	basePtrType^.markTime.time := optTime;
	(* update defineTime in this type's expressions *)
	if cseRootExpr[EXPRVAL] # nil then
	    con := cseRootExpr[EXPRVAL];
	    repeat
		if con^.expr^.basePtrType = basePtrType then
		    con^.rootCongruent^.defineTime := optTime;
		end;
		con := con^.nextClass;
	     until (con = cseRootExpr[EXPRVAL]);
	end;

	(* mark all expressions that depend on this type *)
	con := cseRootExpr[EXPRVAL];
	if con # nil then
	    repeat
		mon := con;
		repeat
		    (* if value depends on this type, mark it *)
		    if mon^.expr^.dependPtrType = basePtrType then
			(* mark all containing expressions *)
			MarkUp(mon);
		    end;
		    mon := mon^.nextCongruent;
		until mon = con;
		con := con^.nextClass;
	    until con = cseRootExpr[EXPRVAL];
	end;
    end;
end MarkOptExpr;

procedure EqualConst(a,b : ConstNode) : boolean;
begin
    if a^.kind # b^.kind then
	return false;
    else
	case a^.kind of
	| DTCHAR	: return a^.charVal = b^.charVal
	| DTINTEGER,
	  DTCARDINAL    : return a^.cardVal = b^.cardVal
	| DTBOOLEAN     : return a^.boolVal = b^.boolVal
	| DTREAL, 
	  DTLONGREAL    : return a^.realVal = b^.realVal
	| DTSET		: return (a^.setVal^.setType = b^.setVal^.setType)
				and (a^.setVal^.value = b^.setVal^.value)
	| DTENUMERATION : return (a^.enumVal^.enumType = b^.enumVal^.enumType)
				and (a^.enumVal^.enumOrd = b^.enumVal^.enumOrd)
	| DTSTRING      : return a^.strVal = b^.strVal
	| DTPROC	: return a^.procVal = b^.procVal
	| DTPOINTER     : return true (* nil pointers match *)
	end;
    end;
end EqualConst;

procedure Congruent(a, b : OptNode) : boolean;
var
    aen, ben, apen, bpen : ExprNode;
    aesn, besn : ExprSetNode;
    acsn, bcsn : ConstSetNode;
begin
    aen := a^.expr;
    ben := b^.expr;
    if aen^.kind # ben^.kind then
	return false;
    elsif a^.rootCongruent = b^.rootCongruent then
	return true;
    else
	case aen^.kind of
	| EXPRNAME :
	    ExprError(aen,'Congruent: bad expr kind');
	
	| EXPRSYM :
	    return aen^.exprSym = ben^.exprSym;
	
	| EXPRCONST :
	    return EqualConst(aen^.exprConst,ben^.exprConst);
	
	| EXPRVAR :
	    return aen^.exprVar = ben^.exprVar;

	| EXPRUNOP :
	    return (aen^.exprUnOp = ben^.exprUnOp) and 
		    Congruent(aen^.opnd^.opt,ben^.opnd^.opt);

	| EXPRBINOP :
	    if aen^.exprBinOp # ben^.exprBinOp then
		return false;
	    elsif aen^.exprBinOp in TokenSet{TKAND,TKOR} then
		(* conditional expressions, never congruent *)
		return false
	    elsif Congruent(aen^.opnd1^.opt,ben^.opnd1^.opt) and
		    Congruent(aen^.opnd2^.opt,ben^.opnd2^.opt) then
		return true;
	    elsif aen^.exprBinOp in TokenSet{TKPLUS,TKASTERISK,TKEQUALS,
		    TKSHARP,TKNOTEQUAL}	then
		(* check for reversed operands on commutative operator *)
		return Congruent(aen^.opnd1^.opt,ben^.opnd2^.opt) and
			Congruent(aen^.opnd2^.opt,ben^.opnd1^.opt)
	    else
		return false;
	    end;
	
	| EXPRFUNC :
	    if (aen^.func^.kind = EXPRCONST) and
		    (aen^.func^.exprConst^.procVal^.builtin in pureFunctions)
		    and Congruent(aen^.func^.opt,ben^.func^.opt) then
		apen := aen^.params^.first;
		bpen := ben^.params^.first;
		while (apen # nil) and (bpen # nil) do
		    if not Congruent(apen^.opt,bpen^.opt) then
			return false;
		    end;
		    apen := apen^.next;
		    bpen := bpen^.next;
		end;
		return apen = bpen;
	    else
		return false
	    end;
	
	| EXPRVAL :
	    if (SizeOf(aen^.exprType) = SizeOf(ben^.exprType)) and
		    (aen^.dependPtrType = ben^.dependPtrType) and
		    Congruent(aen^.exprVal^.opt,ben^.exprVal^.opt) then
		if (aen^.dependVar # ben^.dependVar) then
		    ExprError(aen,'Congruent: different dependencies?');
		end;
		return true;
	    else
		return false;
	    end;
	
	| EXPRCHECK :
	    return  (aen^.exprCheck = ben^.exprCheck) and
		    (aen^.checkVar = ben^.checkVar) and
		    (aen^.checkType = ben^.checkType) and
		    (aen^.checkLower = ben^.checkLower) and
		    (aen^.checkUpper = ben^.checkUpper) and
		    (aen^.checkVariant = ben^.checkVariant) and
		    Congruent(aen^.checkExpr^.opt,ben^.checkExpr^.opt);
	
	| EXPRSAVE :
	    return (aen^.exprSaveVar = ben^.exprSaveVar) and
		    Congruent(aen^.exprSave^.opt,ben^.exprSave^.opt);
	
	| EXPRDESCRIPTOR :
	    if 	(aen^.descripMode = ben^.descripMode) and
		    (aen^.descripCount = ben^.descripCount) and
		    Congruent(aen^.descripBase^.opt, ben^.descripBase^.opt) then
		apen := aen^.descrips^.first;
		bpen := ben^.descrips^.first;
		while (apen # nil) do
		    if not Congruent(apen^.opt, bpen^.opt) then
			return false;
		    end;
		    apen := apen^.next;
		    bpen := bpen^.next;
		end;
		return true;
	    else
		return false;
	    end;

	| EXPRINLINE :
	    return false;
	
	| EXPRSET :
	    if aen^.setType # ben^.setType then
		return false;
	    elsif (aen^.setExpr = nil) or (ben^.setExpr = nil) then
		if aen^.setExpr # ben^.setExpr then
		    return false;
		end;
	    else
		aesn := aen^.setExpr^.first;
		besn := ben^.setExpr^.first;
		while (aesn # nil) and (besn # nil) do
		    if not Congruent(aesn^.lower^.opt,besn^.lower^.opt) then
			return false;
		    elsif (aesn^.upper = nil) or (besn^.upper = nil) then
			if aesn^.upper # besn^.upper then
			    return false;
			end;
		    elsif not Congruent(aesn^.upper^.opt,besn^.upper^.opt) then
			return false;
		    end;
		    aesn := aesn^.next;
		    besn := besn^.next;
		end;
		if aesn # besn then
		    return false;
		end;
	    end;
	    if (aen^.setConst = nil) or (ben^.setConst = nil) then
		return aen^.setConst = ben^.setConst;
	    else
		acsn := aen^.setConst^.first;
		bcsn := ben^.setConst^.first;
		while (acsn # nil) and (bcsn # nil) do
		    if not EqualConst(acsn^.lower,bcsn^.lower) then
			return false;
		    elsif (acsn^.upper = nil) or (bcsn^.upper = nil) then
			if acsn^.upper # bcsn^.upper then
			    return false;
			end;
		    elsif not EqualConst(acsn^.upper,bcsn^.upper) then
			return false;
		    end;
		    acsn := acsn^.next;
		    bcsn := bcsn^.next;
		end;
		return acsn = bcsn;
	    end;
	end (* case *);
    end (* if *);
end Congruent;

(* return cost of containing on *)
procedure ContainedCost(on : OptNode) : OptCost;
begin
    if on = nil then
	return COSTNONE;
    elsif on^.cost = COSTCSE then
	return COSTCONTAINEDCSE;
    else
	return COSTCONTAINEDLOOP;
    end;
end ContainedCost;

procedure EnterExprAtLevel(en : ExprNode; pen : ExprNode; minTime : OptTime;
	sol : SaveOptLevel): OptTime;
var
    old : SaveOptLevel;
    result : OptTime;
begin
    old.level := optBlockLevel;
    old.floor := optBlockFloor;
    old.ceiling := optBlockCeiling;
    old.blockTime := blockOptTime;
    optBlockLevel := sol.level;
    optBlockFloor := sol.floor;
    optBlockCeiling := sol.ceiling;
    blockOptTime := sol.blockTime;
    result := EnterExpr(en,pen,minTime);
    optBlockLevel := old.level;
    optBlockFloor := old.floor;
    optBlockCeiling := old.ceiling;
    blockOptTime := old.blockTime;
    return result;
end EnterExprAtLevel;

procedure EnterExpr(en, pen : ExprNode; minTime : OptTime) : OptTime;
var
    on, ton : OptNode;
begin
    on := NewOptNode(en,pen);
    case en^.kind of
    | EXPRBINOP :
	on^.cost := COSTCSE;
	on^.containedNonTrivial := on;
        if en^.opnd1^.opt^.ineligible or en^.opnd2^.opt^.ineligible then
	    on^.ineligible := true;
	end;
	if en^.exprBinOp = TKPLUS then
	    case target of
	    | TARGETVAX :
		if en^.opnd1^.kind in ExprKindSet{EXPRCONST,EXPRVAR} then
		    if en^.opnd2^.kind = EXPRCONST then
			on^.cost := COSTNONE;
			on^.containedNonTrivial := nil;
		    else
			on^.cost := COSTLOOP;
			on^.containedNonTrivial := on;
		    end;
		elsif en^.opnd2^.kind in ExprKindSet{EXPRCONST,EXPRVAR} then
		    if en^.opnd1^.kind = EXPRCONST then
			on^.cost := COSTNONE;
			on^.containedNonTrivial := nil;
		    else    
			on^.cost := COSTLOOP;
			on^.containedNonTrivial := on;
		    end;
		end;
	    
	    | TARGETTITAN, TARGETTITANM :
		if en^.opnd1^.kind = EXPRCONST then
		    on^.cost := COSTLOOP;
		    on^.containedNonTrivial := on;
		    if en^.opnd2^.kind = EXPRVAR then
			on^.cost := COSTNONE;
			on^.containedNonTrivial := nil;
		    end;
		elsif en^.opnd2^.kind = EXPRCONST then
		    on^.cost := COSTLOOP;
		    on^.containedNonTrivial := on;
		    if en^.opnd1^.kind = EXPRVAR then
			on^.cost := COSTNONE;
			on^.containedNonTrivial := nil;
		    end;
		end;
	    end (* case target of *);
	elsif en^.exprBinOp = TKASTERISK then
	    (* don't count indexing by address unit size *)
	    if en^.operType = addressTypeNode then
		if en^.opnd1^.kind = EXPRCONST then
		    if en^.opnd1^.exprConst^.kind = DTCARDINAL then
			if trunc(en^.opnd1^.exprConst^.cardVal) = UNITSIZE then
			    ton := en^.opnd2^.opt;
			    on^.cost := ContainedCost(ton^.containedNonTrivial);
			    on^.containedNonTrivial := ton^.containedNonTrivial;
			elsif trunc(en^.opnd1^.exprConst^.cardVal) < UNITSIZE
			then
			    (* can't optimize sub-unit addressing *)
			    on^.ineligible := true;
			end;
		    end;
		elsif en^.opnd2^.kind = EXPRCONST then
		    if en^.opnd2^.exprConst^.kind = DTCARDINAL then
			if trunc(en^.opnd2^.exprConst^.cardVal) = UNITSIZE then
			   ton := en^.opnd1^.opt;
			   on^.cost := ContainedCost(ton^.containedNonTrivial);
			   on^.containedNonTrivial := ton^.containedNonTrivial;
			elsif trunc(en^.opnd2^.exprConst^.cardVal) < UNITSIZE
			    then
			    (* can't optimize sub-unit addressing *)
                            on^.ineligible := true;
                        end;
		    end;
		end;
	    end;
	end;
	EnterClass(cseRootBinOp[en^.exprBinOp],on);
    
    | EXPRUNOP :
	if en^.exprUnOp = TKNOT then
	    (* NOT's get removed from flow control in code generation, so can't
	       make it a common subexpression.  Since we don't know if this is 
	       flow control or value, be paranoid. *)
	    on^.cost := COSTNONE;
	    on^.containedNonTrivial := nil;
	    EnterClass(cseRootUnOp[TKNOT], on);
	else
	    on^.cost := COSTCSE;
	    on^.containedNonTrivial := on;
	    EnterClass(cseRootUnOp[en^.exprUnOp], on);
	end;

    | EXPRVAR :
	on^.cost := COSTNONE;
	on^.containedNonTrivial := nil;
	EnterClass(cseRootExpr[EXPRVAR],on);
	
    | EXPRVAL :
	case target of
	| TARGETVAX :
	    ton := en^.exprVal^.opt;
	    case ton^.cost of
	    | COSTNONE:	(* simple load move out of loop *)
		on^.cost := COSTLOOP;
		on^.containedNonTrivial := on;
		if on^.expr^.exprVal^.kind = EXPRVAR then
		    if on^.expr^.exprVal^.exprVar^.address.kind = MEMFAST then
			on^.cost := COSTNONE;
			on^.containedNonTrivial := nil;
		    end;
		end;
	    
	    | COSTCONTAINEDLOOP, COSTCONTAINEDCSE :
		(* contained nonTrivial, propagate it *)
		on^.cost := ContainedCost(ton^.containedNonTrivial);
		on^.containedNonTrivial := ton^.containedNonTrivial;
	    
	    | COSTLOOP :
		on^.cost := COSTCONTAINEDLOOP;
		on^.containedNonTrivial := ton;
	    
	    | COSTCSE :
		on^.cost := COSTCONTAINEDCSE;
		on^.containedNonTrivial := ton;
	    end (* case ton^.cost of *);
	
	| TARGETTITAN, TARGETTITANM :
	    ton := en^.exprVal^.opt;
	    if ton^.cost # COSTNONE then
		on^.cost := COSTCSE;
		on^.containedNonTrivial := on;
	    elsif en^.exprVal^.kind = EXPRVAR then
		on^.cost := COSTNONE;
		on^.containedNonTrivial := nil;
	    else
		on^.cost := ContainedCost(ton^.containedNonTrivial);
		on^.containedNonTrivial := ton^.containedNonTrivial;
	    end;
	end;
	EnterClass(cseRootExpr[EXPRVAL],on);
	
    | EXPRCHECK :
	if (en^.exprCheck in CheckKindSet{CHECKSUBSCR,CHECKRANGE}) and
		(en^.checkExpr^.kind = EXPRCONST) then
	    on^.cost := COSTNONE;
	    on^.containedNonTrivial := nil;
	elsif en^.doCheck then
	    on^.cost := COSTCSE;
	    on^.containedNonTrivial := on;
	else
	    (* Check disable, node has same weight as en^.checkExpr *)
	    on^.cost := en^.checkExpr^.opt^.cost;
	    on^.containedNonTrivial := en^.checkExpr^.opt^.containedNonTrivial;
	end;
	EnterClass(cseRootExpr[EXPRCHECK], on);

    | EXPRSET :
(* |||	if en^.setExpr # nil then *)
	    on^.cost := COSTCSE;
	    on^.containedNonTrivial := on;
(* |||	end; *)
	EnterClass(cseRootExpr[EXPRSET],on);

    | EXPRSAVE,EXPRFUNC :
	on^.cost := COSTCSE;
	on^.containedNonTrivial := on;
	EnterClass(cseRootExpr[en^.kind],on);

    | EXPRDESCRIPTOR :
	on^.cost := COSTNONE;
	on^.containedNonTrivial := nil;
	EnterClass(cseRootExpr[EXPRDESCRIPTOR], on);

    | EXPRCONST :
(* |||	if en^.exprType^.kind = DTLONGREAL then
	    on^.cost := COSTCSE;
	    on^.containedNonTrivial := on;
	end; *)
	EnterClass(cseRootExpr[EXPRCONST], on);

    | else
	EnterClass(cseRootExpr[en^.kind],on);
    end (* case en^.kind of *);
    if minTime < 0 then
	MarkIt(on^.rootEqual);
    elsif on^.rootCongruent^.defineTime < minTime then
	on^.rootCongruent^.defineTime := minTime;
    end;
    return on^.rootCongruent^.defineTime;
end EnterExpr;

(* EnterClass:
    Four possibilities:
	No other expr in that class => make sole element of class
	Does not match any element in class => AddToClassList
	Congruent to some element, but not equal => AddToCongruentList
	Equal to some element => AddToEqualList
*)
procedure EnterClass(var root : OptNode; on : OptNode);
var
    search : OptNode;
begin
    if root = nil then
	(* make sole element of class *)
	AddToClassList(root,on);
    else
	search := root;
	repeat
	    if Congruent(on,search) then
		(* found same class, look for congruent *)
		EnterCongruent(search,on);
		return;
	    else
		search := search^.nextClass;
	    end;
	until search = root;
	(* Not found: new class, add after end of list *)
	AddToClassList(root^.prevClass,on);
    end;
end EnterClass;

(* add newOne to doubly-linked All list, after prevOne *)
procedure AddToAllList(prevOne, newOne : OptNode);
var
    nextOne : OptNode;
begin
    nextOne := prevOne^.nextAll;
    newOne^.nextAll := nextOne;
    newOne^.prevAll := prevOne;
    prevOne^.nextAll := newOne;
    nextOne^.prevAll := newOne;
end AddToAllList;

(* add newOne to doubly-linked Active list, after prevOne *)
procedure AddToActiveList(prevOne, newOne : OptNode);
var
    nextOne : OptNode;
begin
    nextOne := prevOne^.nextActive;
    newOne^.nextActive := nextOne;
    newOne^.prevActive := prevOne;
    prevOne^.nextActive := newOne;
    nextOne^.prevActive := newOne;
end AddToActiveList;

(* remove oldOne from doubly-linked Active list *)
procedure RemoveFromActiveList(oldOne : OptNode);
var
    prevOne, nextOne : OptNode;
begin
    if oldOne^.nextActive # nil then
	nextOne := oldOne^.nextActive;
	prevOne := oldOne^.prevActive;
	prevOne^.nextActive := nextOne;
	nextOne^.prevActive := prevOne;
	oldOne^.nextActive := nil;
	oldOne^.prevActive := nil;
    end;
end RemoveFromActiveList;

(* add newOne to doubly-linked Class list, after prevOne *)
procedure AddToClassList(var prevOne : OptNode; newOne : OptNode);
var
    nextOne : OptNode;
begin
    if prevOne = nil then
	prevOne := newOne;
    else
	nextOne := prevOne^.nextClass;
	newOne^.nextClass := nextOne;
	newOne^.prevClass := prevOne;
	prevOne^.nextClass := newOne;
	nextOne^.prevClass := newOne;
    end;
    AddToActiveList(activeExprs^.prevActive,newOne);
end AddToClassList;

procedure EnterCongruent(root : OptNode; on : OptNode);
var
    search : OptNode;
begin
    search := root;
    repeat
	if (search^.rootEqual # search) or search^.marked or search^.purged
		or (search^.createLevel > optBlockCeiling)
		or (search^.createLevel < optBlockFloor)
	then
	    (* not equal *)
	    search := search^.nextCongruent;
	else
	    (* equal *)
	    AddToEqualList(search,on);
	    return;
	end;
    until (search = root);
    (* Didn't fine equal one *)
    AddToCongruentList(root^.prevCongruent,on);
end EnterCongruent;

(* add newOne to doubly-linked Congruent list, after prevOne *)
procedure AddToCongruentList(prevOne, newOne : OptNode);
var
    nextOne : OptNode;
begin
    nextOne := prevOne^.nextCongruent;
    newOne^.nextCongruent := nextOne;
    newOne^.prevCongruent := prevOne;
    prevOne^.nextCongruent := newOne;
    nextOne^.prevCongruent := newOne;
    newOne^.rootCongruent := prevOne^.rootCongruent;
    AddToActiveList(activeExprs^.prevActive,newOne);
end AddToCongruentList;

procedure CheckEqualOpt(a,b : OptNode) : boolean;
var
    aen, ben : ExprNode;
    aon, bon, aon1, aon2, bon1, bon2 : OptNode;
    aesn, besn : ExprSetNode;
begin
    aen := a^.expr;
    ben := b^.expr;
    case aen^.kind of
    | EXPRCONST,
      EXPRVAR,
      EXPRNAME,
      EXPRSYM :(* nothing to check *)
	return true;

    | EXPRUNOP :
	return aen^.opnd^.opt^.rootEqual = ben^.opnd^.opt^.rootEqual;
    
    | EXPRBINOP:
	aon1 := aen^.opnd1^.opt;
	aon2 := aen^.opnd2^.opt;
	bon1 := ben^.opnd1^.opt;
	bon2 := ben^.opnd2^.opt;
	if (aon1^.rootEqual = bon1^.rootEqual)
		    and (aon2^.rootEqual = bon2^.rootEqual) then
	    return true;
	elsif (aen^.exprBinOp in TokenSet{TKPLUS,TKASTERISK,TKEQUALS,
		    TKSHARP,TKNOTEQUAL}) then
	    return (aon1^.rootEqual = bon2^.rootEqual) and 
		   (aon2^.rootEqual = bon1^.rootEqual);
	else
	    return false;
	end;
    
    | EXPRFUNC :
	if aen^.func^.opt^.rootEqual # ben^.func^.opt^.rootEqual then
	    return false;
	end;
	aen := aen^.params^.first;
	ben := ben^.params^.first;
	while aen # nil do
	    if aen^.opt^.rootEqual # ben^.opt^.rootEqual then
		return false;
	    end;
	    aen := aen^.next;
	    ben := ben^.next;
	end;
	return true;

    | EXPRVAL :
	return aen^.exprVal^.opt^.rootEqual = ben^.exprVal^.opt^.rootEqual;
    
    | EXPRCHECK :
	return aen^.checkExpr^.opt^.rootEqual = ben^.checkExpr^.opt^.rootEqual;
    
    | EXPRSET :
	if aen^.setExpr = nil then
	    return true;
	end;
	aesn := aen^.setExpr^.first;
	besn := ben^.setExpr^.first;
	while aesn # nil do
	    if aesn^.lower^.opt^.rootEqual # besn^.lower^.opt^.rootEqual then
		return false;
	    end;
	    if (aesn^.upper # nil) and
		(aesn^.upper^.opt^.rootEqual # besn^.upper^.opt^.rootEqual) then
		return false;
	    end;
	    aesn := aesn^.next;
	    besn := besn^.next;
	end;
	return aesn = besn;

    | EXPRDESCRIPTOR :
	if (aen^.descripBase^.opt^.rootEqual # ben^.descripBase^.opt^.rootEqual)
	then
	    return false;
	end;
	aen := aen^.descrips^.first;
	ben := ben^.descrips^.first;
	while aen # nil do
	    if aen^.opt^.rootEqual # ben^.opt^.rootEqual then
		return false;
	    end;
	    aen := aen^.next;
	    ben := ben^.next;
	end;
	return true;

	
    | EXPRSAVE :
	return aen^.exprSave^.opt^.rootEqual = ben^.exprSave^.opt^.rootEqual;
    
    | EXPRINLINE :
	return false;
    end;
end CheckEqualOpt;

(* add newOne to doubly-linked Equal list, after prevOne *)
procedure AddToEqualList(prevOne, newOne : OptNode);
var
    nextOne : OptNode;
begin
    if not CheckEqualOpt(prevOne,newOne) then
	ExprError(newOne^.expr,'AddToEqualList: children not equal');
	PrintOptExpr(newOne);
    end;
    (* add to equal list *)
    nextOne := prevOne^.nextEqual;
    newOne^.nextEqual := nextOne;
    newOne^.prevEqual := prevOne;
    prevOne^.nextEqual := newOne;
    nextOne^.prevEqual := newOne;
    newOne^.rootEqual := prevOne^.rootEqual;

    (* add to congruent list *)
    nextOne := prevOne^.nextCongruent;
    newOne^.nextCongruent := nextOne;
    newOne^.prevCongruent := prevOne;
    prevOne^.nextCongruent := newOne;
    nextOne^.prevCongruent := newOne;
    newOne^.rootCongruent := prevOne^.rootCongruent;

    if DEBUG and TraceCount then
	Writef(output,'AddToEqualList: %d/%d %d+1\n', newOne^.uniqueId,
		newOne^.rootEqual^.uniqueId, newOne^.rootEqual^.neededCount);
    end;
    inc(newOne^.rootEqual^.neededCount);
    inc(newOne^.rootEqual^.referencedCount);
end AddToEqualList;

procedure PrintOptExpr(on : OptNode);
var
    en : ExprNode;
begin
    Writef(output,'Expr #%d\n', on^.uniqueId);
    if on^.parent = nil then
	Writef(output,'    Parent=nil');
    elsif on^.parent^.opt = nil then
	Writef(output,'    Parent=not yet entered');
    else
	Writef(output,'    Parent=%d', on^.parent^.opt^.uniqueId);
    end;
    Writef(output, ', Congruent=%d, Equal=%d\n', on^.rootCongruent^.uniqueId,
	    on^.rootEqual^.uniqueId);
    Writef(output,'    marked=%n, joinMark=%n, purged=%n, removed=%n\n',
	    on^.marked,on^.joinMark, on^.purged, on^.removed);
    Writef(output,
	   '    createLevel=%d, markLevel=%d, defineTime=%d/%d, loopNest=%d\n',
	    on^.createLevel, on^.markLevel, on^.defineTime,
	    on^.rootCongruent^.defineTime, on^.loopNest);
    Writef(output,'    usage=%n, temp=%d', on^.usage, on^.tempNumber);
    if on^.inductionVar # nil then
	Writef(output,', indVar=');
	if on^.inductionVar^.address.kind = MEMGLOBAL then
	    WriteString(output,
		    on^.inductionVar^.address.gvn^.globalName);
	else
	    WriteString(output,on^.inductionVar^.name);
	    Writef(output,' addrKind=%n %d %d %d',
		    on^.inductionVar^.address.kind, 
		    on^.inductionVar^.address.level,
		    on^.inductionVar^.address.proc^.block,
		    on^.inductionVar^.address.offset);
	end;
    end;
    Writec(output, '\n');
    Writef(output,
	    '    cost=%n, eligible=%n, used=%d, needed=%d, referenced=%d\n',
	    on^.cost, on^.eligible, on^.usedCount, on^.neededCount,
	    on^.referencedCount);
    if on^.containedNonTrivial # nil then
	Writef(output,'    containedNonTrivial=%d\n',
		on^.containedNonTrivial^.uniqueId);
    end;
    en := on^.expr;
    if en = nil then
	Writef(output,'no expr node\n');
    else
	Writef(output,'    line=%d, file=', en^.lineNumber);
	    WriteString(output,en^.fileName);
	Writef(output,', expr=');
	    WriteExpr(en);
	if en^.baseVar # nil then
	    Writef(output,', baseVar=');
	    WriteString(output,en^.baseVar^.name);
	end;
	if en^.basePtrType # nil then
	    Writef(output,', basePtrType=');
	    WriteString(output,en^.basePtrType^.name);
	end;
	if en^.kind = EXPRVAL then
	    if en^.dependVar # nil then
		Writef(output,', dependVar=');
		WriteString(output,en^.dependVar^.name);
	    end;
	    if en^.dependPtrType # nil then
		Writef(output,', dependPtrType=');
		WriteString(output,en^.dependPtrType^.name);
	    end;
	end;
	Writec(output, '\n');
    end;
end PrintOptExpr;

procedure DumpOptEqual(root : OptNode);
var
    on : OptNode;
begin
    on := root;
    Writef(output,'           ');
    repeat
	Writef(output,' %d', on^.uniqueId);
	on := on^.nextEqual;
    until on = root;
    Writec(output, '\n');
end DumpOptEqual;

procedure DumpOptCongruent(root : OptNode);
var
    on : OptNode;
begin
    on := root;
    repeat
	Writef(output,'        %d\d', on^.uniqueId);
	DumpOptEqual(on);
	on := on^.nextCongruent;
    until on = root;
end DumpOptCongruent;

procedure DumpOptClass(root : OptNode);
var
    on : OptNode;
begin
    on := root;
    repeat
	Writef(output,'    %d\n', on^.uniqueId);
	DumpOptCongruent(on);
	on := on^.nextClass;
    until on = root;
end DumpOptClass;

procedure DumpOptExprs;
var
    ek : ExprKind;
    token : Token;
    on : OptNode;
begin
(****
    for ek := EXPRNAME to EXPRBAD do
	if cseRootExpr[ek] # nil then
	    Writef(output,'Class Expr %n\n', ek);
	    DumpOptClass(cseRootExpr[ek]);
	end;
    end;
    for token := first(Token) to last(Token) do
	if cseRootUnOp[token] # nil then
	    Writef(output,'Class UnOp %n\n', token);
	    DumpOptClass(cseRootUnOp[token]);
	end;
    end;
    for token := first(Token) to last(Token) do begin
	if cseRootBinOp[token] # nil then begin
	    Writef(output,'Class BinOp %n\n', token);
	    DumpOptClass(cseRootBinOp[token]);
	end;
    end;
    Writec(output, '\n');
****)
    Writef(output,'All exprs\n');
    on := allExprs^.nextAll;
    while on # allExprs do
	PrintOptExpr(on);
	on := on^.nextAll;
    end;
end DumpOptExprs;

begin (* Optim *)
    generateUniqueId := 1;
    optLoopNest := 1;
    ResetOptimizer();
end Optim.
