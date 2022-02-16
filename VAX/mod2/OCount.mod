implementation module OCount;

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

from MemLib import
    ALLOCATE;

from Machine import
    BYTESIZE, HALFSIZE, WORDSIZE, LONGREALSIZE;

from Globals import
    TargetMachine, target, TraceCount, TraceOptim, DEBUG;

from Tokens import
    TKPLUS, TKASTERISK, TKMINUS, TokenSet;

from Strings import
    WriteString;

from Symbols import
    NUMOPTTEMPS, NUMFASTWORDS, NUMTEMPSTORAGE, MemoryOffset, EvalMode,
    TypeNode, VarNode, ParamList, InlineParamList,
    IPPARAM, InlineParamNode, ProcNode, ExprNode, ExprList, StmtNode,
    StmtList, CaseNode, ExprSetNode, ParamNode, CodeNode, TempMapNode,
    SYMTYPE, MEMGLOBAL, MEMFAST, DataType, DataTypeSet,
    BIPNOTBIP, ExprKind, StmtKind, ParamKind, 
    TempNumber, OptUsage, COSTCSE, COSTCONTAINEDCSE, OptCostSet, OptNode;

from TypeInfo import
    SizeOf;
    
from BuildExpr import
    WriteExpr;

from OptBuiltin import
    CountBuiltin;

from Errors import
    ErrorNumber, ErrorName, ExprError;

from Optim import
    NULLTEMP, LOOPBIAS, cseRootExpr, allExprs, PrintOptExpr, optLoopNest;

const
    MINCOUNTFORREG = 3;	(* number references to get allocated to a register *)
type
    StorageCount = record
	count     : integer;
	offset    : MemoryOffset;
	addressed : boolean;
    end;
    TempSet = set of [0..NUMOPTTEMPS];
var
    refCount    : array [1..NUMTEMPSTORAGE] of StorageCount;
    maxTemp     : TempNumber;
    inUseTemps, inUseTempsToBeFreed : TempSet;
    tempLevel   : array [1..NUMOPTTEMPS] of integer;

procedure InitTemps;
begin
    inUseTemps := TempSet{};
    inUseTempsToBeFreed := TempSet{};
    maxTemp := 0;
end InitTemps;

procedure AllocTemp (): TempNumber;
var
    t : TempNumber;
begin
    case target of
    | TARGETVAX :
	t := 1;
	while (t <= NUMOPTTEMPS) do
	    if (t in inUseTemps) then
		t := t + 1;
	    else
		incl(inUseTemps, t);
		tempLevel[t] := optLoopNest;
		if t > maxTemp then
		    maxTemp := t;
		end;
		return t;
	    end;
	end;
	(* Couldn't find one *)
	Writef(output,'AllocTemp: Too many temps?\n');
	return NULLTEMP;
	

    | TARGETTITAN, TARGETTITANM :
	(* Unlimited temporaries *)
	maxTemp := maxTemp + 1;
	return maxTemp;
    end;
end AllocTemp;

procedure FreeTemp(t : TempNumber);
begin
    if target = TARGETVAX then
	if not (t in inUseTemps) or (t in inUseTempsToBeFreed) then
	    ErrorNumber('FreeTemp: % not in use? ',t);
	end;
	incl(inUseTempsToBeFreed, t);
    end;
end FreeTemp;

procedure UpdateTemps;
var
    t : TempNumber;
begin
    if inUseTempsToBeFreed # TempSet{} then
	for t := 1 to NUMOPTTEMPS do
	    if t in inUseTempsToBeFreed then
		if optLoopNest <= tempLevel[t] then
		    excl(inUseTempsToBeFreed,t);
		    excl(inUseTemps, t);
		end;
	    end;
	end;
    end;
end UpdateTemps;

procedure ReduceExprNeededCounts(en : ExprNode; count : integer;
	remove : boolean);
var
    on, ron : OptNode;
begin
    on := en^.opt;
    on^.removed := on^.removed or remove;
    ron := on^.rootEqual;
    if DEBUG and TraceCount then
	Writef(output,'Reduce: %d/%d %d-%d %n\n', on^.uniqueId,
	    ron^.uniqueId, ron^.neededCount, count, remove);
    end;
    ron^.neededCount := ron^.neededCount - count;
    ron^.referencedCount := ron^.referencedCount - count;
    if (ron^.tempNumber # NULLTEMP) or (ron^.inductionVar # nil) then
	(* already subexpression or induction var, children already reduced *)
    else
	ReduceNeededCounts(on,count,remove);
    end;
end ReduceExprNeededCounts;

(* reduce use counts in all expression nodes used by this expression *)
procedure ReduceNeededCounts(on : OptNode; count : integer; remove : boolean);
var
    en, pen, den : ExprNode;
    esn : ExprSetNode;
begin
    on^.removed := on^.removed or remove;
    en := on^.expr;
    case en^.kind of
    | EXPRCONST,
      EXPRSYM, 
      EXPRVAR    : (* no descendents *)
    | EXPRNAME,
      EXPRINLINE :
	ExprError(en,'ReduceNeededCounts: unexpected en')
    | EXPRUNOP   :
	ReduceExprNeededCounts(en^.opnd,count,remove);
    
    | EXPRBINOP  :
	ReduceExprNeededCounts(en^.opnd1,count,remove);
	ReduceExprNeededCounts(en^.opnd2,count,remove);
    
    | EXPRFUNC   :
	ReduceExprNeededCounts(en^.func,count,remove);
	pen := en^.params^.first;
	while pen # nil do
	    ReduceExprNeededCounts(pen,count,remove);
	    pen := pen^.next;
	end;
	
    | EXPRVAL :
	ReduceExprNeededCounts(en^.exprVal,count,remove);
    
    | EXPRCHECK :
	ReduceExprNeededCounts(en^.checkExpr,count,remove);
    
    | EXPRSET :
	if en^.setExpr # nil then
	    esn := en^.setExpr^.first;
	    while esn # nil do
		ReduceExprNeededCounts(esn^.lower,count,remove);
		if esn^.upper # nil then
		    ReduceExprNeededCounts(esn^.upper,count,remove);
		end;
		esn := esn^.next;
	    end;
	end;
    
    | EXPRDESCRIPTOR :
	ReduceExprNeededCounts(en^.descripBase, count, remove);
	den := en^.descrips^.first;
	while den # nil do
	    ReduceExprNeededCounts(den, count, remove);
	    den := den^.next;
	end;

    | EXPRSAVE :
	ReduceExprNeededCounts(en^.exprSave,count,remove);
    end;
end ReduceNeededCounts;

procedure CommonSubExpr(en : ExprNode; mode : EvalMode; state : EvalState);
var
    on, ron, cron : OptNode;
begin
    on := en^.opt;
    ron := on^.rootEqual;
    if not ron^.eligible and not ron^.ineligible and (ron^.neededCount > 1)
	    and (ron^.cost in OptCostSet{COSTCSE,COSTCONTAINEDCSE}) then
	(* got a possible subexpression *)
	if (SizeOf(en^.exprType) > WORDSIZE)
	    (* |||  and (en^.exprType^.kind # DTLONGREAL) *) then
	    (* cannot handle multiple word sub expression *)
	elsif ron^.cost = COSTCSE then
	    ron^.eligible := true;
	else
	    (* must contain a non-trivial expression *)
	    cron := ron^.containedNonTrivial^.rootEqual;
	    (* see if contained expression is needed only here *)
	    if cron^.neededCount = ron^.neededCount then
		(* this is the only use of the nonTrivial expression *)
		(* make this be the subexpression *)
		ron^.eligible := true;
	    end;
	end;
	ron^.ineligible := not ron^.eligible;
    end;
    if ron^.inductionVar # nil then
	ron^.usedCount := ron^.usedCount + 1;
	if ron^.usedCount > ron^.neededCount then
	    ExprError(on^.expr,'CommonSubExpr: used > needed 4?');
	    PrintOptExpr(on);
	end;
	(* don't reference on post *)
	if state # EVALPOST then
	    (* induction variable, use index instead of expression *)
	    CountVar(ron^.inductionVar,en,1);
	    on^.usage := OUSEINDUCTION;
	end;
    elsif ron^.eligible then
	if ron^.tempNumber # NULLTEMP then
	    ron^.usedCount := ron^.usedCount + 1;
	    if ron^.usedCount > ron^.neededCount then
		ExprError(on^.expr,'CommonSubExpr: used > needed 1?');
		PrintOptExpr(on);
	    elsif state = EVALPRE then
		(* already calculated *)
	    elsif state = EVALPOST then
	        if ron^.usedCount = ron^.neededCount then
		    if DEBUG and TraceOptim then
			Writef(output,'CSE: %d %n %n freetemp=%d ',
			    on^.uniqueId, on^.usage, state, ron^.tempNumber);
			WriteExpr(on^.expr);
			Writec(output, '\n');
		    end;
		    (* finished with saved value *)
		    FreeTemp(ron^.tempNumber);
		end;
	    else
		(* reuse saved value *)
		on^.usage := OUSEAFTERFIRST;
		if ron^.tempNumber <= NUMTEMPSTORAGE then
		    inc(refCount[ron^.tempNumber].count, on^.loopNest);
		end;
		if DEBUG and TraceOptim then
		    Writef(output,'CSE: %d %n %n temp=%d ',
			    on^.uniqueId, on^.usage, state, ron^.tempNumber);
		    WriteExpr(on^.expr);
		    Writec(output, '\n');
		end;
		if ron^.usedCount = ron^.neededCount then
		    FreeTemp(ron^.tempNumber);
		end;
	    end;
(* ||| Bad Experiment
	This stuff messes up because of moves out of loops.  What
	we really want to do is make tempNumber into a live expression, and
	substitute the checked expression itself if trivial.

	elsif (ron^.expr^.kind = EXPRCHECK) then
	    (* All runtime checks are identity transformations.  Rather than
	       allocating a temp, just turn off checking if not the
	       first time through. *)
	    if ron^.usedCount > 0 then
		on^.expr^.doCheck := false;
		on^.expr^.doPtrAssignCheck := false;
	    end;
	    DoCountExpr(en, mode);
	    inc(ron^.usedCount);
	    if ron^.usedCount > ron^.neededCount then
		ExprError(on^.expr, 'CommonSubExpr: use > need CHECKEXPR?');
		PrintOptExpr(on);
	    end;
||| End Bad Experiment *)
	else
	    (* allocate temp to save it in *)
	    ron^.tempNumber := AllocTemp();
	    if ron^.tempNumber = NULLTEMP then
		(* couldn't allocate a temp *)
		ron^.eligible := false;	(* can't make it a cse *)
		ron^.ineligible := true;
		DoCountExpr(en, mode);
		ron^.usedCount := ron^.usedCount + 1;
		if ron^.usedCount > ron^.neededCount then
		    ExprError(on^.expr,'CommonSubExpr: used > needed 3?');
		    PrintOptExpr(on);
		end;
	    else
		(* will save a value, make it look like it was used *)
		if DEBUG and TraceOptim then
		    Writef(output, 'Reduce: rc=%d, nc=%d, uc=%d, temp=%d, en=',
			    ron^.referencedCount, ron^.neededCount,
			    ron^.usedCount, ron^.tempNumber);
		    WriteExpr(on^.expr);
		    Writec(output, '\n');
		end;
		ReduceNeededCounts(ron,ron^.referencedCount-1,false);
		(* evaluate expression *)
		DoCountExpr(en, mode);
		if state = EVALPRE then
		    on^.usage := OUSEGENERATE;
		else
		    on^.usage := OUSEFIRST;
		end;
		if ron^.tempNumber <= NUMTEMPSTORAGE then
		    inc(refCount[ron^.tempNumber].count, on^.loopNest);
		end;
		if DEBUG and TraceOptim then
		    Writef(output,'CSE: %d %n %n ', 
			    on^.uniqueId, on^.usage, state);
		    WriteExpr(on^.expr);
		    Writec(output, '\n');
		end;
		(* count this as a use, too *)
		ron^.usedCount := ron^.usedCount + 1;
		if ron^.usedCount > ron^.neededCount then
		    ExprError(on^.expr,'CommonSubExpr: used > needed 2?');
		    PrintOptExpr(on);
		end;
	    end;
	end;
    else
	(* not subexpression, do normal evaluate *)
	DoCountExpr(en, mode);
	ron^.usedCount := ron^.usedCount + 1;
	if ron^.usedCount > ron^.neededCount then
	    ExprError(on^.expr,'CommonSubExpr: used > needed 5?');
	    PrintOptExpr(on);
	end;
    end;
end CommonSubExpr;

procedure InitCounts;
var
    index : integer;
begin
    InitTemps;
    for index := 1 to NUMTEMPSTORAGE do
	refCount[index].count := 0;
	refCount[index].offset := (index-1) * WORDSIZE;
	refCount[index].addressed := false;
    end;
end InitCounts;

procedure ComputeCounts(proc : ProcNode);
var
    rooton, classon, equalon, on : OptNode;
    vn : VarNode;
    index : integer;
    enclosing : ProcNode;
begin
    rooton := cseRootExpr[EXPRVAR];
    if rooton # nil then
	classon := rooton;
	repeat
	    vn := classon^.expr^.exprVar;
	    if vn^.address.kind = MEMGLOBAL then
		(* don't worry about globals *)
	    elsif (vn^.address.kind # MEMFAST) or (vn^.address.proc # proc) then
		(* doesn't qualify, but tally uplevel refs *)
		incl(proc^.doesUpLevel, vn^.address.level);
	    elsif vn^.address.offset >= NUMFASTWORDS*WORDSIZE then
		(* doesn't qualify, but tally uplevel refs *)
		incl(proc^.doesUpLevel, vn^.address.level); 
	    else
		(* candidate *)
		index := vn^.address.offset div WORDSIZE +
			    NUMOPTTEMPS + 1;
		if (proc^.displayLevel in proc^.containsUpLevel) and
 			(index <= NUMTEMPSTORAGE) then 
		    (* possible uplevel reference, assume worst *)
		    refCount[index].addressed := true;
		else
		    equalon := classon;
		    repeat
			if equalon^.rootEqual = equalon then
			    (* Experiment: just multiply by reference count 
			    on := equalon;
			    repeat
				inc(refCount[index].count, on^.loopNest);
				on := on^.nextEqual;
			    until on = equalon;
			    *)
			    if index <= NUMTEMPSTORAGE then
				inc(refCount[index].count, 
				  equalon^.loopNest * equalon^.referencedCount);
			    end;
			end;
			equalon := equalon^.nextCongruent;
		    until equalon = classon;
		end;
	    end;
	    classon := classon^.nextClass;
	until classon = rooton;
    end;
    enclosing := proc^.enclosing;
    while enclosing # nil do
	enclosing^.containsUpLevel := enclosing^.containsUpLevel +
		    proc^.doesUpLevel;
	enclosing := enclosing^.enclosing;
    end;
end ComputeCounts;

procedure SortArray(l, r : integer);
var
    i, j : integer;
    tmp : StorageCount;
begin
    for i := l to r do
	if refCount[i].addressed then
	    refCount[i].count := -1000000-refCount[i].count;
	end;
    end;
    for i := l to r-1 do
	for j := i+1 to r do
	    if refCount[i].count < refCount[j].count then
		tmp := refCount[i];
		refCount[i] := refCount[j];
		refCount[j] := tmp;
	    end;
	end;
    end;
end SortArray;

procedure ReassignStorage(proc : ProcNode);
var
    index, numwords, ireg : integer;
    tm : TempMapNode;
begin
    ComputeCounts(proc);

    case target of
    | TARGETVAX :
	numwords := (proc^.mem^.maximum[MEMFAST] + WORDSIZE - 1) div WORDSIZE;
	if numwords > NUMFASTWORDS then
	    numwords := NUMFASTWORDS;
	end;
	(* move T storage down next to opt temps *)
	for index := 1 to numwords do
	    refCount[maxTemp+index] := refCount[NUMOPTTEMPS+index];
	end;
	numwords := numwords + maxTemp;
	if DEBUG and TraceOptim then
	    Writef(output,'Reassign storage before O%d T%d\n', maxTemp,
			    numwords-maxTemp);
	    for index := 1 to numwords do
		Writef(output,'%3d%5d%5d %n\n', index,refCount[index].count,
			refCount[index].offset div WORDSIZE,
			refCount[index].addressed);
	    end;
	end;
	SortArray(1,numwords);
	if DEBUG and TraceOptim then
	    Writef(output,'Reassign storage after\n');
	    for index := 1 to numwords do
		Writef(output,'%3d%5d%5d %n\n', index,refCount[index].count,
			refCount[index].offset div WORDSIZE,
			refCount[index].addressed);
	    end;
	end;
	new(tm);
	tm^.numReg := -1;
	tm^.numOptTemp := maxTemp;
	for index := 0 to NUMTEMPSTORAGE-1 do
	    tm^.map[index] := -1;
	end;
	for index := 1 to numwords do
	    ireg := refCount[index].offset div WORDSIZE;
	    tm^.map[ireg] := (index-1) * WORDSIZE;
	    if (refCount[index].count >= MINCOUNTFORREG) and
		    not refCount[index].addressed and (index-1 > tm^.numReg)
	    then
		tm^.numReg := index-1;
	    end;
	end;
	proc^.mem^.maximum[MEMFAST] := proc^.mem^.maximum[MEMFAST]
			+ maxTemp*WORDSIZE;
	if DEBUG and TraceOptim then
	    Writef(output,'Reassign storage map %d\n', tm^.numReg);
	    for index := 0 to NUMTEMPSTORAGE-1 do
		if tm^.map[index] # -1 then
		    if index >= maxTemp then
			Writef(output, 'T%3d%5d', index,
			    tm^.map[index] div WORDSIZE);
		    else
			Writef(output,'O%3d%5d',index+1,
			    tm^.map[index] div WORDSIZE);
		    end;
		end;
	    end;
	end;
	proc^.tempMap := tm;
    
    | TARGETTITAN, TARGETTITANM :
	new(tm);
	tm^.numReg := maxTemp;
	proc^.tempMap := tm;
    end;
end ReassignStorage;

procedure CountProc (proc : ProcNode);
var
    code : CodeNode;
    t    : TempNumber;
    i    : integer;
    on   : OptNode;
begin
    if not proc^.inlineProc then
	InitCounts();
	optLoopNest := 1;
	code := proc^.code^.first;
	while code # nil do
	    CountStmtList(code^.stmts);
	    code := code^.next;
	end;
	UpdateTemps();
	if inUseTemps # TempSet{} then
	    ErrorName(proc^.name,'CountProc: temps in use after processing $?');
	    for t := 1 to NUMOPTTEMPS do
		if t in inUseTemps then
		    Writef(output,' %d', t);
		end;
	    end;
	    Writec(output, '\n');
	end;
	on := allExprs^.nextAll;
	while on # allExprs do
	    if (on^.rootEqual = on) and (on^.usedCount # on^.neededCount) and
		not on^.removed
	    then
		ExprError(on^.expr,'Count wrong after CountProc?');
		PrintOptExpr(on);
	    end;
	    on := on^.nextAll;
	end;
	ReassignStorage(proc);
    end;
end CountProc;

procedure CountStmtList(stmts : StmtList);
var
    stn : StmtNode;
begin
    if stmts # nil then
	stn := stmts^.first;
	while stn # nil do
	    CountStmt(stn);
	    stn := stn^.next;
	end;
    end;
end CountStmtList;

procedure CountStmtAssign(stn : StmtNode);
var
    done, gotit : boolean;
    lhson, rhson, valon, varon : OptNode;
    tmpen, rhs  : ExprNode;
    size	: MemoryOffset;
begin
    if stn^.assignSizeCheck # nil then
	CountExpr(stn^.assignSizeCheck,EVALGET);
    end;
    done := false;
    gotit := false;
    rhs := stn^.rhs;
    size := SizeOf(stn^.lhsType);
    if (rhs^.kind = EXPRBINOP) and
	    (rhs^.exprBinOp in TokenSet{TKPLUS, TKMINUS, TKASTERISK}) and
	    (rhs^.operType^.kind in 
		DataTypeSet{DTPOINTER,DTINTEGER,DTCARDINAL,DTREAL,DTLONGREAL})
	    and ((size = WORDSIZE) or
(* ||| Still need to do MP2 in xlate for these
		((size = HALFSIZE) and (target = TARGETVAX)) or
		((size = BYTESIZE) and (target = TARGETVAX)) or
*)
		((size = LONGREALSIZE) and (stn^.lhsType^.kind = DTLONGREAL)))
    then
	lhson := stn^.lhs^.opt;
	rhson := rhs^.opt^.rootEqual;
	if (rhs^.opnd1^.kind = EXPRVAL) and 
		(SizeOf(rhs^.opnd1^.exprType) = size) then
	    valon := rhs^.opnd1^.opt^.rootEqual;
	    varon := rhs^.opnd1^.exprVal^.opt;
	    if lhson^.rootEqual = varon^.rootEqual then
		(* matched lhs and opnd1, make sure eligible *)
		gotit := not rhson^.eligible and not valon^.eligible;
	    end;
	end;
	if not gotit and (rhs^.exprBinOp in TokenSet{TKPLUS,TKASTERISK}) and
		(rhs^.opnd2^.kind = EXPRVAL) and 
		(SizeOf(rhs^.opnd2^.exprType) = size) then
	    (* check opnd2 of commutative operation *)
	    valon := rhs^.opnd2^.opt^.rootEqual;
	    varon := rhs^.opnd2^.exprVal^.opt;
	    if (lhson^.rootEqual = varon^.rootEqual) and
		not rhson^.eligible and not valon^.eligible
	    then
		(* opnd2 matches and is OK *)
		gotit := true;
		(* switch operands *)
		tmpen := rhs^.opnd1;
		rhs^.opnd1 := rhs^.opnd2;
		rhs^.opnd2 := tmpen;
	    end;
	end;
	if gotit then
	    (* a := a op b *)
	    stn^.assignOp := rhs^.exprBinOp;
	    (* don't need to evaluate lhs *)
	    ReduceExprNeededCounts(stn^.lhs,1,true);
	    (* don't need val of var or op *)
	    (* ||| Aren't these two statements unnecessary ? *)
	    valon := valon^.rootEqual;
	    rhson := rhson^.rootEqual;
	    valon^.neededCount := valon^.neededCount - 1;
	    valon^.referencedCount := valon^.referencedCount - 1;
	    rhson^.neededCount := rhson^.neededCount - 1;
	    rhson^.referencedCount := rhson^.referencedCount - 1;
	    CountExpr(rhs^.opnd1^.exprVal,EVALGET);
	    CountExpr(rhs^.opnd2,EVALGET);
	    done := true;
	end;
    end;
    if not done then
	CountExpr(rhs,EVALGET);
	CountExpr(stn^.lhs,EVALPUT);
    end;
end CountStmtAssign;

procedure CountParamList(procType : TypeNode; params : ExprList);
var
    pn      : ParamNode;
    pen     : ExprNode;
    i       : cardinal;
begin
    if (params = nil) or (procType^.paramList = nil) then
	(* do nothing *)
    else
	pen := params^.first;
	pn := procType^.paramList^.first;
	while pen # nil do
	    case pn^.kind of
	    | PARAMVALUE,PARAMCONST :
		if pn^.reference then
		    CountExpr(pen,EVALPOINT);
		else
		    CountExpr(pen,EVALGET);
		end;
	    
	    | PARAMVAR :
		CountExpr(pen,EVALPOINT);
	    
	    | PARAMARRAYVALUE, PARAMARRAYVAR, PARAMARRAYCONST :
		assert(pen^.kind = EXPRDESCRIPTOR, 'CountParamList not desc?');
		CountExpr(pen, EVALPOINT);
	    end (* case *);
	    pen := pen^.next;
	    pn := pn^.next;
	end;
    end;
end CountParamList;

procedure CountFuncProc(procExpr : ExprNode;
			params   : ExprList;
			selected : boolean;
			mode     : EvalMode);
var
    proc : ProcNode;
begin
    CountExpr(procExpr,EVALGET);
    if (procExpr^.kind = EXPRSYM) and (procExpr^.exprSym^.kind = SYMTYPE) then
	if selected then
	    CountExpr(params^.first, EVALPOINT);
	else
	    CountExpr(params^.first, mode);
	end;
    elsif procExpr^.kind # EXPRCONST then
	CountParamList(procExpr^.exprType,params);
    else
	proc := procExpr^.exprConst^.procVal;
	if proc^.builtin # BIPNOTBIP then
	    CountBuiltin(proc,params);
	else
	    CountParamList(proc^.procType,params);
	end;
    end;
end CountFuncProc;

procedure CountStmtIf(stn : StmtNode);
begin
    CountExpr(stn^.ifCond,EVALGET);
    CountStmtList(stn^.thenList);
    CountStmtList(stn^.elseList);
end CountStmtIf;

procedure CountStmtCase(stn : StmtNode);
var
    caseNode : CaseNode;
begin
    CountExpr(stn^.caseSel,EVALGET);
    if stn^.cases # nil then
	caseNode := stn^.cases^.first;
	while caseNode # nil do
	    CountStmtList(caseNode^.stmts);
	    caseNode := caseNode^.next;
	end;
    end;
    if stn^.caseElse # nil then
	CountStmtList(stn^.caseElse);
    end;
end CountStmtCase;

procedure CountPrePostEval(el : ExprList;state : EvalState);
var
    en : ExprNode;
begin
    if el # nil then
	en := el^.first;
	while en # nil do
	    CommonSubExpr(en,EVALGET,state);
	    en := en^.next;
	end;
    end;
end CountPrePostEval;

procedure CountStmtWhile(stn : StmtNode);
begin
    CountExpr(stn^.whileCond,EVALGET);
    CountPrePostEval(stn^.whilePreEval,EVALPRE);
    optLoopNest := optLoopNest + 1;
    CountStmtList(stn^.whileBody);
    optLoopNest := optLoopNest - 1;
    CountPrePostEval(stn^.whilePreEval,EVALPOST);
end CountStmtWhile;

procedure CountStmtRepeat(stn : StmtNode);
begin
    CountPrePostEval(stn^.repeatPreEval,EVALPRE);
    optLoopNest := optLoopNest + 1;
    CountStmtList(stn^.repeatBody);
    optLoopNest := optLoopNest - 1;
    CountExpr(stn^.repeatCond,EVALGET);
    CountPrePostEval(stn^.repeatPreEval,EVALPOST);
end CountStmtRepeat;

procedure CountStmtLoop(stn : StmtNode);
begin
    CountPrePostEval(stn^.loopPreEval,EVALPRE);
    optLoopNest := optLoopNest + 1;
    CountStmtList(stn^.loopBody);
    optLoopNest := optLoopNest - 1;
    CountPrePostEval(stn^.loopPreEval,EVALPOST);
end CountStmtLoop;

procedure CountStmtFor(stn : StmtNode);
begin
    CountExpr(stn^.forTo,EVALGET);
    if stn^.forTo^.kind # EXPRCONST then
	CountVar(stn^.forLimitVar,stn^.forTo,1);
    end;
    CountExpr(stn^.forFrom,EVALGET);
    CountVar(stn^.forIndexVar,stn^.forFrom,1);
    if stn^.forBy^.kind # EXPRCONST then
	(* increment variable is used only if variable increment *)
	CountExpr(stn^.forBy,EVALGET);
	CountVar(stn^.forIncVar,stn^.forBy,1);
    else
	(* eliminate increment expression *)
	ReduceExprNeededCounts(stn^.forBy,1,true);
    end;
    if stn^.forLimitCheck # nil then
	CountExpr(stn^.forLimitCheck,EVALGET);
    end;

    CountPrePostEval(stn^.forPreEval,EVALPRE);
    optLoopNest := optLoopNest + 1;
    CountStmtList(stn^.forBody);
    optLoopNest := optLoopNest - 1;
    CountVar(stn^.forIndexVar,stn^.forFrom,LOOPBIAS);
    if stn^.forTo^.kind # EXPRCONST then
	CountVar(stn^.forLimitVar,stn^.forTo,LOOPBIAS);
    end;
    if stn^.forBy^.kind # EXPRCONST then
	(* increment variable is used only if variable increment *)
	CountVar(stn^.forIncVar,stn^.forBy,LOOPBIAS);
    end;
    CountPrePostEval(stn^.forPreEval,EVALPOST);
end CountStmtFor;

procedure CountStmtWith(stn : StmtNode);
begin
    CountExpr(stn^.withQual,EVALPOINT);
    CountVar(stn^.withPtrVar,stn^.withQual,1);
    optLoopNest := optLoopNest + 1;
    CountStmtList(stn^.withBody);
    optLoopNest := optLoopNest - 1;
end CountStmtWith;

procedure CountStmtReturn(stn : StmtNode);
begin
    if stn^.returnVal # nil then
	CountExpr(stn^.returnVal, EVALGET);
	if stn^.inlineVarExpr # nil then
	    CountExpr(stn^.inlineVarExpr, EVALPUT);
	end;
    end;
end CountStmtReturn;


procedure CountInline(inlineParams  : ExprList;
		      inlineFormals : InlineParamList;
		      paramList     : ParamList);
var
    ipn      : InlineParamNode;
    pen      : ExprNode;
    pn       : ParamNode;
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
		case pn^.kind of
		| PARAMVAR, PARAMVALUE, PARAMCONST :
		    if pn^.reference then
			CountExpr(pen,EVALPOINT);
		    else
			CountExpr(pen,EVALGET);
		    end;
		    CountVar(ipn^.formal,pen,1);
		
		| PARAMARRAYVAR,PARAMARRAYVALUE, PARAMARRAYCONST :
		    CountExpr(pen, EVALPOINT);
(* ||| old code incremented only for count; not stride and not array base.
   This incs for all.  Why doesn't it just do CountVar(..., 1)? *)
		    CountVar(ipn^.formal, pen, pen^.descripCount+1);
		end (* case *);
	    end (* if *);
	    pen := pen^.next;
	    pn := pn^.next;
	    ipn := ipn^.next;
	end;
    end;
end CountInline;

procedure CountStmtInline(stn : StmtNode);
begin
    CountInline(stn^.inlineParams, stn^.inlineFormals,
	stn^.inlineProc^.procType^.paramList);
    CountStmtList(stn^.inlineBody);
end CountStmtInline;

procedure CountStmt(stn : StmtNode);
begin
    case stn^.kind of
    | STMTNONE      :   (* nothing *);
    | STMTASSIGN    :	CountStmtAssign(stn);
    | STMTPROC      :	CountFuncProc(stn^.proc, stn^.params, false, EVALGET);
    | STMTIF	    :	CountStmtIf(stn);
    | STMTCASE      :	CountStmtCase(stn);
    | STMTWHILE     :	CountStmtWhile(stn);
    | STMTREPEAT    :	CountStmtRepeat(stn);
    | STMTLOOP      :	CountStmtLoop(stn);
    | STMTFOR       :	CountStmtFor(stn);
    | STMTWITH      :	CountStmtWith(stn);
    | STMTRETURN    :	CountStmtReturn(stn);
    | STMTEXIT      :	(* nothing *);
    | STMTINLINE    :	CountStmtInline(stn);
    | STMTSTMTS     :	CountStmtList(stn^.stmts);
$if pascal then
    | STMTGOTO      :   (* nothing *);
    | STMTLABEL     :   (* nothing *);
$end
    end;
    UpdateTemps;
end CountStmt;

procedure CountVar(vn : VarNode; en : ExprNode; weight : integer);
var
    on : OptNode;
    index : integer;
begin
    if vn^.address.kind = MEMFAST then
	index := vn^.address.offset div WORDSIZE + NUMOPTTEMPS+1;
	if index <= NUMTEMPSTORAGE then
	    on := en^.opt;
	    inc(refCount[index].count,
			on^.loopNest * on^.referencedCount * weight);
	end;
    end;
end CountVar;

procedure CheckVarRef(vn : VarNode; mode : EvalMode);
var
    index : integer;
begin
    if (mode = EVALPOINT) and (vn^.address.kind = MEMFAST) then
	if DEBUG and TraceOptim then
	    Writef(output,'Addressed ');
	    WriteString(output,vn^.name);
	    Writec(output, '\n');
	end;
	index := vn^.address.offset div WORDSIZE + NUMOPTTEMPS+1;
	if index <= NUMTEMPSTORAGE then
	    refCount[index].addressed := true;
	end;
    end;
end CheckVarRef;

procedure CountExprInline(en : ExprNode);
begin
    CountInline(en^.inlineParams, en^.inlineFormals,
	en^.inlineProc^.procType^.paramList);
    CountStmtList(en^.inlineBody);
    CountExpr(en^.inlineResult, EVALGET);
end CountExprInline;

procedure CountExprSet(en : ExprNode; mode : EvalMode);
var
    esn : ExprSetNode;
begin
    if en^.setExpr # nil then
	esn := en^.setExpr^.first;
	while esn # nil do
	    CountExpr(esn^.lower,mode);
	    if esn^.upper # nil then
		CountExpr(esn^.upper,mode);
	    end;
	    esn := esn^.next;
	end;
    end;
end CountExprSet;

procedure CountExprDescriptor(en : ExprNode);
var
    den : ExprNode;
begin
    CountExpr(en^.descripBase, en^.descripMode);
    den := en^.descrips^.first;
    while den # nil do
	CountExpr(den, EVALGET);
	den := den^.next;
    end;
end CountExprDescriptor;

procedure CountExprVal(en : ExprNode; mode : EvalMode);
begin
    if en^.exprVal^.kind = EXPRVAR then
	CountExpr(en^.exprVal, EVALGET);
    else
	CountExpr(en^.exprVal, mode);
    end;
end CountExprVal;

procedure DoCountExpr(en : ExprNode; mode : EvalMode);
begin
    case en^.kind of
    | EXPRBAD       :
    | EXPRSYM       : 
    | EXPRCONST     :
    | EXPRVAR       : CheckVarRef(en^.exprVar, mode);
    | EXPRUNOP      : CountExpr(en^.opnd, mode);
    | EXPRBINOP     : CountExpr(en^.opnd1, mode);
		      CountExpr(en^.opnd2, mode);
    | EXPRFUNC      : CountFuncProc(en^.func, en^.params, en^.selected, mode);
    | EXPRVAL       : CountExprVal(en, mode);
    | EXPRCHECK     : CountExpr(en^.checkExpr, mode);
    | EXPRSAVE      : CountExpr(en^.exprSave, mode);
    | EXPRSET       : CountExprSet(en, EVALGET);
    | EXPRDESCRIPTOR: CountExprDescriptor(en);
    | EXPRINLINE    : CountExprInline(en);
    end;
end DoCountExpr;

procedure CountExpr(en : ExprNode; mode : EvalMode);
begin
    if en = nil then
	ExprError(en,'CountExpr: nil expression?');
    else
	CommonSubExpr(en, mode, EVALNORMAL);
    end;
end CountExpr;

end OCount.
