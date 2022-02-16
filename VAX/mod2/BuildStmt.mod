implementation module BuildStmt;

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

from Tokens import
    Token, stringToken;

from Globals import
    TraceNstmt, DEBUG, INDENT;

from Strings import
    String, WriteString;

from Symbols import
    StmtKind, StmtNode, StmtList, ExprNode, ExprList, CaseNode,
    ExprSetNode, ExprSetList, currLine, currFile;

from BuildExpr import
    Indent, PrintExpr;

$if pascal then
from Strings import NewText;
from io import SWritef;
from Symbols import ConstNode;
$end


procedure NewStmtNode(const kind : StmtKind) : StmtNode;
(* Create a StmtNode and fill in fixed fields (except next). *)
var
    stn : StmtNode;
begin
    new(stn);
    stn^.kind := kind;
    stn^.bad := false;
    stn^.fileName := currFile;
    stn^.lineNumber := currLine;
    return stn;
end NewStmtNode;

procedure SameStmtLine(const stn : StmtNode; const en : ExprNode);
begin
    if en # nil then
	stn^.fileName := en^.fileName;
	stn^.lineNumber := en^.lineNumber;
    end;
end SameStmtLine;

(* grammar+ *) procedure BuildStmtAssign(const lhs, rhs : ExprNode) : StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtAssign\n');
    end;
    stn := NewStmtNode(STMTASSIGN);
    stn^.lhs := lhs;
    stn^.rhs := rhs;
    stn^.assignOp := TKASSIGN;  (* may be changed later to + * - div / *)
    stn^.assignSizeCheck := nil;
    SameStmtLine(stn,lhs);
    return stn;
end BuildStmtAssign;

(* grammar+ *) procedure BuildStmtProc(const proc   : ExprNode; 
				      const params : ExprList) 
	: StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtProc\n');
    end;
    stn := NewStmtNode(STMTPROC);
    stn^.proc := proc;
    stn^.params := params;
    SameStmtLine(stn,proc);
    return stn;
end BuildStmtProc;

(* grammar *) procedure BuildStmtIf(const cond     : ExprNode; 
				    const thenList : StmtList;
				    const elseList : StmtList) 
	: StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtIf\n');
    end;
    stn := NewStmtNode(STMTIF);
    stn^.ifCond := cond;
    stn^.thenList := thenList;
    stn^.elseList := elseList;
    SameStmtLine(stn,cond);
    return stn;
end BuildStmtIf;

(* grammar *) procedure AddCase(      list   : CaseList; 
			        const labels : ExprSetList; 
				const stmts  : StmtList) : CaseList;
var
    cn : CaseNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'AddCase\n');
    end;
    new(cn);
    cn^.labels := labels;
    cn^.stmts := stmts;
    cn^.next := nil;
    if list = nil then
	new(list);
	list^.first := cn;
    else
	list^.last^.next := cn;
    end;
    list^.last := cn;
    return list;
end AddCase;

(* grammar *) procedure BuildStmtCase(const caseSel  : ExprNode; 
				      const caseList : CaseList;
				      const caseElse : StmtList) : StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtCase\n');
    end;
    stn := NewStmtNode(STMTCASE);
    stn^.caseSel := caseSel;
    stn^.cases := caseList;
    stn^.caseTree := nil;
    stn^.caseElse := caseElse;
    SameStmtLine(stn,caseSel);
    return stn;
end BuildStmtCase;

(* grammar *) procedure BuildStmtWhile(const cond      : ExprNode; 
				       const whileBody : StmtList) : StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtWhile\n');
    end;
    stn := NewStmtNode(STMTWHILE);
    stn^.whileCond := cond;
    stn^.whileBody := whileBody;
    stn^.whilePreEval := nil;
    SameStmtLine(stn,cond);
    return stn;
end BuildStmtWhile;


(* grammar *) procedure StartStmtRepeat (): StmtNode;
(* All we really care about is getting position in file right at this point *)
var
    stn : StmtNode;
begin
    stn := NewStmtNode(STMTREPEAT);
    stn^.repeatBody := nil;	    (* Just in case syntax error results in *)
    stn^.repeatCond := nil;	    (* no closing call to RepeatStmtNode    *)
    stn^.repeatPreEval := nil;
    return stn;
end StartStmtRepeat;

(* grammar *) procedure BuildStmtRepeat(const stn        : StmtNode; 
					const repeatBody : StmtList; 
					const cond       : ExprNode) : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtRepeat\n');
    end;
    stn^.repeatBody := repeatBody;
    stn^.repeatCond := cond;
    return stn;
end BuildStmtRepeat;

(* grammar *) procedure BuildStmtFor(const index   : String; 
				     const forFrom : ExprNode;
				     const forTo   : ExprNode;
				     const forBy   : ExprNode; 
				     const forBody : StmtList) : StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtFor\n');
    end;
    stn := NewStmtNode(STMTFOR);
    stn^.forIndexName := index;
    stn^.forFrom := forFrom;
    stn^.forTo := forTo;
    stn^.forBy := forBy;
    stn^.forBody := forBody;
    stn^.forPreEval := nil;
    stn^.forLimitCheck := nil;
    SameStmtLine(stn, forFrom);
    return stn;
end BuildStmtFor;

(* grammar *) procedure BuildStmtWith(const withQual : ExprNode; 
				      const withBody : StmtList) : StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtWith\n');
    end;
    stn := NewStmtNode(STMTWITH);
    stn^.withQual := withQual;
    stn^.withBody := withBody;
    SameStmtLine(stn, withQual);
    return stn;
end BuildStmtWith;

$if modula2 then
(* grammar *) procedure StartStmtLoop (): StmtNode;
(* All we really care about is getting position in file right at this point *)
var
    stn : StmtNode;
begin
    stn := NewStmtNode(STMTLOOP);
    stn^.loopPreEval := nil;
    stn^.loopBody := nil;
    return stn;
end StartStmtLoop;

(* grammar *) procedure BuildStmtLoop(const stn      : StmtNode;
                                      const loopBody : StmtList) : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtLoop\n');
    end;
    stn^.loopBody := loopBody;
    return stn;
end BuildStmtLoop;

(* grammar *) procedure BuildStmtExit(const exitKind : Token) : StmtNode;
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtExit\n');
    end;
    stn := NewStmtNode(STMTEXIT);
    case exitKind of
    | TKLOOP    :   stn^.exitKind := STMTLOOP;
    | TKFOR     :   stn^.exitKind := STMTFOR;
    | TKWHILE   :   stn^.exitKind := STMTWHILE;
    | TKREPEAT  :   stn^.exitKind := STMTREPEAT;
    end;
    return stn;
end BuildStmtExit;

(* grammar *) procedure BuildStmtReturn(const returnVal : ExprNode) : StmtNode;
(* returnVal = nil means no expression specified *)
var
    stn : StmtNode;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtReturn\n');
    end;
    stn := NewStmtNode(STMTRETURN);
    stn^.returnVal := returnVal;
    return stn;
end BuildStmtReturn;

$else (* pascal *)

(* grammar *) procedure BuildStmtLabel(const labelNumber : ConstNode) : StmtNode;
    var stn     : StmtNode;
	rawName : array [0..15] of char;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtLabel\n');
    end;
    stn := NewStmtNode(STMTLABEL);
    new(stn^.label);
    SWritef(rawName, '%d', trunc(labelNumber^.cardVal));
    stn^.label^.labelName := NewText(rawName);
    return stn;
end BuildStmtLabel;

(* grammar *) procedure BuildStmtGoto(const labelNumber : ConstNode) : StmtNode;
    var stn : StmtNode;
        rawName : array [0..15] of char;
begin
    if DEBUG and TraceNstmt then
	Writef(output,'BuildStmtGoto\n');
    end;
    stn := NewStmtNode(STMTGOTO);
    new(stn^.targetLabel);
    SWritef(rawName, '%d', trunc(labelNumber^.cardVal));
    stn^.targetLabel^.labelName := NewText(rawName);
    return stn;
end BuildStmtGoto;

(* grammar *) procedure BuildStmtStmts(const stmts : StmtList) : StmtNode;
    var stn     : StmtNode;
begin
    stn := NewStmtNode(STMTSTMTS);
    stn^.stmts := stmts;
    return stn;
end BuildStmtStmts;

procedure AppendStmtList(const some, more : StmtList) : StmtList;
begin
    if (some = nil) or (some^.first = nil) then
	return more;
    elsif (more = nil) or (more^.first = nil) then
	(* nothing to add *)
    else
	some^.last^.next := more^.first;
	some^.last := more^.last;
    end;
    return some;
end AppendStmtList;

$end (* pascal *)


(* grammar+ *) procedure AddToStmtList (      list   : StmtList; 
				        const newOne : StmtNode) : StmtList;
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
end AddToStmtList;

(* debug *) procedure PrintStmt(const stn : StmtNode; const indent : integer);
var
    ten : ExprNode;
    casen : CaseNode;
    esn : ExprSetNode;
begin
    if stn = nil then
	Indent(output,indent);
	Writef(output,'Statement EMPTY\n');
    else
	Indent(output,indent);
	Writef(output,'Statement %n line %d\n', stn^.kind, stn^.lineNumber);
	case stn^.kind of
	| STMTASSIGN:
	    PrintExpr(stn^.lhs,indent+INDENT);
	    Indent(output,indent);
	    if stn^.assignOp = TKASSIGN then
		Writef(output, ':=\n');
	    else
		Writef(output, ':=');
		WriteString(output, stringToken[stn^.assignOp]);
		Writec(output, '\n');
	    end;
	    PrintExpr(stn^.rhs,indent+INDENT);
	
	| STMTPROC :
	    PrintExpr(stn^.proc,indent+INDENT);
	    if stn^.params # nil then
		ten := stn^.params^.first;
		while ten # nil do
		    Indent(output,indent);
		    Writef(output,'****\n');
		    PrintExpr(ten,indent+INDENT);
		    ten := ten^.next;
		end;
	    end;
	
	| STMTIF:
	    PrintExpr(stn^.ifCond,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'then\n');
	    PrintStmtList(stn^.thenList,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'else\n');
	    PrintStmtList(stn^.elseList,indent+INDENT);
	
	| STMTWHILE:
	    PrintExpr(stn^.whileCond,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'do\n');
	    PrintStmtList(stn^.whileBody,indent+INDENT);
	
	| STMTREPEAT:
	    PrintStmtList(stn^.repeatBody,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'until\n');
	    PrintExpr(stn^.repeatCond,indent+INDENT);
	
	| STMTLOOP:
	    PrintStmtList(stn^.loopBody,indent+INDENT);
	
	| STMTFOR:
	    Indent(output,indent);
	    Writef(output,'index ');
	    WriteString(output,stn^.forIndexName);
	    Writec(output, '\n');
	    Indent(output,indent);
	    Writef(output,'from\n');
	    PrintExpr(stn^.forFrom,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'to\n');
	    PrintExpr(stn^.forTo,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'by\n');
	    PrintExpr(stn^.forBy,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'do\n');
	    PrintStmtList(stn^.forBody,indent+INDENT);
	
	| STMTWITH:
	    PrintExpr(stn^.withQual,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,'do\n');
	    PrintStmtList(stn^.withBody,indent+INDENT);
	
	| STMTRETURN:
	    PrintExpr(stn^.returnVal,indent+INDENT);
	    if stn^.inlineExpr # nil then
		Indent(output,indent);
		Writef(output,'Inline expr\n');
	    end;
	    if stn^.inlineStmt # nil then
		Indent(output,indent);
		Writef(output,'Inline stmt\n');
	    end;
	    if stn^.inlineVarExpr # nil then
	    	Indent(output, indent);
		Writef(output, 'Inline return\n');
		PrintExpr(stn^.inlineVarExpr, indent+INDENT);
	    end;

	| STMTCASE :
	    PrintExpr(stn^.caseSel,indent+INDENT);
	    if stn^.cases # nil then
		casen := stn^.cases^.first;
		while casen# nil do
		    if casen^.labels # nil then
			esn := casen^.labels^.first;
			while esn # nil do
			    PrintExpr(esn^.lower,indent);
			    if esn^.upper # nil then
				Indent(output,indent);
				Writef(output,'..\n');
				PrintExpr(esn^.upper,indent);
			    end;
			    esn := esn^.next;
			end;
			PrintStmtList(casen^.stmts,indent+INDENT);
		    end;
		    casen := casen^.next;
		end;
	    end;
	    if stn^.caseElse # nil then
		Indent(output,indent);
		Writef(output,'else\n');
		PrintStmtList(stn^.caseElse,indent+INDENT);
	    end;
	
	| STMTEXIT :
	    Indent(output, indent);
	    Writef(output, '%n\n', stn^.exitKind);

	| STMTINLINE :
	    Indent(output,indent);
	    WriteString(output,stn^.inlineProc^.name);
	    Writec(output, '\n');
	    if stn^.inlineParams # nil then
		ten := stn^.inlineParams^.first;
		while ten # nil do
		    Indent(output,indent);
		    Writef(output,'****\n');
		    PrintExpr(ten,indent+INDENT);
		    ten := ten^.next;
		end;
	    end;
	    Indent(output,indent);
	    Writef(output,'body\n');
	    PrintStmtList(stn^.inlineBody,indent+INDENT);

	| STMTSTMTS :
	    Writec(output, '\n');
	    PrintStmtList(stn^.stmts,indent+INDENT);

	end (* case *);
    end;
end PrintStmt;

(* debug *) procedure PrintStmtList(const stl : StmtList; 
				    const indent : integer);
var
    stn : StmtNode;
begin
    if stl = nil then
	Indent(output, indent);
	Writef(output,'NIL Statement list\n');
    else
	stn := stl^.first;
	while stn # nil do
	    PrintStmt(stn,indent+INDENT);
	    stn := stn^.next;
	end;
    end;
end PrintStmtList;

end BuildStmt.
