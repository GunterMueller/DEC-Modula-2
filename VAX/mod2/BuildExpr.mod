implementation module BuildExpr;

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
    File, Writef, Writec, output;

from MemLib import
    ALLOCATE;

from Tokens import
    Token, stringToken;

from Globals import
    genCheckFlag, genPtrAssignCheckFlag, TraceNexpr, DEBUG, INDENT;

from Strings import
    String, WriteString;

from Symbols import
    ConstNode, ExprNode, ExprList, ExprSetNode, ExprSetList, IdentNode,
    IdentList, ExprKind, FormerElementNode, currLine, currFile, MemoryType,
    ConstSetNode;

from Consts import
    WriteConstant;
 
from BuildStmt import
    PrintStmtList;

procedure NewExprNode(const kind : ExprKind) : ExprNode;
(* Create an ExprNode with all fixed fields (except next) filled in. *)
var
    en : ExprNode;
begin
    new(en);
    en^.kind := kind;
$if pascal then
    en^.widthExpr := nil;
    en^.decimalExpr := nil;
$end
    en^.fileName := currFile;
    en^.lineNumber := currLine;
    en^.exprType := nil;
    en^.baseVar := nil;
    en^.basePtrType := nil;
    en^.opt := nil;
    en^.optNoReorder := false;
    en^.doCheck := odd(genCheckFlag);
    en^.doPtrAssignCheck := odd(genPtrAssignCheckFlag);
    return en;
end NewExprNode;

procedure SameExprLine(const newOne, oldOne : ExprNode);
(* Give newOne the same text position and runtime checking status as oldOne *)
begin
    newOne^.fileName := oldOne^.fileName;
    newOne^.lineNumber := oldOne^.lineNumber;
    newOne^.doCheck := oldOne^.doCheck;
    newOne^.doPtrAssignCheck := oldOne^.doPtrAssignCheck;
end SameExprLine;


(* grammar *) procedure BuildExprBad() : ExprNode;
begin
    return NewExprNode(EXPRBAD);
end BuildExprBad;


(* grammar+ *) procedure BuildExprConst(const cn : ConstNode) : ExprNode;
(* Create an ExprNode(CONST) that points to cn *) 
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprConst(%n)=', cn^.kind);
	WriteConstant(output,cn);
	Writec(output, '\n');
    end;
    en := NewExprNode(EXPRCONST);
    en^.exprConst := cn;
    en^.constType := nil; (* added by CED on 6/21/88 *)
    return en;
end BuildExprConst;

 
(* grammar *) procedure BuildExprSym(const names : IdentList) : ExprNode;
(* Create an ExprNode(EXPRNAME) that point to (qualified) names.
   This name will be resolved in CheckExpr. *)
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprSym\n');
    end;
    en := NewExprNode(EXPRNAME);
    en^.exprName := names;
    return en;
end BuildExprSym;

(* grammar *) procedure BuildExprDeref(const ptr : ExprNode) : ExprNode;
(* Create an ExprNode(EXPRDEREF) that does a dereference (^) of ptr.
   This will get turned into address arithmetic in CheckExpr. *)
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprDeref\n');
    end;
    en := NewExprNode(EXPRDEREF);
    en^.ptr := ptr;
    en^.realPtr := true;
    return en;
end BuildExprDeref;

(* grammar *) procedure BuildExprDot(const rec   : ExprNode; 
                                     const field : String) : ExprNode;
(* Create an ExprNode(EXPRDOT), and just point to the name field for now.
   The name will be resolved and the offset turned into address arithmetic
   by CheckExpr.  This routine is called only if a [...] or ^ has already been 
   seen...otherwise a.b.c is turned into an IdentList and things are only 
   sorted out later by CheckExpr. *) 
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprDot\n');
    end;
    en := NewExprNode(EXPRDOT);
    en^.rec := rec;
    en^.fieldName := field;
    en^.field := nil;
    SameExprLine(en,rec);
    return en;
end BuildExprDot;

(* grammar *) procedure BuildExprSubscript(const arr      : ExprNode; 
					   const subsList : ExprList) 
		: ExprNode;
(* If arr is a variable, field, etc. create a new ExprNode(EXPRSUBSCR) and
   point it at the list of subscripts subsList.  If arr is already an
   ExprNode(EXPRSUBSCR) (comes up for a[1][2]) just append the new subsList
   onto the one that exists. *)
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprSubscript\n');
    end;
    if arr^.kind = EXPRSUBSCR then
	(* if arr is already a subscript expr, just add subscripts *)
	arr^.subscripts := AppendExprList(arr^.subscripts,subsList);
	return arr;
    else
	(* create a new subscript node *)
	en := NewExprNode(EXPRSUBSCR);
	en^.arr := arr;
	en^.subscripts := subsList;
	return en;
    end;
end BuildExprSubscript;

(* grammar *) procedure BuildExprRange(const index, count : ExprNode) : ExprNode;
(* Create an ExprNode(EXPRRANGE) (for a[index:count] slices).
   Turned into address arithmetic by CheckExpr. *)
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprRange\n');
    end;
    en := NewExprNode(EXPRRANGE);
    en^.exprRangeIndex := index;
    en^.exprRangeCount := count;
    SameExprLine(en,index);
    return en;
end BuildExprRange;

(* grammar+ *) procedure BuildExprFunc(const func   : ExprNode; 
                                      const params : ExprList) : ExprNode;
(* Create an ExprNode pointing to the ExprNode that (presumably) calculates a
   function (remember procedure variables); and its actual parameters *)
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprFunc\n');
    end;
    en := NewExprNode(EXPRFUNC);
    en^.func := func;
    en^.params := params;
    en^.selected := false;
    en^.addrOfValue := nil;
    SameExprLine(en,func);
    return en;
end BuildExprFunc;

(* grammar *) procedure MakeExprSet (const lower, upper : ExprNode) : ExprSetNode;
var
    cln : ExprSetNode;
begin
    new(cln);
    cln^.lower := lower;
    cln^.upper := upper;
    return cln;
end MakeExprSet;

(* grammar+ *) procedure AddToExprSetList (      list   : ExprSetList; 
					   const newOne : ExprSetNode)
	: ExprSetList;
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
end AddToExprSetList;

(* grammar *) procedure BuildExprSet(const elementList : ExprSetList; 
				     const setTypeName : IdentList) : ExprNode;
(* Create an ExprNode(EXPRSET).  setTypeName = nil means none specified,
   so CheckExpr will assume BITSET. *)
var
    en : ExprNode;
begin
    en := NewExprNode(EXPRSET);
    en^.setTypeName := setTypeName;
    en^.setExpr := elementList;
    en^.setConst := nil;
    return en;
end BuildExprSet;


(* grammar+ *) procedure BuildExprUnOp(const oper : Token; 
				       const opnd : ExprNode) : ExprNode;
(* Create an ExprNode(EXPRUNOP), where oper in {TKNOT, TKPLUS, TKMINUS} *)
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprUnOp(%n)\n', oper);
    end;
    en := NewExprNode(EXPRUNOP);
    en^.doCheck := opnd^.doCheck;
    en^.doPtrAssignCheck := opnd^.doPtrAssignCheck;
    en^.exprUnOp := oper;
    en^.opnd := opnd;
    return en;
end BuildExprUnOp;

(* grammar *) procedure BuildExprBinOp(const oper	  : Token; 
				       const opnd1, opnd2 : ExprNode) 
		: ExprNode;
(* Create an ExprNode(EXPRBINOP), where oper in {TKASTERISK, TKSLASH, 
   TKDIV, TKMOD, TKPLUS, TKMINUS, TKAND, TKAMPERSAND, TKOR, TKEQUALS,
   TKSHARP, TKNOTEQUAL, TKLESS, TKLSEQUAL, TKGREATER, TKGREQUAL, TKIN} *)
var
    en : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'BuildExprBin(%n)\n', oper);
    end;
    en := NewExprNode(EXPRBINOP);
    en^.exprBinOp := oper;
    en^.opnd1 := opnd1;
    en^.opnd2 := opnd2;
    SameExprLine(en,opnd1); (* assume multi-line exprs have oper on 1st line *)
    en^.doCheck := opnd1^.doCheck or opnd2^.doCheck;
    en^.doPtrAssignCheck := opnd1^.doPtrAssignCheck or opnd2^.doPtrAssignCheck;
    return en;
end BuildExprBinOp;

(* grammar+ *) procedure AddToExprList(      list   : ExprList; 
				       const newOne : ExprNode) : ExprList;
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
end AddToExprList;

procedure AppendExprList(const some, more : ExprList) : ExprList;
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
end AppendExprList;

$if pascal then
procedure PrependExprList(list : ExprList; const newOne : ExprNode) : ExprList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
	list^.last := nil;
    end;
    newOne^.next := list^.first;
    if list^.last = nil then
	list^.last := newOne;
    end;
    list^.first := newOne;
    return list;
end PrependExprList;

(* grammar  *) procedure AddSubExpr(const expr, widthExpr, decimalExpr : ExprNode)
		: ExprNode;
begin
    expr^.widthExpr := widthExpr;
    expr^.decimalExpr := decimalExpr;
    return expr;
end AddSubExpr;
$end

(* debug *) procedure Indent(f : File; const indent:integer);
   var i : cardinal;
begin
    for i := 1 to indent do
	Writec(output, ' ');
    end;
end Indent;


(* debug *) procedure PrintExpr(const en : ExprNode ; const indent : integer);
(* Recursively print out an ExprNode in a full-blown, fully indented format.
   Used to insure expressions are parsed and check correctly. *)
var
    ten : ExprNode;
    id  : IdentNode;
    esn : ExprSetNode;
    csn : ConstSetNode;
    fen : FormerElementNode;
begin
    Indent(output,indent);
    if en = nil then
	Writef(output,'Expr NIL\n');

    else
	Writef(output, '%n ', en^.kind);
	case en^.kind of
	| EXPRBAD :
	    Writef(output,'\n');
	
	| EXPRNAME :
	    id := en^.exprName^.first;
	    while id # nil do
		WriteString(output,id^.name);
		id := id^.next;
		if id # nil then
		    Writec(output,'.');
		end;
	    end;
	    Writec(output, '\n');
	
	| EXPRVAR :
	    if en^.exprVar^.address.kind = MEMGLOBAL then
		WriteString(output, en^.exprVar^.address.gvn^.globalName);
		Writec(output, '\n');
	    elsif en^.exprVar^.address.kind = MEMINLINE then
		WriteString(output,en^.exprVar^.name);
		Writef(output, ' %n %n\n', en^.exprVar^.address.kind,
			en^.exprVar^.address.inlineVal.kind);
	    else
		WriteString(output, en^.exprVar^.name);
		Writef(output, ' %n %d %d %d\n',
			en^.exprVar^.address.kind,
			en^.exprVar^.address.level,
			en^.exprVar^.address.proc^.block,
			en^.exprVar^.address.offset);
	    end;
	
	| EXPRSYM :
	    WriteString(output, en^.exprSym^.name);
	    Writec(output, '\n');
	
	| EXPRCONST :
	    WriteConstant(output,en^.exprConst);
	    Writec(output, '\n');
	
	| EXPRUNOP :
	    Writec(output, '\n');
	    Indent(output,indent);
	    WriteString(output, stringToken[en^.exprUnOp]);
	    Writec(output, '\n');
	    PrintExpr(en^.opnd,indent+INDENT);
	
	| EXPRBINOP :
	    Writec(output, '\n');
	    PrintExpr(en^.opnd1,indent+INDENT);
	    Indent(output,indent);
	    WriteString(output, stringToken[en^.exprBinOp]);
	    Writec(output, '\n');
	    PrintExpr(en^.opnd2,indent+INDENT);
	
	| EXPRSUBSCR :
	    Writec(output, '\n');
	    PrintExpr(en^.arr,indent+INDENT);
	    ten := en^.subscripts^.first;
	    while ten # nil do
		Indent(output,indent);
		Writef(output,'****\n');
		PrintExpr(ten,indent+INDENT);
		ten := ten^.next;
	    end;
	     
	| EXPRDOT :
	    Writec(output, '\n');
	    PrintExpr(en^.rec,indent+INDENT);
	    Indent(output,indent+INDENT);
	    if en^.field # nil then
		WriteString(output,en^.field^.name);
	    else
		WriteString(output,en^.fieldName);
	    end;
	    Writec(output, '\n');
	
	| EXPRDEREF :
	    Writec(output, '\n');
	    PrintExpr(en^.ptr,indent+INDENT);
	
	| EXPRFUNC :
	    Writec(output, '\n');
	    PrintExpr(en^.func,indent+INDENT);
	    ten := en^.params^.first;
	    while ten # nil do
		Indent(output,indent);
		Writef(output,'****\n');
		PrintExpr(ten,indent+INDENT);
		ten := ten^.next;
	    end;
	
	| EXPRVAL :
	    Writec(output, '\n');
	    PrintExpr(en^.exprVal,indent+INDENT);
	
	| EXPRCHECK :
	    Writef(output, '%n\n', en^.exprCheck);
	    PrintExpr(en^.checkExpr,indent+INDENT);
	
	| EXPRSET :
	    Writef(output,'set expression\n');
	
	| EXPRFORMER:
	    if en^.exprFormer^.formerType # nil then
		WriteString(output,en^.exprFormer^.formerType^.name);
	    end;
	    Writec(output, '\n');

	| EXPRRANGE :
	    Writec(output, '\n');
	    PrintExpr(en^.exprRangeIndex,indent+INDENT);
	    Indent(output,indent);
	    Writef(output,':\n');
	    PrintExpr(en^.exprRangeCount,indent+INDENT);
	
	| EXPRSAVE :
	    Writec(output, '\n');
	    PrintExpr(en^.exprSave,indent+INDENT);
	
	| EXPRINLINE :
	    Writec(output, '\n');
	    Indent(output,indent);
	    WriteString(output,en^.inlineProc^.name);
	    Writec(output, '\n');
	    if en^.inlineParams # nil then
		ten := en^.inlineParams^.first;
		while ten # nil do
		    Indent(output,indent);
		    Writef(output,'****\n');
		    PrintExpr(ten,indent+INDENT);
		    ten := ten^.next;
		end;
	    end;
	    Indent(output,indent);
	    Writef(output,'body\n');
	    PrintStmtList(en^.inlineBody,indent+INDENT);
	end (* case *);
    end;
end PrintExpr;

(* debug *) procedure WriteExpr(const en : ExprNode);
(* WriteExpr is a quick and dirty dump of an expression, more for
   identification of the expression than its structure. *)
var
    id : IdentNode;
    pen : ExprNode;
begin
    case en^.kind of
    | EXPRBAD :
	Writef(output,'BAD');
    
    | EXPRNAME :
	id := en^.exprName^.first;
	while id # nil do
	    WriteString(output,id^.name);
	    if id^.next # nil then
		Writec(output,'.');
	    end;
	    id := id^.next;
	end;
    
    | EXPRVAR :
	Writef(output,'var ');
	if en^.exprVar^.name # nil then
	    WriteString(output,en^.exprVar^.name);
	else
	    Writef(output, '%n %d', en^.exprVar^.address.kind, 
		    en^.exprVar^.address.offset);
	end;
    
    | EXPRSUBSCR :
	WriteExpr(en^.arr);
	Writec(output, '[');
	pen := en^.subscripts^.first;
	while pen # nil do
	    WriteExpr(pen);
	    if pen^.next # nil then
		Writec(output, ',');
	    end;
	    pen := pen^.next;
	end;
	Writec(output, ']');
    
    | EXPRFUNC :
	WriteExpr(en^.func);
	Writec(output, '(');
	pen := en^.params^.first;
	while pen # nil do
	    WriteExpr(pen);
	    if pen^.next # nil then
		Writec(output, ',');
	    end;
	    pen := pen^.next;
	end;
	Writec(output, ')');
    
    | EXPRINLINE : 
            Writef(output,'inline ');
            WriteString(output,en^.inlineProc^.name);
            Writec(output,'(');
            if en^.inlineParams # nil then
                pen := en^.inlineParams^.first;
                while pen # nil do
                    WriteExpr(pen);
                    if pen^.next # nil then
                        Writec(output,',');
                    end;
                    pen := pen^.next;
                end;
            end;
            Writec(output,')');

    | EXPRSET :
	Writef(output, '[set]');
    
    | EXPRSYM :
	Writef(output,'sym ');
	WriteString(output,en^.exprSym^.name);
    
    | EXPRCONST :
	WriteConstant(output,en^.exprConst);
    
    | EXPRUNOP :
	WriteString(output, stringToken[en^.exprUnOp]);
	Writec(output, '(');
	WriteExpr(en^.opnd);
	Writec(output,')');
    
    | EXPRBINOP :
	Writec(output,'(');
	WriteExpr(en^.opnd1);
	WriteString(output, stringToken[en^.exprBinOp]);
	WriteExpr(en^.opnd2);
	Writec(output,')');
    
    | EXPRDOT :
	WriteExpr(en^.rec);
	Writec(output,'.');
	if en^.field # nil then
	    WriteString(output,en^.field^.name);
	else
	    WriteString(output,en^.fieldName);
	end;
    
    | EXPRDEREF :
	WriteExpr(en^.ptr);
	Writec(output,'^');
    
    | EXPRVAL :
	Writef(output,'value ');
	WriteExpr(en^.exprVal);

    | EXPRCHECK :
	Writef(output,'%n ', en^.exprCheck);
	WriteExpr(en^.checkExpr);
    
    | EXPRRANGE :
	WriteExpr(en^.exprRangeIndex);
	Writec(output,':');
	WriteExpr(en^.exprRangeCount);
    
    | EXPRSAVE :
	Writef(output,'save ');
	WriteExpr(en^.exprSave);
	
    end;
end WriteExpr;


end BuildExpr.
