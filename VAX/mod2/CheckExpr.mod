implementation module CheckExpr;

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
    WORDSIZE, CHARSIZE, MAXINT, HugeInteger, UNITSIZE;

from Tokens import
    Token, TokenSet, stringToken;

from Strings import
    String;

from Globals import
    target, TARGETVAX, standardCardinalFlag, TraceNexpr, DEBUG;

from Symbols import
    MemoryOffset, EvalMode, CheckKind, CheckKindSet, 
    GSNORMAL, Symbol,
    ConstNode, TypeNode, VarNode, FieldNode, VariantNode, ExprNode, ExprList, 
    ConstSetNode, ConstSetList, ExprSetNode, ExprSetList, ParamNode,
    IdentNode, IdentList, MEMFAST,
    MEMINLINE, MEMNORMAL, WithQualNode, ProcNode,
    DataType, DataTypeSet, stringDataType,
    ArrayKind, ArrayKindSet, 
    BIPNOTBIP, SymbolKind, ExprKind, ExprKindSet, ParamKind, ParamKindSet,
    integerTypeNode,  cardinalTypeNode,
    booleanTypeNode, addressTypeNode,
    cardIntTypeNode, realConstTypeNode, charConstTypeNode, 
    unknownSetTypeNode, intsetTypeNode, anyTypeNode, currProc, 
    withQualList, LookUpSymbol, DefineSymbol, VarKind,
    ONECASE, QualifiedName, currLine, currFile;
    
from Errors import
    ExprError, ExprErrorName, ExprErrorNumber, ExprErrorConst,
    ExprErrorNameNumber, SilentError, ExprWarning;

from TypeInfo import
    NewTypeNode, BaseType, ActualType, StoredType, LowerBoundOf,
    UpperBoundOf, AlignmentOf, SizeOf;

from TypeDefs import 
    SetType;

from Consts import
    WriteConstant, OrdOf, CardinalConst, ConstUnOp, ConstBinOp,
    AddToConstSetList;

from Decls import
    DefineVar;

from BuildExpr import
    NewExprNode, SameExprLine, BuildExprConst, WriteExpr, AddToExprList;

$if modula2 then
from Globals import standardStringFlag;
from Symbols import bitsetTypeNode, wordTypeNode, packedByteTypeNode,
    packedCharTypeNode;
from TypeInfo import WordSizeOf, NumberOf;

$else

from Symbols import 
    inputString, inputDeclared, outputString, outputDeclared,
    fnilProcNode, 
    longrealTypeNode, longfloatProcNode;
from BuildExpr import
    BuildExprFunc;
$end

from CheckBuiltin import
    CheckBuiltin;

from Former import
    CheckExprFormer;

from Inline import
    ExpandFuncInline;

from Compatible import
    Compatible, Assignable, Passable;

(*
    Resolve, type-check and reconstruct expressions:

        Resolution is through the procedures CheckExprName, CheckExprSym,
        CheckExprRecord, and CheckExprImplQual.  These routines figure out what
        a name list means and call the regular type-checking routines.  They do
        no checking on their own.

	The regular type-checking routines take an ExprNode parameter and a
	mode indicating how the expression is used (principally, whether the
	value or address is of interest).  For address expressions, they are
	free to reconstruct the expression tree in terms of explicit address
	calculations.  For value expressions, they must insert a value node.
	Since the original expression nodes may be lost, having been converted
	to an address expression, these routines return a type which represents
	the logical type of the expression.  In reconstructing the expression
	tree, the argument ExprNode must remain the root of the expression,
	since there are pointers to it.

	After this pass, there should be no SUBSCRIPT, DOT, or DEREF nodes.
	NAME nodes should also be gone.  SYM nodes may still exist for
	type names.  VAR nodes subsequently mean the address of the variable.

*)
procedure BadExpr(en : ExprNode);
begin
    en^.kind := EXPRBAD;
    en^.exprType := anyTypeNode;
end BadExpr;

procedure IsBadExpr(en : ExprNode) : boolean;
begin
    return (en = nil) or (en^.kind = EXPRBAD);
end IsBadExpr;

procedure IsAddressableExpr(en : ExprNode) : boolean;
begin
    return (en = nil) or (en^.kind = EXPRBAD) or
	    ((en^.kind in ExprKindSet{EXPRVAL, EXPRVAR, EXPRBINOP, EXPRCHECK, 
				     EXPRBAD,EXPRFUNC}) and
	    (ActualType(en^.exprType) = addressTypeNode)) or
	    ((en^.kind = EXPRFUNC) and (en^.func^.kind = EXPRSYM));
end IsAddressableExpr;


procedure IsAddressableType(tn : TypeNode) : boolean;
begin
    return (tn = nil) or ((AlignmentOf(tn) mod UNITSIZE) = 0);
end IsAddressableType;


(* MakeExprVal: make a new val expression of specified type *)
procedure MakeExprVal(tn : TypeNode; addr : ExprNode) : ExprNode;
var
    en : ExprNode;
begin
    en := NewExprNode(EXPRVAL);
    en^.exprVal := addr;
    en^.dependVar := addr^.baseVar;
    en^.dependPtrType := addr^.basePtrType;
    en^.exprType := tn;
    en^.doCheck := addr^.doCheck;
    en^.doPtrAssignCheck := addr^.doPtrAssignCheck;
    return en;
end MakeExprVal;

$if pascal then
procedure CheckFileDeclared(en : ExprNode);
begin
    if (en^.exprVar^.name = inputString) and (not inputDeclared) then
	ExprError(en,
	    'File "input" must be declared in program heading');
	inputDeclared := true;
    elsif (en^.exprVar^.name = outputString) and (not outputDeclared) then
        ExprError(en,
            'File "output" must be declared in program heading');
        outputDeclared := true;
    end;
end CheckFileDeclared;
$end

(* MakeExprVar: make a new var expression (address type) *)
procedure MakeExprVar(vn : VarNode; infoen : ExprNode) : ExprNode;
var
    en : ExprNode;
begin
    en := NewExprNode(EXPRVAR);
    en^.fileName := infoen^.fileName;
    en^.lineNumber := infoen^.lineNumber;
    en^.doCheck := infoen^.doCheck;
    en^.doPtrAssignCheck := infoen^.doPtrAssignCheck;
    en^.exprVar := vn;
    en^.exprType := addressTypeNode;
    en^.baseVar := vn;
    
$if pascal then
    CheckFileDeclared(en);
$end
    return en;
end MakeExprVar;

(* MakeExprConst: make a new cardinal constant expr with appropriate type*)
procedure MakeExprConst(tn : TypeNode; value : HugeInteger) : ExprNode;
var
    en : ExprNode;
begin
    en := NewExprNode(EXPRCONST);
    en^.exprConst := CardinalConst(value);
    en^.exprType := tn;
    en^.constType := tn; (* CED 6/21/88 *)
    return en;
end MakeExprConst;


(* Make a new string constant expression *)
procedure MakeExprConstString(string : String) : ExprNode;
var
    en : ExprNode;
    tn : TypeNode;
    cn : ConstNode;
begin
    new(cn);
    cn^.kind := DTSTRING;
    cn^.strVal := string;
    en := BuildExprConst(cn);
    tn := CheckExpr(en, EVALPOINT);
    return en;
end MakeExprConstString;


procedure EvalConstExprBinOp(en : ExprNode);
var
    con : ConstNode;
begin
    if (en^.opnd1^.kind = EXPRCONST) then
        if (en^.opnd2^.kind = EXPRCONST) then
	    con := ConstBinOp(en^.exprBinOp,en^.opnd1^.exprConst,
		    en^.opnd2^.exprConst,true);
	    en^.kind := EXPRCONST;
	    en^.exprConst := con;
	    if en^.exprType^.kind in DataTypeSet{DTINTEGER, DTCARDINAL} then
		en^.exprType := ConstType(con);
	    end;
	    en^.constType := en^.exprType;

	elsif (en^.exprBinOp in TokenSet{TKAND, TKAMPERSAND}) then
	    if en^.opnd1^.exprConst^.boolVal then
		(* Just evaluate second argument *)
		AssignExprNode(en, en^.opnd2);
	    else
		(* Expression is false *)
		AssignExprNode(en, en^.opnd1);
	    end;

	elsif en^.exprBinOp = TKOR then
	    if en^.opnd1^.exprConst^.boolVal then
		(* Expression is true *)
		AssignExprNode(en, en^.opnd1);
	    else
		(* Just evaluate second argument *)
		AssignExprNode(en, en^.opnd2);
	    end;
	end;
(* ||| Actually could do the same if left var, right const, as long as left
   doesn't make a (possibly side-effecting) function call *)
    end;
end EvalConstExprBinOp;


(* MakeExprUnOp: make a new unary op expression with appropriate type *)
procedure MakeExprUnOp(tn : TypeNode; oper : Token; opnd : ExprNode) : ExprNode;
var
    en : ExprNode;
begin
    en := NewExprNode(EXPRUNOP);
    en^.fileName := opnd^.fileName;
    en^.lineNumber := opnd^.lineNumber;
    en^.doCheck := opnd^.doCheck;
    en^.doPtrAssignCheck := opnd^.doPtrAssignCheck;
    en^.exprUnOp := oper;
    en^.opnd := opnd;
    en^.exprType := tn;
    en^.unOperType := tn;
    en^.baseVar := opnd^.baseVar;
    en^.basePtrType := opnd^.basePtrType;
    return en;
end MakeExprUnOp;

(*
Make a new binary op expression with appropriate type.
*)
procedure MakeExprBinOp(tn : TypeNode; oper : Token; opnd1, opnd2 : ExprNode)
	: ExprNode;
var
    en : ExprNode;
begin
    en := NewExprNode(EXPRBINOP);
    en^.fileName := opnd1^.fileName;
    en^.lineNumber := opnd1^.lineNumber;
    en^.doCheck := opnd1^.doCheck or opnd2^.doCheck;
    en^.doPtrAssignCheck := opnd1^.doPtrAssignCheck or opnd2^.doPtrAssignCheck;
    en^.exprBinOp := oper;
    en^.opnd1 := opnd1;
    en^.opnd2 := opnd2;
    en^.exprType := tn;
    en^.operType := tn;
    assert ((opnd1^.baseVar = nil) or (opnd2^.baseVar = nil));
    assert ((opnd1^.basePtrType = nil) or (opnd2^.basePtrType = nil));
    if opnd1^.baseVar # nil then
	en^.baseVar := opnd1^.baseVar;
    else
	en^.baseVar := opnd2^.baseVar;
    end;
    if opnd1^.basePtrType # nil then
	en^.basePtrType := opnd1^.basePtrType;
    else
	en^.basePtrType := opnd2^.basePtrType;
    end;
    EvalConstExprBinOp(en);
    return en;
end MakeExprBinOp;

(*
If opnd1 or opnd2 is NIL, assume this means the identify transformation, and
just return the non-NIL operand.  Otherwise do MakeExprBinOp.  Used to
make a "chain" of subscript/size calculations that starts off NIL.
*)
procedure MakeExprBinChain(tn : TypeNode; oper : Token; opnd1, opnd2 : ExprNode)
	: ExprNode;
begin
    if opnd1 = nil then
	return opnd2;
    elsif opnd2 = nil then
	return opnd1;
    else
	return MakeExprBinOp(tn, oper, opnd1, opnd2);
    end;
end MakeExprBinChain;


procedure MakeExprOffset(const tn    : TypeNode;
			 const oper  : Token;
			 const opnd1 : ExprNode;
			 const value : HugeInteger) : ExprNode;
begin
    return MakeExprBinOp(tn, oper, opnd1, MakeExprConst(tn, value));
end MakeExprOffset;

(* MakeExprSave: make a save expr node *)
procedure MakeExprSave(saved : ExprNode; vn : VarNode) : ExprNode;
var
    en : ExprNode;
begin
    en := NewExprNode(EXPRSAVE);
    en^.exprSave := saved;
    en^.exprSaveVar := vn;
    en^.exprType := saved^.exprType;
    en^.fileName := saved^.fileName;
    en^.lineNumber := saved^.lineNumber;
    en^.baseVar := saved^.baseVar;
    en^.basePtrType := saved^.basePtrType;
    en^.doCheck := saved^.doCheck;
    en^.doPtrAssignCheck := saved^.doPtrAssignCheck;
    return en;
end MakeExprSave;

(* MakeExprCheck: make a runtime check expr node *)
procedure MakeExprCheck(checkExpr : ExprNode; kind : CheckKind;
    arrVar, checkVar : VarNode; tn : TypeNode;
    lowerBound, upperBound : integer; checkVariant : VariantNode) : ExprNode;
var
    en       : ExprNode;
    constVal : integer;
begin
    if (kind in CheckKindSet{CHECKSUBSCR,CHECKRANGE}) and 
	    (checkExpr^.kind = EXPRCONST) then
	(* Do the check now *)
	constVal := trunc(OrdOf(checkExpr^.exprConst));
	if ((lowerBound <= upperBound) and
		((constVal < lowerBound) or (constVal > upperBound))) 
	   or ((lowerBound > upperBound) and
		((unsigned(constVal) < unsigned(lowerBound)) or
		    (unsigned(constVal) > unsigned(upperBound)))) then
	    ExprErrorConst(checkExpr, 'Constant value $ exceeds range bounds',
		checkExpr^.exprConst);
	end;
	return checkExpr;
    else
	en := NewExprNode(EXPRCHECK);
	en^.exprType := checkExpr^.exprType;
	en^.fileName := checkExpr^.fileName;
	en^.lineNumber := checkExpr^.lineNumber;
	en^.baseVar := checkExpr^.baseVar;
	en^.basePtrType := checkExpr^.basePtrType;
	en^.doCheck := checkExpr^.doCheck;
	en^.doPtrAssignCheck := checkExpr^.doPtrAssignCheck;
	en^.exprCheck := kind;
	en^.checkExpr := checkExpr;
	en^.checkVar := checkVar;
	en^.arrVar := arrVar;
	en^.checkType := tn;
	en^.checkVariant := checkVariant;
	en^.checkLower := lowerBound;
	en^.checkUpper := upperBound;
	return en;
    end;
end MakeExprCheck;


$if pascal then
procedure MakeExprProc(const pn : ProcNode; const infoen : ExprNode) : ExprNode;
var
    en : ExprNode;
    cn : ConstNode;
begin
    new(cn);
    cn^.kind := DTPROC;
    cn^.procVal := pn;
    en := NewExprNode(EXPRCONST);
    SameExprLine(en, infoen);
    en^.exprConst := cn;
    en^.constType := pn^.procType;
    en^.exprType := pn^.procType;
    return en;
end MakeExprProc;
    

procedure MakeExprFunc(const proc    : ProcNode;
		       const infoen  : ExprNode;
		       const params  : ExprList;
		       const retType : TypeNode) : ExprNode;
var
    en : ExprNode;
begin
    en := BuildExprFunc(MakeExprProc(proc, infoen), params);
    en^.exprType := retType;
    SameExprLine(en, infoen);
    return en;
end MakeExprFunc;
$end (* pascal *)

procedure ConstType(cn : ConstNode) : TypeNode;
var
    tn : TypeNode;
begin
    if cn = nil then
	tn := nil;
    else
	if DEBUG and TraceNexpr then
	    Writef(output,'ConstType(%n)=', cn^.kind);
	    WriteConstant(output,cn);
	    Writec(output, '\n');
	end;
	case cn^.kind of
	| DTINTEGER, DTCARDINAL :
	    if cn^.cardVal > MAXINT then
		tn := cardinalTypeNode;
	    elsif cn^.cardVal < 0.0 then
		tn := integerTypeNode;
	    else
		tn := cardIntTypeNode;
	    end;

	| DTLONGREAL, 
	  DTREAL :
	    tn := realConstTypeNode
	| DTCHAR :
	    tn := charConstTypeNode
	| DTBOOLEAN :
	    tn := booleanTypeNode
	| DTSTRING :
	    tn := NewTypeNode(DTSTRING);
$if modula2 then
	    if standardStringFlag then
		tn^.stringLength := cn^.strVal^.length;
		if tn^.stringLength = 0 then
		    tn^.stringLength := 1;
		end;
		tn^.size := tn^.stringLength * CHARSIZE;
	    else
		tn^.stringLength := cn^.strVal^.length;
		(* count null *)
		tn^.stringLength := cn^.strVal^.length;
		tn^.size := (tn^.stringLength+1) * CHARSIZE;
	    end;
$else (* pascal *)
	    tn^.stringLength := cn^.strVal^.length;
	    tn^.size := tn^.stringLength * CHARSIZE;
$end
	| DTENUMERATION :
	    tn := cn^.enumVal^.enumType
	| DTPOINTER :
	    tn := addressTypeNode
	| DTPROC :
	    tn := cn^.procVal^.procType
	| DTSET :
	    tn := cn^.setVal^.setType
	end;
    end;
    return tn;
end ConstType;

procedure CheckExprImplQual(en : ExprNode; sym : Symbol; mode : EvalMode)
		: TypeNode;
var
    qen, aen : ExprNode;
    wqn : WithQualNode;
    fn : FieldNode;
    tn : TypeNode;
begin
    fn := sym^.symField;
    if fn = nil then 
	(* field used but not defined by user; compiler entered it *)
	BadExpr(en);
	return anyTypeNode;
    end;
    wqn := withQualList;
    while (wqn # nil) and (fn^.recType # wqn^.recType) do
	wqn := wqn^.next;
    end;
    tn := nil;
    if wqn = nil then
	ExprErrorName(en,sym^.name,'Field $ used without qualification???');
	BadExpr(en);
    else
	(* found field in a with statemtent: generate implQual^.field *)
	qen := NewExprNode(EXPRVAR);		(* node for implQual *)
	SameExprLine(qen,en);
	qen^.exprVar := wqn^.implQual;
	aen := NewExprNode(EXPRDEREF);		(* node for implQual^ *)
	SameExprLine(aen,en);
	aen^.ptr := qen;
	aen^.realPtr := false;
	en^.kind := EXPRDOT;			(* node for implQual^.field *)
	en^.rec := aen;
	en^.field := fn;
	en^.fieldName := nil;
	(* get address *)
	tn := CheckExprDot(en,EVALPOINT);
	(* this expression depends on the var in the with expression *)
	en^.baseVar := wqn^.baseVar;
	en^.basePtrType := wqn^.basePtrType;
	(* do dereference if necessary *)
	ValueOrAddr(en,fn^.fieldType,mode);
    end;
    return tn;
end CheckExprImplQual;

procedure CheckExprRecord(en : ExprNode; sym : Symbol; names : IdentList;
	mode : EvalMode) : TypeNode;
var
    id : IdentNode;
    vt, rentn : TypeNode;
    ren, fen : ExprNode;
begin
    (* get expression for record symbol *)
    ren := NewExprNode(EXPRSYM);
    SameExprLine(ren,en);
    ren^.exprSym := sym;
    (* add field qualifications to record *)
    id := names^.first;
    while id # nil do
	if id^.next = nil then
	    fen := en;	(* use original en for final field *)
	    fen^.kind := EXPRDOT;
	else
	    fen := NewExprNode(EXPRDOT);    (* otherwise, get new one*)
	    SameExprLine(fen,ren);
	end;
	fen^.rec := ren;
	fen^.fieldName := id^.name;
	fen^.field := nil;
	ren := fen;
	id := id^.next;
    end;
    assert (en = ren);
     return CheckExprDot(en,mode);
end CheckExprRecord;

procedure CheckExprSym(en : ExprNode; mode : EvalMode) : TypeNode;
var
    sym : Symbol;
    cn : ConstNode;
    tn : TypeNode;
    sympn, pn : ProcNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprSym\n');
    end;
    sym := en^.exprSym;
    tn := nil;
    if sym = nil then
	BadExpr(en);
        return nil;
    end;
    case sym^.kind of
    | SYMCONST :
	en^.kind := EXPRCONST;
	en^.exprConst := sym^.symConst;
	tn := CheckExprConst(en,mode);

    | SYMVAR :
	en^.kind := EXPRVAR;
	en^.exprVar := sym^.symVar;
	tn := CheckExprVar(en,mode);

    | SYMFIELD :
	tn := CheckExprImplQual(en,sym,mode);

    | SYMPROC :
$if modula2 then
	new(cn);
	cn^.kind := DTPROC;
	cn^.procVal := sym^.symProc;
	en^.kind := EXPRCONST;
	en^.exprConst := cn;
	tn := CheckExprConst(en,mode);
	if mode = EVALGET then
	    if sym^.symProc^.builtin # BIPNOTBIP then
		ExprErrorName(en,sym^.name,
		    'Builtin procedure $ cannot be a procedure value');
	    end;
	    sym^.symProc^.internalProc := false;
	end;
$else (* pascal *)
	if mode = EVALPUT then		(* store in function name *)
	    sympn := sym^.symProc;
	    if sympn^.builtin # BIPNOTBIP then
		ExprErrorName(en, sympn^.name, 
		    'Cannot assign result to standard procedure/function $');
	    elsif sympn^.procType^.funcType = nil then
		ExprErrorName(en, sympn^.name, 
		    'Cannot assign function result to procedure $');
		BadExpr(en);
	    else
		pn := currProc;
		while (pn # nil) and (pn # sympn) do
		    pn := pn^.enclosing;
		end;
		if pn = nil then
		    ExprErrorName(en, sympn^.name, 
			'Function $ is not within an assignable scope');
		    BadExpr(en);
		else
		    (* Convert this to the function's return value VarNode *)
		    sympn^.funcAssigned := true;
		    en^.kind := EXPRVAR;
		    en^.exprVar := sympn^.returnVar;
		    return CheckExprVar(en, EVALPUT);
		end;
	    end (* if procType = nil then ... else ... *);

	elsif mode = EVALPOINT then     (* return proc to be called *)
	    new(cn);
	    cn^.kind := DTPROC;
	    cn^.procVal := sym^.symProc;
	    en^.kind := EXPRCONST;
	    en^.exprConst := cn;
	    tn := CheckExprConst(en,mode);

	else (* mode = EVALGET *)       (* return function call value *)
	    en^.kind := EXPRFUNC;
	    en^.func := NewExprNode(EXPRCONST);
	    en^.params := AddToExprList(nil, nil);
	    en^.selected := false;
	    en^.addrOfValue := nil;
	    SameExprLine(en^.func, en);
	    new(cn);
	    cn^.kind := DTPROC;
	    cn^.procVal := sym^.symProc;
	    en^.func^.exprConst := cn;
	    tn := CheckExprFunc(en, EVALGET);
	end;
$end

    | SYMENUM :
	new(cn);
	cn^.kind := DTENUMERATION;
	cn^.enumVal := sym^.symEnum;
	en^.kind := EXPRCONST;
	en^.exprConst := cn;
	tn := CheckExprConst(en,mode);

    | SYMTYPE :
	en^.kind := EXPRSYM;
	en^.exprSym := sym;
	en^.exprType := StoredType(sym^.symType);
	tn := en^.exprType;

    | else
	ExprErrorName(en,sym^.name,'Symbol $ not valid in expression');
	BadExpr(en);
    end;
    return tn;
end CheckExprSym;

procedure CheckExprName(en : ExprNode; mode : EvalMode) : TypeNode;
var
    sym : Symbol;
    cn : ConstNode;
    tn : TypeNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprName\n');
    end;
    tn := nil;
    sym := QualifiedName(en^.exprName);
    if sym = nil then
	BadExpr(en);
    elsif en^.exprName^.first # nil then
	(* more qualifiers, must be a record *)
	tn := CheckExprRecord(en,sym,en^.exprName,mode);
    else
	en^.kind := EXPRSYM;
	en^.exprSym := sym;
	tn := CheckExprSym(en,mode);
    end;
    return tn;
end CheckExprName;

(* Insert a value node if necessary (mode = EVALGET). *)
(*  Change expression type to address.  *)
procedure ValueOrAddr(en : ExprNode; tn : TypeNode; mode : EvalMode);
var
    nen : ExprNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'ValueOrAddr ');
	WriteExpr(en);
	Writec(output, '\n');
    end;
    if mode = EVALGET then
	nen := NewExprNode(en^.kind);
	nen^ := en^;
	nen^.exprType := addressTypeNode;
	en^.kind := EXPRVAL;
	en^.exprVal := nen;
	en^.dependVar := nen^.baseVar;
	en^.dependPtrType := nen^.basePtrType;
	en^.baseVar := nil;
	en^.basePtrType := nil;
	en^.exprType := tn;
    else
	en^.exprType := addressTypeNode;
    end;
end ValueOrAddr;


(*  Add a dereference if the expression is an open array *)
procedure RefOpenArray(en : ExprNode; tn : TypeNode);
    var fen : ExprNode;
begin
    if (tn^.kind = DTARRAY) and (tn^.arrayKind # ARRAYNORMAL) then
	(* ||| Kludge to keep EXPRVALUE from being formed over EXPRFUNC 
	   that returns a NOCOUNT dynarray *)
	fen := en;
	if (fen^.kind = EXPRCHECK) and (fen^.exprCheck = CHECKDYNARRAY) and 
		(fen^.checkExpr # nil) then
	    fen := fen^.checkExpr;
	end;
	if (fen^.kind = EXPRFUNC) and (fen^.exprType^.kind = DTDYNARRAY) and
		(SizeOf(fen^.exprType) <= WORDSIZE) then
	    (* No Value node needed...selected set false in CheckFuncProc *)
	else
	    ValueOrAddr(en,addressTypeNode,EVALGET);
	    (* indirection is not a real value *)
	    en^.dependVar := nil;
	    en^.dependPtrType := nil;
	    (* propagate base variable *)
	    en^.baseVar := en^.exprVal^.baseVar;
	    en^.basePtrType := en^.exprVal^.basePtrType;
	end;
    end;
end RefOpenArray;

(* The following are the real expression manipulating routines. *)
(*   The above name-resolving routines "always" call them. *)
(*   ( exceptions are errors, type names, procedure names, etc.) *)

procedure CheckExprConst(en : ExprNode; mode : EvalMode) : TypeNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprConst(%n)=', en^.exprConst^.kind);
	WriteConstant(output,en^.exprConst);
	Writec(output, '\n');
    end;
    if mode = EVALPUT then
	ExprErrorName(en,stringDataType[en^.exprConst^.kind],
		'Constant $ must not be changed');
	BadExpr(en);
    else
	en^.exprType := ConstType(en^.exprConst);
	en^.constType := en^.exprType;
    end;
    return en^.exprType;
end CheckExprConst;

procedure CheckExprVar(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn : TypeNode;
    vn : VarNode;
begin
    vn := en^.exprVar;
$if pascal then
    CheckFileDeclared(en);
$end
    tn := StoredType(vn^.varType);
    if tn = anyTypeNode then
	SilentError();
    end;
    if vn^.indirect and (vn^.address.kind # MEMINLINE) then
	(* insert value node to get address of variable *)
	en^.baseVar := vn;
	ValueOrAddr(en,addressTypeNode,EVALGET);
	(* indirection is not real value *)
	en^.dependVar := nil;
	en^.dependPtrType := nil;
    end;
    if mode = EVALPUT then
	if vn^.kind = VARNORMAL then
	elsif vn^.kind = VARCONSTPARAM then
	    ExprErrorName(en,vn^.name,'Cannot change CONST parameter $');
	elsif vn^.kind = VARFORINDEX then
	    ExprErrorName(en,vn^.name,'Cannot change FOR loop index $');
	end;
    end;
    en^.baseVar := vn;
    ValueOrAddr(en,tn,mode);
    return tn;
end CheckExprVar;

procedure CheckExprUnOp(en : ExprNode; mode : EvalMode) : TypeNode;
var
    opndtn, et : TypeNode;
    opnd : ExprNode;
    con : ConstNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprUnOp(%n,%n)\n', en^.exprUnOp, mode);
    end;
    assert(en^.exprUnOp in TokenSet{TKNOT,TKPLUS,TKMINUS}, 
	'CheckExprUnOp operator invalid');
    opndtn := BaseType(CheckExpr(en^.opnd,EVALGET));
    en^.unOperType := opndtn;
    opnd := en^.opnd;
    et := nil;
    if (opndtn = nil) or (opndtn^.kind = DTANY) then
	(* do nothing *)
    elsif en^.exprUnOp = TKNOT then
	if opndtn^.kind = DTBOOLEAN then
	    et := opndtn;
	else
	    ExprError(en, 'NOT allowed only on BOOLEAN');
	end;
    elsif (en^.exprUnOp = TKMINUS) then
        if (opndtn^.kind = DTCARDINAL) then
	    (* - of cardinal becomes an integer *)
	    en^.unOperType := integerTypeNode;
	    et := integerTypeNode;
	    ExprWarning(en, 'Unary - on UNSIGNED?  Are you sure?');
	elsif opndtn^.kind in DataTypeSet{DTINTEGER, DTREAL, DTLONGREAL} then
	    et := opndtn;
	else
	    ExprError(en,
		'Unary - allowed on REAL, LONGREAL, and INTEGER only');
	end;
    else (* en^.exprUnOp = TKPLUS *)
	if opndtn^.kind in
		DataTypeSet{DTINTEGER, DTCARDINAL, DTREAL, DTLONGREAL} then
	    et := opndtn;
	else
	    ExprError(en,
	       'Unary + allowed on REAL, LONGREAL, INTEGER, and CARDINAL only');
	end;
    end;
    if et = nil then
	BadExpr(en);
    else
	en^.exprType := et;
	en^.baseVar := opnd^.baseVar;
	en^.basePtrType := opnd^.basePtrType;
	if en^.opnd^.kind = EXPRCONST then
	    if mode = EVALPUT then
		ExprError(en, 'Cannot store into a constant expression');
		BadExpr(en);
	    else
		con := ConstUnOp(en^.exprUnOp,en^.opnd^.exprConst);
		en^.kind := EXPRCONST;
		en^.exprConst := con;
		if en^.exprType^.kind in DataTypeSet{DTINTEGER, DTCARDINAL} then
		    en^.exprType := ConstType(con);
		end;
		en^.constType := en^.exprType;
	    end;
	elsif mode # EVALGET then
	    ExprError(en, 'Cannot address or store into an expression');
	    BadExpr(en);
	end;
    end;
    return en^.exprType;
end CheckExprUnOp;


$if pascal then
procedure SetSetType(en : ExprNode; var setType : TypeNode);
(* Traverse expression tree looking for EXPRSET nodes to fill in.
   Also set exprType, setType, and operType fields as appropriate.
   In worst case (e.g. [a..b] = [c..d]) setType will start as
   unknownSetTypeNode, and be changed to a real type by 
   ExprSetToConstSet at the first chance it gets. *)

begin
    case en^.kind of
    | EXPRSET :
	en^.setConst := ExprSetToConstSet(en^.setExpr, setType, DTSET);
	en^.setType := setType;
	en^.exprType := setType;
    | EXPRBINOP :
	assert(en^.exprBinOp in TokenSet{TKPLUS, TKMINUS, TKASTERISK});
	assert(en^.exprType = en^.operType);
	assert(en^.exprType^.kind = DTSET);
	SetSetType(en^.opnd1, setType);
	SetSetType(en^.opnd2, setType);
	if en^.exprType = unknownSetTypeNode then
	    en^.exprType := setType;
	    en^.operType := setType;
	else
	    assert(en^.exprType = setType);
	end;
    | else
	(* no need to go down tree any further...won't find any set
	   constructors *)
    end (* case *);
end SetSetType;


procedure SetNilSetType(en : ExprNode; setType : TypeNode);
(* In VERY worst case (e.g. [] = [a..b]) any nil sets will still
   have unknownTypeSetNode.  Look for them and fix them.  Note we
   must look at left AND right side to catch [] = ([] + [a..b]). *)
begin
    case en^.kind of
    | EXPRSET :
	en^.setType := setType;
	en^.exprType := setType;
    | EXPRBINOP :
	en^.exprType := setType;
	en^.operType := setType;
	SetNilSetType(en^.opnd1, setType);
	SetNilSetType(en^.opnd2, setType);
    | else
	(* The end.  So there. *)
    end (* case *);
end SetNilSetType;

procedure IntegerToReal(en : ExprNode);
var  newen : ExprNode;
begin
    assert((en^.exprType^.kind = DTINTEGER) or
	   (en^.exprType^.kind = DTSUBRANGE) and 
	     (en^.exprType^.baseType^.kind = DTINTEGER));
    new(newen);
    AssignExprNode(newen, en);
    (* Convert en into a call to longfloat *)
    en^.exprType := longrealTypeNode;
    en^.kind := EXPRFUNC;
    en^.selected := false;
    en^.addrOfValue := nil;
    en^.func := NewExprNode(EXPRCONST);
    SameExprLine(en^.func, en);
    en^.func^.constType := longfloatProcNode^.procType;
    en^.func^.exprType := longfloatProcNode^.procType;
    new(en^.func^.exprConst);
    en^.func^.exprConst^.kind := DTPROC;
    en^.func^.exprConst^.procVal := longfloatProcNode;

    en^.params := AddToExprList(nil, newen);
end IntegerToReal;
$end (* pascal *)

(*

    The following are legal binary operations.  In all cases except IN,
	operands must be compatible.
    
    e in s: result boolean
	e must be compatible with range of set s

    relations: result boolean
	    I  C  R  S  B  St Ch E Ptr Ad
	=   +  +  +  +  +  +  +  +  +  + 
	#   +  +  +  +  +  +  +  +  +  + 
	<>  +  +  +  +  +  +  +  +  +  + 
	>=  +  +  +  +  +  +  +  +     +
	>   +  +  +  p  +  +  +  +     +
	<=  +  +  +  +  +  +  +  +     +
	<   +  +  +  p  +  +  +  +     +

    (p means available in Pascal compiler only)

    arithmetic and set operations: result operand type
	    I  C  R  S
	+   +  +  +  +
	-   +  +  +  +
	*   +  +  +  +
	/         +  +
	div +  +
	mod +  +
    
    boolean operations: result boolean
	    B
	and +
	&   +
	or  +
*)
procedure CheckExprBinOp(en : ExprNode; mode : EvalMode) : TypeNode;
var
    opndType, resultType, setType, elementType : TypeNode;
    opnd1, opnd2, nen : ExprNode;
    opnd1tn, opnd2tn : TypeNode;
    opnd1btn, opnd2btn : TypeNode;
    oper : Token;
    con : ConstNode;
    lowerBound : HugeInteger;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprBinOp(%n,%n)\n', oper, mode);
    end;
    oper := en^.exprBinOp;
    resultType := nil;
    opnd1tn := CheckExpr(en^.opnd1,EVALGET);
    opnd1 := en^.opnd1;
    opnd2tn := CheckExpr(en^.opnd2,EVALGET);
    opnd2 := en^.opnd2;
    if IsBadExpr(opnd1) or IsBadExpr(opnd2) or 
	  (opnd1tn^.kind = DTANY) or (opnd2tn^.kind = DTANY) then
	(* do nothing *)
    elsif oper = TKIN then
(* IN *)
	setType := ActualType(opnd2tn);
	if setType^.kind # DTSET then
	    ExprError(en,'Right operand of IN is not a set');
	else
$if pascal then
	    if setType = unknownSetTypeNode then
		if opnd1tn^.kind = DTINTEGER then
		    setType :=  intsetTypeNode;
		else
		    setType := SetType(opnd1tn, TKNULL);
		end;
	    end;
	    SetSetType(opnd2, setType);
$end
	    opndType := Compatible(opnd1tn,opnd1,setType^.setRange,nil);
	    if opndType = nil then
		ExprErrorName(en, setType^.name,
			'Left operand of IN does not match set type $');
	    else
		opndType := BaseType(opndType);
		en^.operType := setType;
		lowerBound := LowerBoundOf(setType^.setRange);
		if lowerBound # 0.0 then
		    (* replace opnd1 with (opnd1 - lowerBound) *)
		    en^.opnd1 :=
		       MakeExprOffset(opnd1^.exprType,TKMINUS,opnd1,lowerBound);
		end;
		resultType := booleanTypeNode;
	    end;
	end;
    else
(* ALL OTHERS *)
$if pascal then
	opnd1btn := BaseType(opnd1tn);
	opnd2btn := BaseType(opnd2tn);
	if (opnd1btn^.kind = DTINTEGER) and (opnd2btn^.kind = DTLONGREAL) then
	    IntegerToReal(opnd1);
	    opnd1tn := longrealTypeNode;
	elsif (opnd1btn^.kind = DTLONGREAL) and (opnd2btn^.kind = DTINTEGER)
	    then 
	    IntegerToReal(opnd2);
	    opnd2tn := longrealTypeNode;
	end;
$end (* pascal *)
	opndType := Compatible(opnd1tn,opnd1,opnd2tn,opnd2);
	if opndType = nil then
	    ExprErrorName(en,stringToken[oper],
		    'Incompatible operands on binary operator $');
	else
	    opndType := BaseType(opndType);
	    en^.operType := opndType;
	    
	    case oper of
	    | TKEQUALS, TKNOTEQUAL, TKSHARP, 
	      TKLESS, TKGREATER, TKLSEQUAL, TKGREQUAL:
(* RELATIONAL *)
		if opndType^.kind in 
			DataTypeSet{DTINTEGER, DTBOOLEAN, DTCHAR, DTSTRING,
			DTREAL, DTLONGREAL, DTCARDINAL, DTENUMERATION} then
		    resultType := booleanTypeNode;
                elsif opndType = addressTypeNode then
		    en^.operType := cardinalTypeNode;
		    resultType := booleanTypeNode;
		elsif opndType^.kind = DTARRAY then
		    elementType := BaseType(opndType^.elementType);
		    if (elementType^.kind = DTCHAR) and
			(opndType^.arrayKind = ARRAYNORMAL) then
			resultType := booleanTypeNode;
		    end;
		elsif (opndType^.kind = DTSET)
$if modula2 then
			and not (oper in TokenSet{TKLESS, TKGREATER})
$end
		then
		    if oper = TKGREQUAL then
			(* change >= to <= with switched operands *)
			en^.opnd1 := opnd2;
			en^.opnd2 := opnd1;
			en^.exprBinOp := TKLSEQUAL;
		    end;
$if pascal then
		    if oper = TKGREATER then
			(* change > to < with switched operands *)
			en^.opnd1 := opnd2;
			en^.opnd2 := opnd1;
			en^.exprBinOp := TKLESS;
		    end;
		    SetSetType(en^.opnd1, opndType);
		    SetSetType(en^.opnd2, opndType);
		    en^.operType := opndType;
		    (* If set(s) on left are null set, they are STILL of
		       unknownSetType.  Fix them up *)
		    SetNilSetType(en^.opnd1, opndType);
		    SetNilSetType(en^.opnd2, opndType);
$end
		    resultType := booleanTypeNode;
		elsif (opndType^.kind in 
			DataTypeSet{DTPOINTER, DTOPAQUE, DTBYTE, DTWORD, DTPROC
$if pascal then
			, DTRECORD
$end
				    }) and
			(oper in TokenSet{TKEQUALS, TKNOTEQUAL, TKSHARP}) then
		    resultType := booleanTypeNode;
		elsif (opndType^.kind = DTDYNARRAY) and
			(oper in TokenSet{TKEQUALS, TKNOTEQUAL, TKSHARP}) then
		    resultType := booleanTypeNode;
		    (* Don't bother checking dimensions, too *)
		    en^.operType := addressTypeNode;
		    opnd1^.exprType := addressTypeNode;
		    opnd2^.exprType := addressTypeNode;
		end;

	    | TKPLUS, TKMINUS, TKASTERISK, TKSLASH, TKDIV, TKMOD:
(* NUMBER, SET ARITHMETIC *)
$if modula2 then
		if (opndType^.kind in DataTypeSet{DTINTEGER, DTCARDINAL}) and
			(oper # TKSLASH) then
		    resultType := opndType;
                elsif (opndType = addressTypeNode) and (oper # TKSLASH) then
		    en^.operType := cardinalTypeNode;
		    resultType := opndType;
$else (* pascal *)
		if opndType^.kind = DTINTEGER then
		    if oper = TKSLASH then
			IntegerToReal(opnd1);
			IntegerToReal(opnd2);
			en^.operType := longrealTypeNode;
		        resultType := longrealTypeNode;
		    else
			resultType := opndType;
		    end;
$end
		elsif (opndType^.kind in DataTypeSet{DTREAL,DTLONGREAL}) and
			not (oper in TokenSet{TKDIV,TKMOD})
		then
		    resultType := opndType;
		elsif (opndType^.kind = DTSET) and
			not (oper in TokenSet{TKDIV,TKMOD
$if pascal then
				,TKSLASH
$end
			})
		then
		    resultType := opndType;
		end;
	    | TKAND, TKOR, TKAMPERSAND:
(* BOOLEAN ARITHMETIC *)
		if opndType^.kind = DTBOOLEAN then
		    resultType := booleanTypeNode;
		end;
	    end (* case *);
	    if resultType = nil then
		ExprErrorName(en,stringToken[oper],
			'Operand types invalid for operator $');
	    end;
	end;
    end;
    if resultType = nil then
	BadExpr(en);
    else
	en^.exprType := resultType;
	assert ((opnd1^.baseVar = nil) or (opnd2^.baseVar = nil));
	assert ((opnd1^.basePtrType = nil) or (opnd2^.basePtrType = nil));
	if opnd1^.baseVar # nil then
	    en^.baseVar := opnd1^.baseVar;
	else
	    en^.baseVar := opnd2^.baseVar;
	end;
	if opnd1^.basePtrType # nil then
	    en^.basePtrType := opnd1^.basePtrType;
	else
	    en^.basePtrType := opnd2^.basePtrType;
	end;
	EvalConstExprBinOp(en);
	if en^.kind = EXPRCONST then
	    if mode = EVALPUT then
		ExprError(en, 'Cannot store into a constant expression');
		BadExpr(en);
	    end;
	elsif (mode # EVALGET) and (* not an EVALPOINT set *)
	    not ((mode = EVALPOINT) and (opnd1tn # nil) and 
		 (ActualType(opnd1tn)^.kind = DTSET)) then
	    ExprError(en, 'Cannot address or store into an expression');
	    BadExpr(en);
	end;
    end;
    return en^.exprType;
end CheckExprBinOp;

procedure AssignExprNode(toen, fromen : ExprNode);
(* Copy contents of fromen to toen, but preserve next field of toen. *)
var
    saveNext : ExprNode;
begin
    saveNext := toen^.next;
    toen^ := fromen^;
    toen^.next := saveNext;
end AssignExprNode;

procedure FactorInConstants(var en : ExprNode; 
	var total, multiple : MemoryOffset;
	multiply : boolean);
var
    olden : ExprNode;
begin
    olden := en;
    (* en will be multiplied by multiple and added to total *)
    (* update en, total, and multiple to try to improve things *)
    if en^.kind = EXPRBINOP then
	if en^.exprBinOp = TKPLUS then
	    if en^.opnd1^.kind = EXPRCONST then
		total := total + trunc(OrdOf(en^.opnd1^.exprConst)) * multiple;
		en := en^.opnd2;
	    elsif en^.opnd2^.kind = EXPRCONST then
		total := total + trunc(OrdOf(en^.opnd2^.exprConst)) * multiple;
		en := en^.opnd1;
	    end;
	elsif en^.exprBinOp = TKMINUS then
(* ||| For when xlate is better about pcneg
	    if en^.opnd1^.kind = EXPRCONST then
		total := total + trunc(OrdOf(en^.opnd1^.exprConst)) * multiple;
		en := MakeExprUnOp(cardIntTypeNode, TKMINUS, en^.opnd2);
	    els*)if en^.opnd2^.kind = EXPRCONST then
		total := total - trunc(OrdOf(en^.opnd2^.exprConst)) * multiple;
		en := en^.opnd1;
	    end;
	elsif multiply and (en^.exprBinOp = TKASTERISK) then
	    if en^.opnd1^.kind = EXPRCONST then
		multiple := multiple * trunc(OrdOf(en^.opnd1^.exprConst));
		en := en^.opnd2;
	    elsif en^.opnd2^.kind = EXPRCONST then
		multiple := multiple * trunc(OrdOf(en^.opnd2^.exprConst));
		en := en^.opnd1;
	    end;
	end;
    end;
    if en # olden then
	FactorInConstants(en,total,multiple,multiply);
    end;
end FactorInConstants;

(* NormalSubscriptExpr: build expression for normal array subscript *)
(* For a[i,j], aen represents the address of a, sen is next subscript *)
(*    Build expression as in (a + (i * rowsize)) + (j * elementsize) *)
(*    Each subscript is multiplied by the constant size of its dimension *)
(*    Building can stop when it runs out of subscripts *)
procedure NormalSubscriptExpr(arrtn    : TypeNode;
			      aen, sen : ExprNode; 
			      subnum   : integer;
			  var resulttn : TypeNode; 
			  var resulten : ExprNode);
var
    adden, multen, sennext : ExprNode;
    sentn : TypeNode;
    subMultiple, lowerBound, upperBound : MemoryOffset;
    baseOffset, subOffset, rowsize : MemoryOffset;
    error : boolean;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'NormalSubscriptExpr\n');
    end;
    adden := aen;
    baseOffset := 0;
    error := false;
    while sen # nil do
	sennext := sen^.next;
	arrtn := ActualType(arrtn);
	if arrtn^.kind # DTARRAY then
	    ExprErrorNumber(sen, 'Too many subscripts, subscript # %',subnum);
	    error := true;
	    exit while;
	end;
	assert(arrtn^.arrayKind = ARRAYNORMAL,'Open array in NormalSubscript?');
	sentn := CheckExpr(sen,EVALGET);
	if IsBadExpr(sen) then
	    error := true;
	elsif Assignable(arrtn^.indexType,sentn,sen) = nil then
	    ExprErrorNumber(sen,'Incompatible type, subscript # %',subnum);
	    error := true;
	else
	    (* index bounds *)
	    lowerBound := trunc(LowerBoundOf(arrtn^.indexType));
	    upperBound := trunc(UpperBoundOf(arrtn^.indexType));
	    subOffset := 0;     (* Adjustment to add to baseOffset *)
	    subMultiple := 1;   (* Adjustment to multiply to rowSize *)
	    if sen^.doCheck then
		(* No constant folding subscript expression into baseOffset *)
		(* or rowsize: range-check should be on original index *)
		sen := MakeExprCheck(sen, CHECKSUBSCR, nil, nil, sentn,
		    lowerBound, upperBound, nil);
	    else
		(* Extract any +/- constants in subscript into subOffset *)
		(* Extract any * constants in subscript into subMultiple *)
		FactorInConstants(sen, subOffset, subMultiple, true);
	    end;
	    (* size of row *)
	    rowsize := arrtn^.elementSize;
	    (* adjust baseOffset by lowerBound and constant +/- in subscript *)
	    baseOffset := baseOffset + (subOffset - lowerBound) * rowsize;

	    (* multiply subscript by rowsize and subMultiple *)
	    multen := MakeExprOffset(addressTypeNode,TKASTERISK, sen,
		longfloat(rowsize*subMultiple));

	    (* add to other subscripts *)
	    adden := MakeExprBinOp(addressTypeNode,TKPLUS,adden,multen);
	end;
	arrtn := arrtn^.elementType;
	sen := sennext;
	subnum := subnum + 1;
    end (* while *);
    if error then
	BadExpr(adden);
	resulttn := anyTypeNode;
	resulten := adden;
    else
	if baseOffset # 0 then
	    (* add in constant offset *)
	    adden := MakeExprOffset(addressTypeNode,TKPLUS,adden,
			longfloat(baseOffset));
	    adden^.baseVar := aen^.baseVar;
	    adden^.basePtrType := aen^.basePtrType;
	end;
	resulttn := arrtn;
	resulten := adden;
    end;
end NormalSubscriptExpr;

procedure GetArrayDesc(var aen  : ExprNode;
		       var desc : VarNode;
		       var save : boolean);
    var en : ExprNode;
begin
    save := false;
    en := aen;
    if (en^.kind = EXPRCHECK) and (en^.exprCheck = CHECKDYNARRAY) then
	en := en^.checkExpr;
    end;
    if en^.kind = EXPRVAR then
	(* use open array variable directly *)
	desc := en^.exprVar;
(* ||| I would like for indirect vars, which at the point are EXPRVAL over
    EXPRVAR, to not get a temporary.  But this would require changing things
    entirely.  Don't return a VarNode, but instead an ExprNode that can be
    copied.
*)
    else
	(* need to save array address for multiple uses *)
	desc := DefineVar(nil, addressTypeNode, MEMFAST, GSNORMAL, nil);
	aen := MakeExprSave(aen, desc);
	save := true;
    end;
end GetArrayDesc;


(* OpenSubscriptExpr: build an expression for an open/dynamic subscript *)
(* For a[i,j], aen is the address of the descriptor, sen is the first subscript *)
(*    For open build : (a + ((i * n2) + j) * n3 * ... * nk * elementsize) *)
(*    For subarray build : (a + ((i * s1) + j) * s2 * ... * sk ) *)
(*    Must multiply by all dimensions whether there is a subscript or not *)
procedure OpenSubscriptExpr(arrtn    : TypeNode;
			    aen,sen  : ExprNode; 
			var resulttn : TypeNode; 
			var resulten : ExprNode);
var
    countOffset, multOffset, offsetIncrement, subMultiple, subOffset, rowSize
	: MemoryOffset;
    kind : ArrayKind;
    error, anotherKind, doSave : boolean;
    arrayDesc : VarNode;
    adden, newadden, varen, sennext : ExprNode;
    sentn, rowtn, atn : TypeNode;
    subnum   : integer;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'OpenSubscriptExpr\n');
    end;
    error := false;
    subnum := 1;
    GetArrayDesc(aen, arrayDesc, doSave);
    kind := arrtn^.arrayKind;
    countOffset := WORDSIZE;
    multOffset := 2*WORDSIZE;
    if kind = ARRAYSUBARRAY then
	offsetIncrement := 2*WORDSIZE;
    else
	offsetIncrement := WORDSIZE;
    end;
    adden := nil;
    atn := arrtn;
    anotherKind := false;
    resulttn := atn;
    while (atn # nil) and not anotherKind do
	if sen # nil then
	    sennext := sen^.next;
	    sentn := CheckExpr(sen, EVALGET);
	    if IsBadExpr(sen) then
		error := true;
	    elsif Assignable(cardinalTypeNode, sentn, sen) = nil then
		ExprErrorNumber(sen,'Incompatible type, subscript # %',subnum);
		error := true;
	    end;
	    if sen^.doCheck and (kind # ARRAYNOCOUNT) then
		(* Check subscript *)
		sen := MakeExprCheck(sen, CHECKSUBSCROPEN, arrayDesc, nil,
		    sentn, countOffset, ord(doSave), nil);
	    end;
	    (* add sen to previous subscripts adden *)
	    adden := MakeExprBinChain(cardIntTypeNode, TKPLUS, adden, sen);
	    sen := sennext;
	    subnum := subnum + 1;
	    resulttn := atn^.elementType;
	end;
	rowSize := atn^.elementSize;
	(* see what the row type is *)
	rowtn := atn^.elementType;
	if (rowtn^.kind # DTARRAY) or (rowtn^.arrayKind = ARRAYNORMAL) then
	    anotherKind := true;
	end;
	(* if not a dynamic size array, exit and do constant multiply *)
	if (not anotherKind) or	(kind = ARRAYSUBARRAY) then
	    (* dynamic dimension, get number of elements from descriptor *)
	    varen := MakeExprVar(arrayDesc, aen);
	    if doSave then
		varen := MakeExprVal(addressTypeNode, varen);
	    end;
	    newadden :=
	     MakeExprOffset(addressTypeNode,TKPLUS,varen,longfloat(multOffset));
	    newadden := MakeExprVal(cardIntTypeNode, newadden);
	    (* multiply together *)
	    adden := MakeExprBinOp(cardIntTypeNode,TKASTERISK,adden,newadden);
	    (* continue with more dimensions *)
	    atn := rowtn;
	    countOffset := countOffset + offsetIncrement;
	    multOffset := multOffset + offsetIncrement;
	end;
    end (* while *);
    if not error then
	(* do indirection for base address *)
	RefOpenArray(aen,arrtn);
	(* multiply by size of element *)
	subMultiple := rowSize;
	(* first try to get constants out of subscripts *)
	subOffset := 0;
	FactorInConstants(adden,subOffset,subMultiple,true);
	if subMultiple # 0 then
	    adden := MakeExprOffset(addressTypeNode,TKASTERISK,adden,
			longfloat(subMultiple));
	    (* add base to subscript expression *)
	    aen := MakeExprBinOp(addressTypeNode,TKPLUS,aen,adden);
	end;
	if subOffset # 0 then
	    aen :=
		MakeExprOffset(addressTypeNode,TKPLUS,aen,longfloat(subOffset));
	end;
	(* if saved variable address, don't allow reordering *)
	aen^.optNoReorder := doSave;
	if sen # nil then
	    rowtn := ActualType(rowtn);
	    if rowtn^.kind = DTARRAY then
		NormalSubscriptExpr(rowtn,aen,sen,subnum,resulttn,resulten);
	    else
		ExprErrorNumber(aen,
		    'Too many subscripts, subscript # %', subnum);
		error := true;
	    end;
	else
	    resulten := aen;
	end;
    end;
    if error then
	resulttn := anyTypeNode;
	resulten := aen;
	BadExpr(aen);
    end;
end OpenSubscriptExpr;

procedure CheckExprSubscript(en : ExprNode; mode : EvalMode) : TypeNode;
var
    aen, sen, resulten : ExprNode;
    arrtn, resulttn : TypeNode;
    error : boolean;
begin
    error := false;
    aen := en^.arr;
    sen := en^.subscripts^.first;
    if mode = EVALPUT then
	arrtn := ActualType(CheckExpr(aen,EVALPUT));
    else
	arrtn := ActualType(CheckExpr(aen,EVALPOINT));
    end;
    if IsBadExpr(aen) or (arrtn^.kind = DTANY) then
	(* do nothing *)
	error := true;
    elsif arrtn^.kind # DTARRAY then
	ExprError(en,'Subscripted expression not an array');
	error := true;
    else
	(* Process subscripts according to kind of array *)
	case arrtn^.arrayKind of
	| ARRAYNORMAL :
	    NormalSubscriptExpr(arrtn, aen, sen, 1, resulttn, resulten);
	
	| ARRAYNOCOUNT, ARRAYOPEN, ARRAYSUBARRAY :
	    OpenSubscriptExpr(arrtn, aen, sen, resulttn, resulten);
	    
	end;
	error := IsBadExpr(resulten);
    end;
    if error then
	resulttn := anyTypeNode;
	BadExpr(en);
    else
	AssignExprNode(en,resulten);
	ValueOrAddr(en,resulttn,mode);
    end;
    return resulttn;
end CheckExprSubscript;

procedure CheckExprDot(en : ExprNode; mode : EvalMode) : TypeNode;
var
    rectn, fieldtn : TypeNode;
    sym : Symbol;
    rec, saveNext : ExprNode;
    field : FieldNode;
    offset, multiple : MemoryOffset;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprDot\n');
    end;
    if mode = EVALPUT then
	rectn := ActualType(CheckExpr(en^.rec,EVALPUT));
    else
	rectn := ActualType(CheckExpr(en^.rec,EVALPOINT));
    end;
    rec := en^.rec;
    field := nil;
    if IsBadExpr(rec) or (rectn^.kind = DTANY) then
	(* do nothing *)
    elsif rectn^.kind # DTRECORD then
	ExprError(en,'Dot follows non-record expression');
    elsif en^.field # nil then
	field := en^.field;
    else
	sym := LookUpSymbol(en^.fieldName,rectn^.recScope);
	if sym = nil then
	    ExprErrorName(en,en^.fieldName,'$ is not a field of this record');
	    if DefineSymbol(sym, en^.fieldName, rectn^.recScope,
			    ONECASE, SYMFIELD) then
		sym^.symField := nil;
	    end;
	else
	    field := sym^.symField;
	end;
    end;
    if field = nil then
	BadExpr(en);
	fieldtn := nil;
    else
	fieldtn := StoredType(field^.fieldType);
	(* combine field offset with any other constants *)
	offset := field^.offset;
	multiple := 1;
	FactorInConstants(rec,offset,multiple,false);
	if rec^.doCheck and (field^.containingVariant # nil) and
		(field^.containingVariant^.tagField # nil) then
	    rec := MakeExprCheck(rec, CHECKVARIANT, nil, nil, nil,
			    offset-field^.offset, 0, field^.containingVariant);
	end;
	if offset = 0 then
	    (* no need to do add *)
	    AssignExprNode(en,rec);
	else
	    (* add in offset *)
	    en^.kind := EXPRBINOP;
	    en^.exprBinOp := TKPLUS;
	    en^.opnd1 := rec;
	    en^.opnd2 := MakeExprConst(addressTypeNode, longfloat(offset));
	    en^.baseVar := rec^.baseVar;
	    en^.basePtrType := rec^.basePtrType;
	    en^.exprType := addressTypeNode;
	    en^.operType := addressTypeNode;
	end;
	ValueOrAddr(en,fieldtn,mode);
    end;
    return fieldtn;
end CheckExprDot;

procedure CheckExprDeref(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn, ptrtn, atn : TypeNode;
    ptr, saveNext, conen : ExprNode;
    cn : ConstNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprDeref\n');
    end;
    tn := ActualType(CheckExpr(en^.ptr,EVALPOINT));
    ptrtn := nil;
    if (tn = nil) or (tn^.kind = DTANY) then
	(* do nothing *)
	BadExpr(en);
    elsif tn^.kind = DTPOINTER then
	(* pointer, get value *)
	if (en^.ptr^.kind = EXPRFUNC) and (en^.ptr^.func^.kind # EXPRSYM)  then
	    (* Function call automatically returns value *)
	else
	    en^.ptr := MakeExprVal(tn,en^.ptr);
	end;
	ptrtn := StoredType(tn^.toType);
	if ptrtn = nil then
	    ExprError(en,'Dereference of pointer to unknown type');
	    BadExpr(en);
	else
	    (* loaded pointer, make address depend on pointer type *)
	    en^.ptr^.basePtrType := ptrtn;
	    ptr := en^.ptr;
	    (* convert deref expr into check of appropriate kind *)
	    en^.kind := EXPRCHECK;
	    en^.exprCheck := CHECKPTRREF;
	    en^.checkPtr := tn^.ptrKind;
	    en^.checkExpr := ptr;
	    en^.checkVar := nil;
	    en^.checkType := nil;
	    en^.checkVariant := nil;
	    en^.checkLower := 0;
	    en^.checkUpper := 0;
	    en^.exprType := ptr^.exprType;
	    en^.basePtrType := ptr^.basePtrType;
	    en^.baseVar := nil;
	    ValueOrAddr(en,ptrtn,mode);
	end;
$if pascal then
    elsif tn^.kind = DTFILE then
	(* File buffer variable dereference - address of file on stack *)
	(* Just change this into a dereference of an FNIL call *)
	ptr := en^.ptr;
	ptrtn := ActualType(tn^.fileType);
	new(cn);
	cn^.kind := DTPROC;
	cn^.procVal := fnilProcNode;
	en^.kind := EXPRFUNC;
	en^.selected := false;
	en^.addrOfValue := nil;
	en^.func := NewExprNode(EXPRCONST);
	SameExprLine(en^.func, en);
	en^.func^.exprConst := cn;
	en^.func^.exprType := ptrtn;
	en^.func^.constType := ptrtn; (* CED 6/21/88 *)
	en^.params := AddToExprList(nil, ptr);
	(* Tell code gen to use EVALPOINT on this fnil call *)
	conen := MakeExprConst(cardinalTypeNode, longfloat(ord(EVALPOINT)));
	en^.params := AddToExprList(en^.params, conen);
	en^.exprType := ptrtn;
	(* Since this is not a real pointer, leave value dependent on var,
	   not file element type. *)
	en^.basePtrType := nil;
	en^.baseVar := ptr^.baseVar;
	ValueOrAddr(en,ptrtn,mode);
$end
    elsif tn^.kind = DTDYNARRAY then
	(* dynamic array, leave descriptor address for subscript routine *)
	(* type is type of array pointed to *)
	ptrtn := tn^.dynArrayType;
	ptr := en^.ptr;
	(* convert deref into check for dynamic array *)
	en^.kind := EXPRCHECK;
	en^.exprCheck := CHECKDYNARRAY;
	en^.checkExpr := ptr;
	en^.checkPtr := tn^.dynArrayKind;
	en^.checkVar := nil;
	en^.checkType := nil;
	en^.checkVariant := nil;
	en^.checkLower := 0;
	en^.checkUpper := 0;
	en^.exprType := addressTypeNode;
	atn := tn^.dynArrayType;
	while (atn^.kind = DTARRAY) and
		(atn^.arrayKind in ArrayKindSet{ARRAYOPEN,ARRAYNOCOUNT}) do
	    atn := atn^.elementType;
	end;
	(* dynamic array is pointer eventually to element type *)
	en^.baseVar := nil;
	en^.basePtrType := atn;
    else
	ExprError(en,'Dereference of a non-pointer');
	BadExpr(en);
    end;
    return ptrtn;
end CheckExprDeref;

procedure CheckExprRange(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn : TypeNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprRange\n');
    end;
    tn := CheckExpr(en^.exprRangeIndex,mode);
    tn := CheckExpr(en^.exprRangeCount,mode);
    ExprError(en,'Range expression valid only in actual parameter of open array');
    tn := nil;
    BadExpr(en);
    return tn;
end CheckExprRange;


procedure ScaleBySize(en : ExprNode; size, unit : MemoryOffset) : ExprNode;
begin
    if size = unit then		    (* units are correct *)
	return en;
    elsif size mod unit = 0 then    (* size is a multiple of units *)
	return
	 MakeExprOffset(cardIntTypeNode,TKASTERISK,en,longfloat(size div unit));
    else			    (* size not a multiple of units, round up *)
	en := MakeExprOffset(cardIntTypeNode,TKASTERISK,en,longfloat(size));
	en := MakeExprOffset(cardIntTypeNode,TKPLUS,en,longfloat(unit-1));
	return MakeExprOffset(cardIntTypeNode,TKDIV,en,longfloat(unit));
    end;
end ScaleBySize;

$if modula2 then
(* CheckOpenArrayParam: check an open array parameter.
    Cases to handle:
    Formal		Actual		Generate
I   array of word/byte	non-array	addr, fixed scaled count
    array of word/byte	regular array	addr, fixed scaled count
    array of word/byte	open array	addr, var scaled count
II  nocount array	1-d array	addr
III open array		regular array	addr, fixed count, ...
    open array		open array	addr, var count, ...
    sub array		regular array	addr, fixed count, fixed mult, ...
    sub array		open array	addr, var count, fixed mult, ...
    sub array		sub array	addr, var count, var mult, ...
    sub array		array section	addr, var count, var mult, ...
IV  string		char array	addr, fixed count
    string		char subarray	addr, fixed count, 1
    char constant	char array	addr, 1
    char constant	char subarray	addr, 1, 1
    Note: open/sub arrays may have elements that are fixed arrays, but
	not vice versa.
*)
procedure CheckOpenArrayParam(pexp      : ExprNode;
			      parm      : ParamNode;
		          var newParams : ExprList;
			      pnum      : integer) : boolean;
var
    aen, sen, sennext, currMult, descriptor,
	subscript, adden, newcount, count, nextcount : ExprNode;
    aarrtn, atn, farrtn, ftn, senctn : TypeNode;
    subnum : integer;
    multActive, done, error, doSave, arrayofword : boolean;
    countList : ExprList;
    arrayDesc, saveSubVar : VarNode;
    fkind, akind : ArrayKind;
    countOffset, multOffset, offsetIncrement, conMult, conAdd : MemoryOffset;

(*
Return ExprNode with the number of elements in the current dimension of
the actual parameter 
*)
(**)procedure DimensionCount (): ExprNode;
var en : ExprNode;
begin
    if atn^.arrayKind = ARRAYNORMAL then
	return MakeExprConst(cardIntTypeNode,NumberOf(atn^.indexType));
    else
	(* get address of descriptor plus current offset, then do indirect *)
	en := MakeExprVar(arrayDesc, aen);
	if doSave then
	    en := MakeExprVal(addressTypeNode, en);
	end;
	en := MakeExprOffset(addressTypeNode,TKPLUS,en,longfloat(countOffset));
	return MakeExprVal(cardIntTypeNode, en);
    end;
end DimensionCount;

(*
Return ExprNode with the multiplier to use for the current dimension.
NIL means 1, which in turn means no multiply needed.
*)
(**)procedure MultiplierFactor (): ExprNode;
    var en : ExprNode;
	elements : MemoryOffset;
begin
    if atn^.arrayKind = ARRAYNORMAL then
	if atn^.elementType^.kind = DTARRAY then
	    elements :=	atn^.elementSize div atn^.elementType^.elementSize;
	    if (elements * atn^.elementType^.elementSize) # atn^.elementSize
	    then
		ExprError(en,
	"Can't index open array arg if size is not a multiple of element size");
	    end;
	    return MakeExprConst(cardIntTypeNode, longfloat(elements));
	else
	    return nil;
	end;
    else
	(* if last variable dimension of open array, use 1 *)
	if akind = ARRAYSUBARRAY then
	    (* subarray: use variable multiplier code below *)
	elsif atn^.elementType^.kind # DTARRAY then
	    (* not an array, must be last dimension *)
	    return nil;
	elsif atn^.elementType^.arrayKind = ARRAYNORMAL then
	    (* normal array, treat as fixed dimension *)
	    return MakeExprConst(cardIntTypeNode,
		    NumberOf(atn^.elementType^.indexType));
	end;
	(* get address of descriptor plus current offset, then do indirect *)
	en := MakeExprVar(arrayDesc, aen);
	if doSave then
	    en := MakeExprVal(addressTypeNode,en);
	end;
	en := MakeExprOffset(addressTypeNode,TKPLUS,en,longfloat(multOffset));
	return MakeExprVal(cardIntTypeNode,en);
    end;
end MultiplierFactor;

(*
Evaluate next subscript, check range if needed, offset to 0-based, and
add into previous subscripts.  Finally, multiply by size of dimension.
*)
(**)procedure AddInSubscript;
var
    sentn : TypeNode;
    lowerBound, upperBound : MemoryOffset;
begin
    sentn := CheckExpr(sen,EVALGET);
    if IsBadExpr(sen) then
	(* do nothing *)
	error := true;
    elsif atn^.arrayKind = ARRAYNORMAL then
	(* normal array: check type, subtract lower bound *)
	if Assignable(atn^.indexType,sentn,sen) = nil then
	    ExprErrorNumber(sen,'Incompatible type, subscript # %',subnum);
	    error := true;
	else
	    lowerBound := trunc(LowerBoundOf(atn^.indexType));
	    upperBound := trunc(UpperBoundOf(atn^.indexType));
	    if sen^.doCheck then
		(* Check subscript *)
		sen := MakeExprCheck(sen,CHECKSUBSCR,nil,nil,sentn,
		    lowerBound,upperBound,nil);
	    end;
	    if lowerBound # 0 then
		sen := MakeExprOffset(cardIntTypeNode,TKMINUS,sen,
			longfloat(lowerBound));
	    end;
	end;
    else
	(* open array: cardinal type, 0 based *)
	if Assignable(cardinalTypeNode,sentn,sen) = nil then
	    ExprErrorNumber(sen,
		'Index must be assignable to CARDINAL, subscript # %',subnum);
	    error := true;
	else
	    if sen^.doCheck and (akind # ARRAYNOCOUNT) then
		(* Check subscript *)
		sen := MakeExprCheck(sen,CHECKSUBSCROPEN,arrayDesc,nil,sentn,
		    countOffset,ord(doSave),nil);
	    end;
	end;
    end;
    if not error then
	(* add in previous subscripts (if any) *)
	subscript := MakeExprBinChain(cardIntTypeNode,TKPLUS,subscript,sen);
	subscript := MakeExprBinChain(cardIntTypeNode,TKASTERISK,subscript,
			MultiplierFactor());
    end;
    sen := sennext;
    subnum := subnum + 1;
end AddInSubscript;

(**)procedure StartMultiple;
begin
    if fkind = ARRAYSUBARRAY then
	currMult := MultiplierFactor();
	multActive := true;
    end;
end StartMultiple;

(**)procedure EndMultiple;
begin
    if multActive then (* subarray *)
	if currMult = nil then
	    currMult := MakeExprConst(cardIntTypeNode,1.0);
	end;
	if DEBUG and TraceNexpr then
	    Writef(output,'Multiple = ');
	    WriteExpr(currMult);
	    Writec(output, '\n');
	end;
	countList := AddToExprList(countList,currMult);
	currMult := nil;
	multActive := false;
    end;
end EndMultiple;

    var i, dimensions : cardinal;
	elementSize   : MemoryOffset;

begin (* CheckOpenArrayParam *)
    if DEBUG and TraceNexpr then
	Writef(output,'CheckOpenArrayParam:');
	WriteExpr(pexp);
	Writec(output, '\n');
    end;
    elementSize := -1;
    countOffset := 0;
    multOffset := 0;
    offsetIncrement := 0;
    error := false;
    if pexp^.kind = EXPRSUBSCR then
	aen := pexp^.arr;
	sen := pexp^.subscripts^.first;
    else
	(* make it look like an array with no subscripts*)
	aen := pexp;
	sen := nil;
    end;

    (* get types of actual array and formal open/sub array *)
    if parm^.kind = PARAMARRAYVAR then
	aarrtn := CheckExpr(aen, EVALPUT);
    else
	aarrtn := CheckExpr(aen, EVALPOINT);
    end;
    aarrtn := BaseType(aarrtn);
    farrtn := parm^.paramType;
    ftn := farrtn;
    fkind := ftn^.arrayKind;
    arrayofword := (ftn^.arrayKind in ArrayKindSet{ARRAYOPEN,ARRAYNOCOUNT})
	    and    ((ftn^.elementType = wordTypeNode)
                            or (ftn^.elementType = packedByteTypeNode));

    arrayDesc := nil;
    if IsBadExpr(aen) then
	error := true;
    else
	(* Process subscripts for open dimensions			    *)
	(* Produce two things: expr for base addr (a la CheckExprSubscript) *)
	(* and list of counts and multipliers for rest of parameter	    *)
	(* This code is reminiscent of OpenSubscriptExpr followed by	    *)
	(* NormalSubscriptExpr, since in the worst case, that's what the    *)
	(* actual array parameter is.					    *)
	(* Each iteration advances atn and moves one step through the       *)
	(* subscript list, if it has not been exhausted.  A range subscript *)
	(* or a missing subscript implies a dimension for the parameter,    *)
	(* which advances ftn.						    *)
	(* The processing stops when it gets to a non-open dimension in the *)
	(* formal parameter.						    *)
	atn := aarrtn;	(* for current dimension of actual parameter *)
	countList := AddToExprList(nil,nil); (* counts and multipliers *)
	currMult := nil;	(* current multiplier *)
	multActive := false;
	subscript := nil;
	if (aen^.kind = EXPRDESCRIPTOR) or (atn^.kind # DTARRAY) then
	    (* see special processing at end of while loop *)
	    done := true; 	(* pop out asap *)
	else
	    akind := atn^.arrayKind;
	    done := false;
	    if akind in ArrayKindSet{ARRAYOPEN, ARRAYSUBARRAY} then
		(* save addr of array descriptor, will need it several times *)
		GetArrayDesc(aen, arrayDesc, doSave);
		(* variables for offsets into open/sub actual array parameter *)
		countOffset := WORDSIZE;
		multOffset := 2*WORDSIZE;
		if akind = ARRAYSUBARRAY then
		    if fkind # ARRAYSUBARRAY then
			ExprError(aen,
			    'SUBARRAY can only be passed to a SUBARRAY');
		    end;
		    offsetIncrement := 2*WORDSIZE;
		else
		    offsetIncrement := WORDSIZE;
		end;
	    elsif (akind = ARRAYNOCOUNT) and (fkind # ARRAYNOCOUNT) and
		    ((sen = nil) or (sen^.kind # EXPRRANGE)) and 
		    not arrayofword then
(* ||| This still lets thru an entire @nocount to array of word *)
		ExprError(aen, 
		      '@NOCOUNT array can only be passed to an @NOCOUNT array');
		error := true;
	    end;
	end;
	while not error and not done do
	    (* Three possibilities:
		A. sen = nil
		    Flush multiplier
		    Add next dimension to parameter
		    Start multiplier
		B. sen = range
		    Flush multiplier
		    Add next dimension to parameter
		    Add index to subscript
		    Start multiplier
		C. sen = value
		    Fold in multiplier
		    Add index to subscript
	    *)
	    if sen = nil then
		EndMultiple;
		if fkind # ARRAYNOCOUNT then
		    (* add current dimension count to list *)
		    countList := AddToExprList(countList, DimensionCount());
		end;
		(* multiply earlier subscripts, even though there aren't more *)
		if subscript # nil then
		    subscript := MakeExprBinChain(cardIntTypeNode,TKASTERISK,
				    subscript, MultiplierFactor());
		end;
		StartMultiple;
		(* consider this another dimension *)
		if not arrayofword then
		    ftn := ftn^.elementType;
		end;
	    elsif sen^.kind = EXPRRANGE then
		sennext := sen^.next;
		EndMultiple;
		(* add count as new dimension *)
		newcount := sen^.exprRangeCount;
		senctn := CheckExpr(newcount,EVALGET);
		if IsBadExpr(newcount) then
		    error := true;
		elsif Assignable(cardinalTypeNode,senctn,newcount) = nil then
		    ExprErrorNumber(sen,
		       'Number of elements must be CARDINAL, subscript # %',
			subnum);
		    error := true;
		else
		    sen := sen^.exprRangeIndex;
		    if fkind # ARRAYNOCOUNT then
			if newcount^.doCheck and (akind # ARRAYNOCOUNT) then
			    (* save subscript for count check *)
			    saveSubVar := DefineVar(nil, cardIntTypeNode,
				    MEMFAST,GSNORMAL, nil);
			    sen := MakeExprSave(sen,saveSubVar);
			    if akind = ARRAYNORMAL then
				(* check against fixed upper bound *)
				newcount := MakeExprCheck(newcount,
				    CHECKSUBARRAY, nil, saveSubVar, nil,
				    trunc(UpperBoundOf(atn^.indexType)), 0,nil);
			    else
				(* check against variable upper bound *)
				newcount := MakeExprCheck(newcount,
				    CHECKSUBARRAY, arrayDesc, saveSubVar, nil,
				    countOffset, ord(doSave), nil);
			    end;
			end;
			(* add count to list *)
			countList := AddToExprList(countList,newcount);
		    end;
		    AddInSubscript;
		    StartMultiple;
		    (* consider this another dimension *)
		    if (sennext # nil) and (fkind # ARRAYSUBARRAY) then
			ExprErrorNumber(sen,
			'Parameter # %: slicing only allowed on last dimension',
			    pnum);
			error := true;
		    elsif (ftn^.kind # DTARRAY) or 
			    (ftn^.arrayKind = ARRAYNORMAL) then
			ExprErrorNumber(aen,
			   'Too many dimensions for array parameter # %', pnum);
			error := true;
		    elsif not arrayofword then
			ftn := ftn^.elementType;
		    end;
		end;
	    else
		(* just a subscript *)
		sennext := sen^.next;
		if multActive then
		    (* Multiply exising multipler by this dimension's *)
		    currMult := MakeExprBinChain(cardIntTypeNode,TKASTERISK,
			currMult, MultiplierFactor());
		end;
		AddInSubscript;
	    end;
	    (* continue with next subscript or dimension *)
	    elementSize := atn^.elementSize;
	    atn := atn^.elementType;
	    countOffset := countOffset + offsetIncrement;
	    multOffset := multOffset + offsetIncrement;
	    if atn^.kind # DTARRAY then
		done := true;
	    elsif (ftn^.kind # DTARRAY) or (ftn^.arrayKind = ARRAYNORMAL) and
		    (sen = nil) then
		done := true;
	    end;
	end;
    end;

    (* ftn is element type of formal; atn, of actual *) 
    if error then
	(* do nothing *)
    elsif sen # nil then
	ExprErrorNumber(sen,'Too many subscripts, subscript # %',subnum);
	error := true;
    elsif StoredType(ftn) = StoredType(atn) then
	(* type OK *)
    elsif atn^.kind = DTANY then
	(* Don't bother displaying error with error *)
	error := true;
    elsif aen^.kind = EXPRDESCRIPTOR then
	(* Special descriptor compatibility *)
	(* Find element type of open array, number of open dimensions *)
	dimensions := 0;
	while (ftn^.kind = DTARRAY) and (ftn^.arrayKind # ARRAYNORMAL) do
	    inc(dimensions);
	    ftn := ftn^.elementType;
	end;
	if ftn # ActualType(aen^.exprType^.descripType) then
	    ExprError(aen,
		'Descriptor element type not the same as array element type');
	    error := true;
	end;
	if fkind = ARRAYSUBARRAY then
	    ExprError(aen, 'Cannot create descriptor for SUBARRAY (yet?)');
	    error := true;
	end;
	if aen^.descripCount = 0 then
	    (* NIL descriptor *)
	    if parm^.kind = PARAMARRAYVALUE then
		ExprError(aen,
		    'Cannot pass NIL descriptor to pass-by-value array');
	    end;
	    for i := 1 to dimensions do
		aen^.descrips := AddToExprList(aen^.descrips,
		    MakeExprConst(cardIntTypeNode, 0.0));
	    end;
	    aen^.descripCount := dimensions;
	else
	    (* Normal descriptor *)
	    if aen^.descripCount # dimensions then
		ExprErrorNumber(aen, 'Descriptor should have % dimensions',
		    dimensions);
	    end;
	end;
	newParams := AddToExprList(newParams, aen);
	return error;
		
    elsif ftn^.kind = DTARRAY then
	(* special array compatibility : word, byte, char, string *)
	if arrayofword then
	    (* array of word or byte, scale size *)
	    if fkind = ARRAYOPEN then
		(* Compute total number of elements *)
		newcount := countList^.first;
		if newcount = nil then
		    newcount := MakeExprConst(cardIntTypeNode, 1.0);
		else
		    (* multiply list of counts together *)
		    count := newcount^.next;
		    while count # nil do
			nextcount := count^.next;
			newcount := MakeExprBinOp(cardIntTypeNode, TKASTERISK,
                                newcount,count);
                        count := nextcount;
                    end;
                end;
		(* scale count and put back on list (only element) *)
		elementSize := ftn^.elementSize;
		countList^.first :=
		    ScaleBySize(newcount, SizeOf(atn), elementSize);
		countList^.last := countList^.first;
		countList^.last^.next := nil;
	    end;
	elsif (atn^.kind = DTSTRING) and 
		(ftn^.elementType = packedCharTypeNode) then
	    (* string for array of char *)
	    elementSize := CHARSIZE;
	    if fkind # ARRAYNOCOUNT then
		(* add count *)
		newcount := MakeExprConst(cardIntTypeNode,
			longfloat(atn^.size div CHARSIZE));
		countList := AddToExprList(countList,newcount);
		if fkind = ARRAYSUBARRAY then
		    (* add multiplier *)
		    newcount := MakeExprConst(cardIntTypeNode,1.0);
		    countList := AddToExprList(countList,newcount);
		end;
	    end;
	elsif (atn^.kind = DTCHAR) and (aen^.kind = EXPRCONST)
                and (ftn^.elementType = packedCharTypeNode) then
	    (* single char constant for array of char *)
	    elementSize := CHARSIZE;
	    atn := NewTypeNode(DTSTRING);
            atn^.stringLength := 1;
            if standardStringFlag then
                atn^.size := CHARSIZE;
            else
                atn^.size := 2*CHARSIZE;
            end;
            aen^.constType := atn;
	    if fkind # ARRAYNOCOUNT then
		(* add count *)
		if standardStringFlag then
		    newcount := MakeExprConst(cardIntTypeNode,1.0);
		else
		    newcount := MakeExprConst(cardIntTypeNode,2.0);
		end;
		countList := AddToExprList(countList,newcount);
		if fkind = ARRAYSUBARRAY then
		    (* add multiplier *)
		    newcount := MakeExprConst(cardIntTypeNode,1.0);
		    countList := AddToExprList(countList,newcount);
		end;
	    end;
	else
	    ExprErrorNumber(aen,
	       'Actual array element type (parameter # %) differs from formal',
	       pnum);
	    error := true;
	end;
    else
	ExprErrorNumber(aen,
	    'Actual array element type (parameter # %) differs from formal',
	    pnum);
	error := true;
    end;
    if not error then
	(* add last multiplier to list *)
	EndMultiple;
	(* finish base address calculation: multiply subscripts by  *)
	(* element size and add in base address			    *)
	RefOpenArray(aen,aarrtn);
	if subscript # nil then
            conAdd := 0;
            conMult := 1;
            FactorInConstants(aen,conAdd,conMult,false);
	    if elementSize = -1 then
		ExprError(aen, "CheckOpenArrayParam: Didn't set elementSize?");
	    end;
	    conMult := elementSize;
            FactorInConstants(subscript,conAdd,conMult,true);
	    adden := MakeExprOffset(addressTypeNode,TKASTERISK,subscript,
			longfloat(conMult));
	    aen := MakeExprBinOp(addressTypeNode,TKPLUS,aen,adden);
            if conAdd # 0 then
                aen := MakeExprOffset(addressTypeNode,TKPLUS,aen,
			longfloat(conAdd));
            end;
	end;
	if DEBUG and TraceNexpr then
	    Writef(output,'Base address = ');
	    WriteExpr(aen);
	    Writec(output, '\n');
	end;
	descriptor := NewExprNode(EXPRDESCRIPTOR);
	descriptor^.exprType := farrtn;
	descriptor^.baseVar := aen^.baseVar;
	descriptor^.basePtrType := aen^.basePtrType;
	descriptor^.descripBase := aen;
	descriptor^.descripMode := EVALPOINT;
	descriptor^.descripCount := farrtn^.descripCount;
	descriptor^.descrips := countList;

	newParams := AddToExprList(newParams,descriptor);
    end;
    return error;
end CheckOpenArrayParam;
$end (* modula2 *)

procedure CheckParam(pexp : ExprNode; parm : ParamNode; newParams : ExprList;
	    errorName : String; pnum : integer) : boolean;
var
    mode : EvalMode;
    parmtn, pexptn, pexpbtn : TypeNode;
    error : boolean;
begin
    error := false;
    if parm^.kind = PARAMVAR then
	mode := EVALPUT;
    elsif parm^.reference then
	mode := EVALPOINT;
    else
	mode := EVALGET;
    end;
    pexptn := CheckExpr(pexp,mode);
    if IsBadExpr(pexp) or (pexptn^.kind = DTANY) then
	error := true;
    else
	parmtn := parm^.paramType;
$if pascal then
	if parmtn^.kind = DTSET then
	    if pexptn = unknownSetTypeNode then
		pexptn := parmtn;
	    end;
	    SetSetType(pexp, pexptn);
	end;
$end
	if not Passable(parmtn,parm^.kind,pexptn,pexp) then
	    ExprErrorNameNumber(pexp,errorName, 
		    'Wrong type, procedure $ parameter # %',pnum);
	    error := true;
	elsif (parm^.kind = PARAMVAR) and not IsAddressableExpr(pexp) then
	    ExprErrorNameNumber(pexp,errorName,
		'VAR parameter not variable, $ parameter #%',pnum);
	    error := true;
	elsif (parm^.kind = PARAMVAR) and not IsAddressableType(pexptn) then
	    ExprErrorNameNumber(pexp,errorName,
                'VAR parameter not addressable, $ parameter #%',pnum);
            error := true;
	else
	    if pexp^.doCheck and (parm^.kind in 
		    ParamKindSet{PARAMVALUE,PARAMCONST}) then
		if ((parmtn^.kind = DTSUBRANGE) or 
			(parmtn = cardIntTypeNode)) then
		    pexp := MakeExprCheck(pexp,CHECKRANGE,nil,nil, pexptn,
			    trunc(LowerBoundOf(parmtn)),
			    trunc(UpperBoundOf(parmtn)),nil);
		elsif (((parmtn^.kind = DTINTEGER) and
			(pexptn^.kind = DTCARDINAL))
		    or ((parmtn^.kind = DTCARDINAL) and
			(pexptn^.kind = DTINTEGER)))
	      and (standardCardinalFlag) then
		    pexp := MakeExprCheck(pexp,CHECKRANGE,nil,nil,pexptn,
			0, trunc(MAXINT), nil);
		end;
	    end;
$if pascal then
	    if (parm^.kind = PARAMVALUE) and (pexptn^.containsFiles) then
		ExprError(pexp, 'Files cannot be passed by value');
	    end;
$end
	    if (target # TARGETVAX) and parm^.reference and
		    ((AlignmentOf(pexptn) mod WORDSIZE) # 0) and
		    ((AlignmentOf(parmtn) mod WORDSIZE) = 0) then
		ExprErrorNameNumber(pexp, errorName,
       'Argument must be word-aligned if parameter is word-aligned, $ param #%',
		    pnum);
	    end;
	    newParams := AddToExprList(newParams,pexp);
	end;
    end;
    return error;
end CheckParam;


procedure CheckFuncProc(const stn          : StmtNode;
			const procValExpr  : ExprNode;
			const procNameExpr : ExprNode;
			var   params       : ExprList; 
			var   retType      : TypeNode; 
			const mode         : EvalMode) : boolean;
var
    parm : ParamNode;
    pexp, pexpnext : ExprNode;
    pnum : integer;
    tn, at : TypeNode;
    error : boolean;
    proc : ProcNode;
    procType, pexptn : TypeNode;
    newParams : ExprList;
    errorName : String;
begin
    error := true;
    procType := procNameExpr^.exprType;
    errorName := nil;
    if procNameExpr^.kind = EXPRSYM then
	errorName := procNameExpr^.exprSym^.name;
    elsif procNameExpr^.kind = EXPRCONST then
	if procNameExpr^.exprConst^.kind = DTPROC then
	    errorName := procNameExpr^.exprConst^.procVal^.name;
	end;
    end;
    
$if modula2 then
    (* beware: type names can be used as both types and funcs *)
    if (procNameExpr^.kind = EXPRSYM) and
	    (procNameExpr^.exprSym^.kind = SYMTYPE) then
	retType := procNameExpr^.exprSym^.symType;
	if params^.first = nil then
	    ExprErrorName(procNameExpr,errorName,
		    'Type transfer function $ requires a parameter');
	elsif params^.first^.next # nil then
	    ExprErrorName(procNameExpr,errorName,
		    'Type transfer function $ must have only one parameter');
	else
	    pexptn := CheckExpr(params^.first,mode);
	    if pexptn = nil then
		(* found an error *)
	    elsif WordSizeOf(pexptn) # WordSizeOf(retType) then
		ExprErrorName(procNameExpr,errorName,
		 'Type transfer function $ cannot change size');
	    elsif (pexptn^.kind = DTARRAY) and 
		    (pexptn^.arrayKind # ARRAYNORMAL) then
		ExprErrorName(procNameExpr,errorName,
		 "Type transfer function $ can't change type of an open array");
	    elsif (retType^.kind = DTARRAY) and
		    (retType^.arrayKind # ARRAYNORMAL) then
		ExprErrorName(procNameExpr,errorName,
		 "Type transfer function $ can't change type to an open array");
	    else 
		error := false;
		procValExpr^.baseVar := params^.first^.baseVar;
		procValExpr^.basePtrType := params^.first^.basePtrType;
		if mode # EVALGET then
		    at := ActualType(retType);
		    if (at^.kind in DataTypeSet{DTARRAY, DTRECORD}) or
			    ((at^.kind = DTDYNARRAY) and 
			     (SizeOf(at) > WORDSIZE)) then
			procValExpr^.selected := true;
		    end;
		end;
	    end;
	end;
	return not error;
    end;
$end (* modula2 *)

    
    (* check for builtin function (must be a constant) *)
    if procNameExpr^.kind = EXPRCONST then
	if procNameExpr^.exprConst^.kind = DTPROC then
	    proc := procNameExpr^.exprConst^.procVal;
	    if proc^.builtin # BIPNOTBIP then
		(* pass proc constant by var in case CheckBuiltin updates it *)
		return CheckBuiltin(stn, procValExpr, procNameExpr,
		    procNameExpr^.exprConst^.procVal,params,retType);
	    end;
	end;
    end;

    if procType^.kind # DTPROC then
	if procType^.kind # DTANY then
	    ExprErrorName(procNameExpr,errorName,
		    'Non-procedure $ used as a procedure/function');
	end;
    else
	if (stn = nil) and (procType^.funcType = nil) then
	    ExprErrorName(procNameExpr,errorName,
		'Procedure $ used as a function');
	elsif (stn # nil) and (procType^.funcType # nil) then
	    ExprErrorName(procNameExpr,errorName,
		'Function $ used as a procedure');
	else
	    at := ActualType(procType^.funcType);
	    if (at # nil) and 
		(at^.kind in DataTypeSet{DTARRAY, DTDYNARRAY, DTRECORD}) then
		if mode = EVALPUT then
		    if at^.kind = DTRECORD then
		        ExprError(procNameExpr,
			    'Cannot assign to fields of RECORD functions'); 
		    else
			ExprError(procNameExpr,
			    'Cannot assign to elements of ARRAY functions');
		    end;
		elsif (mode = EVALPOINT) then
		    if at^.kind = DTDYNARRAY then
			if SizeOf(at) > WORDSIZE then
			    procValExpr^.selected := true;
			end;
		    else
			procValExpr^.selected := true;
			if SizeOf(at) <= WORDSIZE then
			    (* Function is going to return a value, so we'll
			       want to stash that in a temp. *)
			    procValExpr^.addrOfValue := DefineVar(nil, 
				    at, MEMNORMAL, GSNORMAL, nil);
			end;
		    end;
		end;
	    end;
	    error := false;
	    if (params = nil) or (procType^.paramList = nil) then
		(* make sure they match *)
		if params # nil then
		    if params^.first # nil then
			ExprErrorName(procNameExpr,errorName,
			    'Too many parameters on procedure/function $');
			error := true;
		    end;
		end;
		if procType^.paramList # nil then
		    if procType^.paramList^.first # nil then
			ExprErrorName(procNameExpr,errorName,
			    'Not enough parameters on procedure/function $');
			error := true;
		    end;
		end;
	    else
		pnum := 0;
		pexp := params^.first;
		parm := procType^.paramList^.first;
		newParams := AddToExprList(nil,nil);
		while (pexp # nil) and (parm # nil) do
$if pascal then
		    if pexp^.decimalExpr # nil then
			ExprErrorName(pexp, errorName,
		       'Procedure $: "e:m:n" parameters allowed only in write');
			error := true;
		    elsif pexp^.widthExpr # nil then
			ExprErrorName(pexp, errorName,
			'Procedure $: "e:m" parameters allowed only in write');
			error := true;
		    end;
$end
		    pnum := pnum + 1;
		    pexpnext := pexp^.next;
		    (* decide how parameter will be evaluated *)
$if modula2 then
		    if parm^.kind in 
			ParamKindSet{PARAMARRAYVALUE,PARAMARRAYVAR,PARAMARRAYCONST} then
		        if CheckOpenArrayParam(pexp,parm,newParams,pnum)
			    then error := true;
			end;
		    elsif CheckParam(pexp,parm,newParams,errorName,pnum) then
			error := true;
		    end;
$else (* pascal *)
		    if CheckParam(pexp,parm,newParams,errorName,pnum) then
			error := true;
		    end;
$end
		    pexp := pexpnext;
		    parm := parm^.next;
		end;
		params := newParams;
		if parm # nil then
		    ExprErrorName(procNameExpr, errorName,
			'Not enough parameters on procedure/function $');
		    error := true;
		elsif pexp # nil then
		    ExprErrorName(procNameExpr, errorName,
			'Too many parameters on procedure/function $');
		    error := true;
		end;
	    end;
	end;
	if not error then
	    retType := procType^.funcType;
	end;
    end;
    return not error;
end CheckFuncProc;


procedure CheckBadParams(const params : ExprList);
    var en : ExprNode;
        dummyType : TypeNode;
begin
    if (params # nil) then
    	en := params^.first;
	while en # nil do
	    dummyType := CheckExpr(en, EVALGET);
	    en := en^.next;
 	end;
    end;
end CheckBadParams;


procedure CheckExprFunc(en : ExprNode; mode : EvalMode) : TypeNode;
var
    exprType, funcType : TypeNode;
    func : ExprNode;
    proc : ProcNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprFunc\n');
    end;
    funcType := CheckExprFunction(en^.func,EVALPOINT);
    func := en^.func;
    if IsBadExpr(func) then
	CheckBadParams(en^.params);
	BadExpr(en);
    else
	if (mode = EVALPUT) and (func^.kind # EXPRSYM) then
	    ExprError(en,'Function cannot be used where variable is needed');
	end;
	if func^.kind = EXPRCONST then
	    proc := func^.exprConst^.procVal;
	    if not CheckFuncProc(nil,en,func,en^.params,exprType, mode) then
		BadExpr(en);
	    else
		en^.exprType := ActualType(exprType);
		if proc^.inlineProc and not currProc^.inlineProc then
		    ExpandFuncInline(proc,en);
		end;
	    end;
	else
	    if not (func^.kind in ExprKindSet{EXPRSYM, EXPRFUNC}) then
		(* Get procedure address from variable *)
		ValueOrAddr(en^.func,funcType,EVALGET);
	    end;
	    if not CheckFuncProc(nil,en,func,en^.params,exprType, mode) then
		BadExpr(en);
	    else
		en^.exprType := ActualType(exprType);
	    end;
	end;
    end;
    return en^.exprType;
end CheckExprFunc;

procedure ExprSetToConstSet(var esl     : ExprSetList; 
			    var setType : TypeNode;
			        objKind : DataType) : ConstSetList;
var
    esn, prevesn : ExprSetNode;
    csl : ConstSetList;
    csn : ConstSetNode;
    rangeType, tn : TypeNode;
    allconstant, bothconstant : boolean;
    lowerBound : HugeInteger;
begin
    rangeType := setType^.setRange;
    if esl = nil then
	csl := nil;
    else
	lowerBound := LowerBoundOf(rangeType);
	csl := nil;
	esn := esl^.first;
	allconstant := true;
	while (esn # nil) do
	    bothconstant := true;
	    new(csn);
	    tn := CheckExpr(esn^.lower,EVALGET);
	    if IsBadExpr(esn^.lower) then
		csn^.lower := CardinalConst(0.0);
	    else
		if setType = unknownSetTypeNode then (* Create a set type *)
		    if tn^.kind = DTINTEGER then
			setType := intsetTypeNode;
		    else
			setType := SetType(tn, TKNULL);
		    end;
		    rangeType := setType^.setRange;
		end;
		if Assignable(rangeType,tn,esn^.lower) = nil then
		    if objKind = DTSET then
			ExprError(esn^.lower,
			    'Element expression incompatible with set range');
		    else
			ExprError(esn^.lower,
			    'Subscript expression incompatible with array index');
		    end;
		elsif esn^.lower^.kind # EXPRCONST then
		    bothconstant := false;
		else
		    csn^.lower := esn^.lower^.exprConst;
		end;
	    end;
	    if esn^.upper = nil then
		csn^.upper := nil;
	    else
		tn := CheckExpr(esn^.upper,EVALGET);
		if IsBadExpr(esn^.upper) then
		    csn^.upper := CardinalConst(0.0);
		else    
		    if setType = unknownSetTypeNode then (* Create a set type *)
			if tn^.kind = DTINTEGER then
			    setType := intsetTypeNode;
			else
			    setType := SetType(tn, TKNULL);
			end;
			rangeType := setType^.setRange;
		    end;
		    if Assignable(rangeType,tn,esn^.upper) = nil then
			if objKind = DTSET then
			    ExprError(esn^.upper,
				'Element expression incompatible with set range');
			else
			    ExprError(esn^.upper,
				'Subscript expression incompatible with array index');
			end;
		    elsif esn^.upper^.kind # EXPRCONST then
			bothconstant := false;
		    else
			csn^.upper := esn^.upper^.exprConst;
		    end;
		end;
	    end;
	    if bothconstant then
		(* both constant, delete this node *)
		if esn = esl^.first then
		    (* delete first *)
		    esl^.first := esn^.next;
		else
		    (* delete not first *)
		    prevesn^.next := esn^.next;
		end;
		(* add to constant list *)
		csl := AddToConstSetList(csl,csn);
	    else
		if lowerBound # 0.0 then
		    (* replace element with (element - lowerBound) *)
		    esn^.lower :=
			MakeExprOffset(rangeType,TKMINUS,esn^.lower,lowerBound);
		    if esn^.upper # nil then
			esn^.upper := MakeExprOffset(rangeType,TKMINUS,
			    esn^.upper,lowerBound);
		    end;
		end;
		allconstant := false;
	    end;
	    prevesn := esn;
	    esn := esn^.next;
	end;
	if allconstant then
	    esl := nil;
	end;
    end;
    return csl;
end ExprSetToConstSet;


procedure CheckExprSet(en : ExprNode; mode : EvalMode) : TypeNode;
var
    csl : ConstSetList;
    tn : TypeNode;
    sym : Symbol;
begin
$if pascal then
    (* This will be patched up later by SetSetType *)
    en^.setType := unknownSetTypeNode;
    en^.exprType := unknownSetTypeNode;
    if (mode = EVALPUT) then
        ExprError(en,'Cannot change a set expression');
    end;
    return unknownSetTypeNode;
$else (* modula2 *)
    if en^.setTypeName = nil then
	tn := bitsetTypeNode;
    else
	tn := nil;
	sym := QualifiedName(en^.setTypeName);
	if sym = nil then
	    (* do nothing *)
	elsif en^.setTypeName^.first # nil then
	    (* more qualifiers remain *)
	    ExprErrorName(en,sym^.name,'Invalid set type $ on set expression');
	elsif sym^.kind # SYMTYPE then
	    ExprErrorName(en,sym^.name,'Symbol $ on set constant not a set type');
	else
	    tn := BaseType(sym^.symType);
	    if tn^.kind # DTSET then
		ExprErrorName(en,sym^.name,
		    'Symbol $ on set constant not a set type');
		tn := nil;
	    end;
	end;
    end;
    if tn = nil then
	tn := unknownSetTypeNode;
	en^.setConst := ExprSetToConstSet(en^.setExpr,tn, DTSET);
	BadExpr(en);
	return nil;
    else
	en^.setConst := ExprSetToConstSet(en^.setExpr,tn, DTSET);
	en^.setType := tn;
	en^.exprType := tn;
	if (mode = EVALPUT) then
	    ExprError(en,'Cannot change a set expression');
	end;
    end;
    return tn;
$end (* modula2 *)
end CheckExprSet;

procedure CheckExprSave(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn : TypeNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprSave\n');
    end;
    tn := CheckExpr(en^.exprSave,mode);
    en^.exprType := en^.exprSave^.exprType;
    return tn;
end CheckExprSave;

procedure CheckExprRuntime(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn : TypeNode;
begin
    if DEBUG and TraceNexpr then
	Writef(output,'CheckExprRuntime\n');
    end;
    tn := CheckExpr(en^.checkExpr,mode);
    en^.exprType := en^.checkExpr^.exprType;
    return tn;
end CheckExprRuntime;

procedure DoCheckExpr(en : ExprNode; mode : EvalMode) : TypeNode;
begin
    if en = nil then
	return nil;
    elsif en^.exprType # nil then
	(* already checked *)
	ExprError(en,'CheckExpr: already checked?');
	return en^.exprType;
    else
	currLine := en^.lineNumber;
	currFile := en^.fileName;
	case en^.kind of
	| EXPRBAD   :	return nil;
	| EXPRNAME  :	return CheckExprName(en,mode);
	| EXPRSYM   :	return CheckExprSym(en,mode);
	| EXPRVAR   :	return CheckExprVar(en,mode);
	| EXPRCONST :	return CheckExprConst(en,mode);
	| EXPRUNOP  :	return CheckExprUnOp(en,mode);
	| EXPRBINOP :	return CheckExprBinOp(en,mode);
	| EXPRSUBSCR:   return CheckExprSubscript(en,mode);
	| EXPRDOT   :	return CheckExprDot(en,mode);
	| EXPRDEREF :	return CheckExprDeref(en,mode);
	| EXPRFUNC  :	return CheckExprFunc(en,mode);
	| EXPRSET   :	return CheckExprSet(en,mode);
	| EXPRFORMER:   return CheckExprFormer(en,mode);
	| EXPRRANGE :	return CheckExprRange(en,mode);
	| EXPRCHECK :	return CheckExprRuntime(en,mode);
	| EXPRSAVE  :	return CheckExprSave(en,mode);
	end;
    end;
end DoCheckExpr;

procedure CheckExpr(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn : TypeNode;
begin
    tn := DoCheckExpr(en,mode);
    if IsBadExpr(en) then
    elsif not (en^.kind in ExprKindSet{EXPRVAL, EXPRVAR, EXPRBINOP, EXPRCHECK,
		EXPRUNOP, EXPRCONST, EXPRFUNC, EXPRSAVE, EXPRSET, EXPRFORMER,
		EXPRINLINE, EXPRDESCRIPTOR})
    then
	ExprError(en,'Expression is not a value or variable');
	BadExpr(en);
    end;
    return tn;
end CheckExpr;

$if pascal then
procedure CheckExprWithSubExprs(en : ExprNode; mode : EvalMode) : TypeNode;
(* Various CheckExpr guys replace existing expression nodes with new nodes,
   so save the current width and decimal point fields around the CheckExpr
   call *)
var
    widthExpr, decimalExpr : ExprNode;
    tn : TypeNode;
begin
    if en = nil then
	return nil;
    else
	widthExpr := en^.widthExpr;
	decimalExpr := en^.decimalExpr;
	tn := CheckExpr(en, mode);
	en^.widthExpr := widthExpr;
	en^.decimalExpr := decimalExpr;
	return tn;
    end;
end CheckExprWithSubExprs;
$end (* pascal *)


procedure CheckExprFunction(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn : TypeNode;
begin
    tn := DoCheckExpr(en,mode);
    if IsBadExpr(en) or (tn^.kind = DTANY) then
    elsif en^.kind # EXPRSYM then
	(* any normal expression that is a procedure value is OK *)
	if tn^.kind # DTPROC then
	    ExprError(en,
		'Procedure/function name is not a procedure, function, or type');
	    BadExpr(en);
	end;
    elsif en^.exprSym^.kind # SYMTYPE then
	ExprError(en,'Procedure/function name is not a procedure, function, or type');
	BadExpr(en);

    end;
    return tn;
end CheckExprFunction;

procedure CheckExprType(en : ExprNode; mode : EvalMode) : TypeNode;
var
    tn : TypeNode;
begin
    tn := DoCheckExpr(en,mode);
    if IsBadExpr(en) then
    elsif en^.kind # EXPRSYM then
	ExprError(en,'Expression found where type name expected');
	BadExpr(en);
    elsif en^.exprSym^.kind # SYMTYPE then
	ExprError(en,'Type not found where type name expected');
	BadExpr(en);
    end;
    return tn;
end CheckExprType;

end CheckExpr.
