implementation module Former;

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


from MemLib import
    ALLOCATE;

from Strings import
    String;

from Symbols import
    IdentList, SYMCONST, SYMENUM, Symbol, LookUpSymbol, 
    ConstSetNode, ConstSetList, ConstNode, 
    DataType, ArrayKind, TypeNode, TypeOf, EXPRFORMER, ExprNode, EvalMode,
    FormerElementKind, FormerNode, FormerElementList;

from TypeInfo import
    BaseType;

from Errors import
    Error, ErrorName, ExprError;

from Consts import
    AddToConstSetList, AssignableConst;

from BuildExpr import
    NewExprNode;

from CheckExpr import
    BadExpr;

procedure CheckExprFormer(en : ExprNode; mode : EvalMode) : TypeNode;
begin
    ExprError(en,'Former expressions not implemented');
    BadExpr(en);
    return nil;
end CheckExprFormer;

procedure BuildExprFormer(typeId : IdentList; fel : FormerElementList) : ExprNode;
var
    en : ExprNode;
    tn : TypeNode;
begin
    en := NewExprNode(EXPRFORMER);
    tn := TypeOf(typeId);
    en^.exprFormer := MakeFormer(tn,fel);
    return en;
end BuildExprFormer;

procedure ConstFormer(typeId : IdentList; fel : FormerElementList) : ConstNode;
var
    cn : ConstNode;
    tn : TypeNode;
begin
    new(cn);
    tn := TypeOf(typeId);
    if tn^.kind = DTRECORD  then
	cn^.kind := DTRECORD;
	cn^.recordVal := MakeFormer(tn,fel);
    elsif tn^.kind = DTARRAY  then
	cn^.kind := DTARRAY;
	cn^.arrayVal := MakeFormer(tn,fel);
    else
	Error('Invalid former type');
    end;
    return cn;
end ConstFormer;

procedure MakeFormer(formerType : TypeNode; fel : FormerElementList) : FormerNode;
var
    fn      : FormerNode;
    fen     : FormerElementNode;
    fekind  : FormerElementKind;
    value, lower : ConstNode;
    vtn     : TypeNode;
    field   : String;
    sym     : Symbol;
    csn     : ConstSetNode;
    csl     : ConstSetList;
    checked : boolean;
begin
    new(fn);
    fn^.formerType := BaseType(formerType);
    case fn^.formerType^.kind of
    | DTARRAY :
	fekind := FEARRAYCONST;
        if (fn^.formerType^.elementType^.kind = DTCHAR) and
                (fn^.formerType^.arrayKind = ARRAYNORMAL) then
            fekind := FECONST;
        end;
    | DTRECORD :
	fekind := FERECORDCONST;
    | DTSTRING, DTCHAR, DTINTEGER, DTCARDINAL, DTBOOLEAN, DTREAL, DTLONGREAL, 
      DTENUMERATION, DTPROC :
	fekind := FECONST;
    else
	Error('Invalid type for former');
	fekind := FECONST;
    end;
    fn^.kind := fekind;
    if fekind = FERECORDCONST then
	checked := false;
	fen := fel^.first;
	if (fen^.kind = FECONST) and (fen^.next = nil) then
	    if fen^.valueConst^.kind = DTRECORD then
		if fen^.valueConst^.recordVal^.formerType = formerType
		then
		    fel := fen^.valueConst^.recordVal^.value;
		    checked := true;
		end;
	    end;
	end;
	if checked then
	else
	    while fen # nil do
		if fen^.kind # FEFIELD then
		    Error('Field name required for record former');
		else
		    sym := LookUpSymbol(fen^.tagField,formerType^.recScope);
		    if sym = nil then
			ErrorName(fen^.tagField,'$ is not a field of this record');
		    else
			value := fen^.valueField;
			if not AssignableConst(sym^.symField^.fieldType,value)
			then
			    ErrorName(field,'Value not assignable to field $');
			else
			    fen^.kind := FERECORDCONST;
			    fen^.recordField := sym^.symField;
			    fen^.recordConst := value;
			end;
		    end;
		end;
		fen := fen^.next;
	    end;
	end;
    elsif fekind = FEARRAYCONST then
	fen := fel^.first;
	if (fen^.kind = FECONST) and (fen^.next = nil) then
	    if fen^.valueConst^.kind = DTARRAY then
		if fen^.valueConst^.arrayVal^.formerType = formerType
		then
		    fel := fen^.valueConst^.arrayVal^.value;
		    checked := true;
		end;
	    end;
	end;
	if not checked then
	    while fen # nil do
		if fen^.kind = FEFIELD then
		    (* simple ident, convert to constant *)
		    lower := nil;
		    sym := LookUpSymbol(fen^.tagField,nil);
		    if sym = nil then
			ErrorName(fen^.tagField,'$ is not a field of this record');
		    elsif sym^.kind = SYMCONST then
			lower := sym^.symConst;
		    elsif sym^.kind = SYMENUM then
			new(lower);
			lower^.kind := DTENUMERATION;
			lower^.enumVal := sym^.symEnum;
		    else
			ErrorName(fen^.tagField,'$ is an invalid subscript value');
		    end;
		    if lower # nil then
			value := fen^.valueField;
			new(csn);
			csn^.lower := lower;
			csn^.upper := nil;
			fen^.kind := FECONST;
			fen^.tagsConst := AddToConstSetList(nil,csn);
			fen^.valueConst := value;
		    end;
		end;
		if fen^.kind # FECONST then
		    Error('Invalid array former');
		elsif fen^.tagsConst = nil then
		    Error('Invalid array former');
		else
		    csn := fen^.tagsConst^.first;
		    while csn # nil do
			if not AssignableConst(formerType^.indexType,csn^.lower)
			then
			    Error('Subscript value not assignable to index type');
			elsif csn^.upper = nil then
			elsif not AssignableConst(formerType^.indexType,csn^.upper)
			then
			    Error('Subscript value not assignable to index type');
			end;
			csn := csn^.next;
		    end;
		    if not AssignableConst(formerType^.elementType,fen^.valueConst)
		    then
			Error('Value not assignable to array element');
		    end;
		    csl := fen^.tagsConst;
		    value := fen^.valueConst;
		    fen^.kind := FEARRAYCONST;
		    fen^.arrayConst := value;
		    fen^.indexConst := csl;
		end;
		fen := fen^.next;
	    end;
	end;
    elsif fekind = FECONST then
	fen := fel^.first;
	if fen^.tagsConst # nil then
	    Error('Invalid scalar initial value');
	elsif not AssignableConst(formerType,fen^.valueConst) then
	    Error('Initial value not assignable to variable type');
	end;
    else
	Error('Unimplemented former type');
    end;
    fn^.value := fel;
    return fn;
end MakeFormer;

procedure MakeFormerConstElement(elementList : ConstSetList; value : ConstNode) 
	    : FormerElementNode;
var
    fen : FormerElementNode;
begin
    new(fen);
    fen^.kind := FECONST;
    fen^.tagsConst := elementList;
    fen^.valueConst := value;
    return fen;
end MakeFormerConstElement;

procedure MakeFormerFieldElement(field : String; value : ConstNode) : FormerElementNode;
var
    fen : FormerElementNode;
begin
    new(fen);
    fen^.kind := FEFIELD;
    fen^.tagField := field;
    fen^.valueField := value;
    return fen;
end MakeFormerFieldElement;

procedure AddToFormerElementList(list : FormerElementList; 
				 newOne : FormerElementNode) : FormerElementList;
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
end AddToFormerElementList;

procedure AppendFormerElementList(some, more : FormerElementList) : FormerElementList;
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
end AppendFormerElementList;

end Former.
