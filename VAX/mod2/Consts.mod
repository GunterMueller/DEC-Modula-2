implementation module Consts;

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


from IO import
    File, Writef, Writec, output;

from MemLib import
    ALLOCATE;

from Machine import
    HugeInteger, MININT, MAXINT, MAXCARD, CharSet;

from Globals import
    TraceCexpr, DEBUG;

from Tokens import
    Token, TokenSet, stringToken;

from Strings import
    AddChar, NonHashString, WriteString, Compare;

from Symbols import
    Symbol, ConstNode, IdentList, ConstSetList, ConstSetNode, TypeNode,
    SetValue, ConstParamNode, ConstParamList, ConstParamKind, 
    SymbolKind, DataType, DataTypeSet, stringDataType, bitsetTypeNode, 
    indexableTypes, QualifiedName, ConstSetType, unknownSetTypeNode,
    MAXSETSIZE;

from Errors import
    ErrorName, Error, Warning, ErrorNumber;

from TypeInfo import
    BaseType, ActualType, LowerBoundOf, UpperBoundOf, NumberOf;

procedure OrdOf(cn : ConstNode) : HugeInteger;
(* Return the ordinal value of the ConstNode if it is a scalar, else error *)
begin
    if cn = nil then
	return 1.0;
    else
	case cn^.kind of
	| DTINTEGER,
	  DTCARDINAL    : return cn^.cardVal;
	| DTCHAR	: return longfloat(ord(cn^.charVal));
	| DTBOOLEAN     : return longfloat(ord(cn^.boolVal));
	| DTENUMERATION : return longfloat(cn^.enumVal^.enumOrd);
	| else		  ErrorName(stringDataType[cn^.kind],
				    'Constant $ type not discrete scalar');
			  return 1.0;
	end;
    end;
end OrdOf;

procedure AssignableConst(tn : TypeNode; cn : ConstNode) : boolean;
    var etn : TypeNode;
begin
    if (tn = nil) or (cn = nil) then
	return true;
    end;
    tn := BaseType(tn);
    case tn^.kind of
    | DTANY :
	return true;
    | DTINTEGER, DTCARDINAL :
        return cn^.kind in DataTypeSet{DTINTEGER,DTCARDINAL};
    | DTENUMERATION :
        return (cn^.kind = DTENUMERATION) and (cn^.enumVal^.enumType = tn);
    | DTRECORD :
        return (cn^.kind = DTRECORD) and (cn^.recordVal^.formerType = tn);
    | DTARRAY :
        if cn^.kind = DTARRAY then
            return cn^.arrayVal^.formerType = tn;
	end;
	etn := BaseType(tn^.elementType);
	if (cn^.kind = DTSTRING) and (etn^.kind = DTCHAR) then
            return cn^.strVal^.length < trunc(NumberOf(tn^.indexType));
	elsif (cn^.kind = DTCHAR) and (etn^.kind = DTCHAR) and
		(1 < trunc(NumberOf(tn^.indexType))) then
	    (* Convert character to string *)
	    AddChar(cn^.charVal);
	    cn^.kind := DTSTRING;
	    cn^.strVal := NonHashString();
	    return true;
	else
	    return false;
	end;
    | DTREAL, DTLONGREAL :
        return cn^.kind in DataTypeSet{DTREAL,DTLONGREAL};
    | DTBOOLEAN, DTCHAR :
        return cn^.kind = tn^.kind;
    | DTPROC :
	return (cn^.kind = DTPROC) and (cn^.procVal^.procType = tn);
    | else
	return false;
    end;
end AssignableConst;

procedure CardinalConst(value : HugeInteger) : ConstNode;
(* Return a ConstNode(DTCARDINAL) with the ordinal value passed *)
var
    cn : ConstNode;
begin
    if DEBUG and TraceCexpr then
	Writef(output, 'CardinalConst(%1.0F)\n', value);
    end;
    new(cn);
    cn^.kind := DTCARDINAL;
    cn^.cardVal := value;
    if (value > MAXCARD) or (value < MININT) then
	Error('Cardinal constant exceeds implementation limits');
    end;
    return cn;
end CardinalConst;


procedure ConstSym(names : IdentList) : ConstNode;
(* If names is a constant declared in a CONST section, return a copy of 
   the ConstNode.  Otherwise names should be the name of an enumerated 
   constant, so return a ConstNode(DTENUMERATION) *)
var
    value : ConstNode;
    sym : Symbol;
begin
    if DEBUG and TraceCexpr then
	Writef(output, 'SymConst\n');
    end;
    sym := QualifiedName(names);
    if sym = nil then       (* QualifiedNames gave error *)
	return nil;
    elsif names^.first # nil then
	ErrorName(sym^.name,'Qualification error on constant $');	
	return nil;
    elsif sym^.kind = SYMENUM then
	new(value);
	value^.kind := DTENUMERATION;
	value^.enumVal := sym^.symEnum;
	return value;
    elsif sym^.kind = SYMPROC then
	new(value); (* CED 6/16/88 - to implement initial values w/ DTPROC *)
	value^.kind := DTPROC;
	value^.procVal := sym^.symProc;
	return value;
    elsif sym^.kind # SYMCONST then
	ErrorName(sym^.name,'Symbol $ not a constant');
	return nil;
    elsif sym^.symConst = nil then
	return nil;
    else
	new(value);
	value^ := sym^.symConst^;
	return value;
    end;
end ConstSym;

procedure AddToConstSetList (list : ConstSetList; newOne : ConstSetNode)
	: ConstSetList;
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
end AddToConstSetList;

procedure MakeConstSet (const lower : ConstNode;
		              upper : ConstNode) : ConstSetNode;
(* Type-check and structure check lower and upper, and construct a
   ConstSetNode, which is just one subrange or singleton part of
   of constant set constructor. *)

var
    cln : ConstSetNode;
    error : boolean;
begin
    error := false;
    cln := nil;
    if lower = nil then
	(* do nothing *)
    else
	if upper = nil then
	    upper := lower;
	elsif (lower^.kind # upper^.kind) and
	    not ((lower^.kind in DataTypeSet{DTCARDINAL, DTINTEGER})
		and (upper^.kind in DataTypeSet{DTCARDINAL,DTINTEGER}))
	then
	    error := true;
	elsif lower^.kind = DTENUMERATION then
	    if lower^.enumVal^.enumType # upper^.enumVal^.enumType then
		error := true;
	    end;
	end;
	if error then
	    Error('Lower bound and upper bound of range must be same type');
	elsif not (lower^.kind in indexableTypes) then
	    ErrorName(stringDataType[lower^.kind],'Range type $ must be indexable');
	else
	    new(cln);
	    cln^.lower := lower;
	    cln^.upper := upper;
	end;
    end;
    return cln;
end MakeConstSet;

procedure ConstSet(elementList : ConstSetList; setType : TypeNode) : ConstNode;
var
    cn : ConstNode;
    sv : SetValue;
    element : ConstSetNode;
    elementType : TypeNode;
    i : integer;
    lowerBound, upperBound, low, upp : HugeInteger;
begin
    new(cn);
    cn^.kind := DTSET;
    new(sv);
    if setType = nil then
	setType := bitsetTypeNode;
    elsif setType^.kind = DTANY then
	setType := unknownSetTypeNode;
    elsif setType^.kind # DTSET then
	ErrorName(setType^.name, 
	    'Identifier $ preceding set constant must be set type name');
	setType := unknownSetTypeNode;
    end;
    lowerBound := LowerBoundOf(setType^.setRange);
    upperBound := UpperBoundOf(setType^.setRange);
    sv^.setType := setType;
    elementType := BaseType(setType^.setRange);
    sv^.value := ConstSetType{};
    if elementList = nil then
	(* do nothing *)
    else
	element := elementList^.first;
	while element # nil do
	    if AssignableConst(elementType, element^.lower) and
		    ((element^.upper = nil) or
		     AssignableConst(elementType, element^.upper)) then
		low := OrdOf(element^.lower);
		if element^.upper = nil then
		    upp := low;
		else
		    upp := OrdOf(element^.upper);
		end;
		if low > upp then
		    Error('Set element range first greater than last');
		elsif (low < lowerBound) or (upp > upperBound) then
		    Error('Set elements not within set bounds');
		elsif trunc(upp - lowerBound) > MAXSETSIZE then
		    ErrorNumber(
			'Constant sets must have no more than % elements',
			MAXSETSIZE+1);
		else
(* ||| Mike's runtime__makeset on Titan is broken
		    sv^.value := sv^.value + 
			ConstSetType{trunc(low-lowerBound)..
					trunc(upp-lowerBound)};
*)
		    for i := trunc(low-lowerBound) to
					trunc(upp-lowerBound) do
			incl(sv^.value, i);
		    end;
		end;
	    else
		Error('Constant types improper for set expression');
	    end;
	    element := element^.next;
	end;
    end;
    cn^.setVal := sv;
    return cn;
end ConstSet;

procedure ConstUnOp(oper : Token; opnd : ConstNode) : ConstNode;
(* Return the result of applying oper on opnd in a new ConstNode *)
var
    value : ConstNode;
begin
    if DEBUG and TraceCexpr then
	Writef(output, 'UnOpConst(%n)\n', oper);
    end;
    assert(oper in TokenSet{TKNOT,TKPLUS,TKMINUS},
	'ConstUnOp operator invalid');
    if opnd = nil then
	return nil;
    else
	new(value);
	value^ := opnd^;
	if oper = TKNOT then
	    if opnd^.kind = DTBOOLEAN then
		value^.boolVal := not opnd^.boolVal;
	    else
		Error('NOT allowed only on BOOLEAN');
	    end;
	elsif oper = TKMINUS then
	    if opnd^.kind in 
		    DataTypeSet{DTREAL,DTLONGREAL,DTINTEGER,DTCARDINAL} then
		if oper = TKMINUS then
		    case opnd^.kind of
		    | DTCARDINAL :
			if value^.cardVal > MAXINT then
			    Warning('Unary - on UNSIGNED?  Are you sure?');
			end;
			value^.kind := DTINTEGER;
			value^.cardVal := -opnd^.cardVal;
		    | DTINTEGER :
			value^.cardVal := -opnd^.cardVal
		    | DTREAL, DTLONGREAL :
			value^.realVal := -opnd^.realVal;
		    end;
		end (* if TKMINUS *);
	    else
		Error('Unary - allowed on REAL, LONGREAL, and INTEGER only');
	    end;
	else (* oper = TKPLUS *)
	    if opnd^.kind in 
		    DataTypeSet{DTREAL,DTLONGREAL,DTINTEGER,DTCARDINAL} then
	    else
		Error(
	       'Unary + allowed on REAL, LONGREAL, INTEGER, and CARDINAL only');
	    end;
	end;
    end;
    return value;
end ConstUnOp;

(* CardDiv:  simulate div with real arithmetic *)
(*  Trick: only do divide if b >= 2 so trunc is guaranteed to work *)
procedure CardDiv(a,b : HugeInteger) : HugeInteger;
var
    q : HugeInteger;
begin
    if b = 0.0 then
	Error('Cardinal division by 0');
	q := longfloat(1);
    elsif a > b * MAXINT then
	Error('Cardinal division overflow');
	q := 1.0;
    elsif a * b < 0.0 then
	(* negative result, watch for truncate *)
	q := longfloat(trunc(a / b));
	if q * b # a then
	    (* not exact, round toward -maxint *)
	    q := q - 1.0;
	end;
    else
	q := longfloat(trunc(a / b));
    end;
    return q;
end CardDiv;

procedure CardMod(a,b : HugeInteger) : HugeInteger;
begin
    return a - (b*CardDiv(a,b));
end CardMod;

procedure ConstBinOp(oper : Token; opnd1, opnd2 : ConstNode; eval : boolean)
	: ConstNode;
(* Return the result of applying oper on opnd1 and opnd2 in a new ConstNode.
   eval TRUE means compiler-generated arithmetic, don't worry about types *)
var
    value, nopnd    : ConstNode;
    cardVal	    : HugeInteger;
    error, negate   : boolean;
    val1, val2      : HugeInteger;
    atn, btn	    : TypeNode;
begin
    if DEBUG and TraceCexpr then
	Writef(output, 'BinOpConst(%n)\n', oper);
    end;
    assert(oper in TokenSet{TKMINUS, TKASTERISK, TKPLUS, TKSLASH, TKAND,
	    TKAMPERSAND, TKEQUALS, TKSHARP, TKLESS, TKGREATER, TKNOTEQUAL,
	    TKLSEQUAL, TKGREQUAL, TKOR, TKIN, TKDIV, TKMOD},
	'BinOpConst operator invalid');
    if (opnd1 = nil) or (opnd2 = nil) then
	return nil;
    end;

    error := false;
    if (opnd1^.kind # opnd2^.kind) and not
	    (((opnd1^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
		and (opnd2^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})) or
	     ((opnd1^.kind in DataTypeSet{DTREAL, DTLONGREAL})
	        and (opnd2^.kind in DataTypeSet{DTREAL, DTLONGREAL})))
    then
	if eval then
	    new(nopnd);
	    nopnd^.kind := DTINTEGER;
	    nopnd^.cardVal := OrdOf(opnd1);
	    opnd1 := nopnd;
	    new(nopnd);
	    nopnd^.kind := DTINTEGER;
	    nopnd^.cardVal := OrdOf(opnd2);
	    opnd2 := nopnd;
	elsif (oper = TKIN) and (opnd2^.kind = DTSET) then
	    (* Different kinds are okay *)
	else
	    ErrorName(stringToken[oper],'Operands for $ must be the same type');
	    error := true;
	end;
    end;
    new(value);
    value^ := opnd1^;
    if not error then
	case opnd2^.kind of
	| DTBOOLEAN:
	    case oper of
	    | TKAMPERSAND,
	      TKAND     : value^.boolVal := opnd1^.boolVal and opnd2^.boolVal
	    | TKOR      : value^.boolVal := opnd1^.boolVal or opnd2^.boolVal
	    | TKEQUALS  : value^.boolVal := opnd1^.boolVal = opnd2^.boolVal
	    | TKSHARP,
	      TKNOTEQUAL: value^.boolVal := opnd1^.boolVal # opnd2^.boolVal
	    | TKLESS    : value^.boolVal := opnd1^.boolVal < opnd2^.boolVal
	    | TKLSEQUAL : value^.boolVal := opnd1^.boolVal <= opnd2^.boolVal
	    | TKGREATER : value^.boolVal := opnd1^.boolVal > opnd2^.boolVal
	    | TKGREQUAL : value^.boolVal := opnd1^.boolVal >= opnd2^.boolVal
	    | else	  ErrorName(stringToken[oper], 
				    'Operator $ not allowed on BOOLEAN');
	    end;

	| DTCHAR :
	    value^.kind := DTBOOLEAN;
	    case oper of
	    | TKEQUALS  : value^.boolVal := opnd1^.charVal = opnd2^.charVal;
	    | TKSHARP,
	      TKNOTEQUAL: value^.boolVal := opnd1^.charVal #  opnd2^.charVal;
	    | TKLESS    : value^.boolVal := opnd1^.charVal <  opnd2^.charVal;
	    | TKLSEQUAL : value^.boolVal := opnd1^.charVal <= opnd2^.charVal;
	    | TKGREATER : value^.boolVal := opnd1^.charVal >  opnd2^.charVal;
	    | TKGREQUAL : value^.boolVal := opnd1^.charVal >= opnd2^.charVal;
	    | else        ErrorName(stringToken[oper],
				    'Operator $ not allowed on CHAR');
	    end;

	| DTSTRING :
	    value^.kind := DTBOOLEAN;
	    case oper of
	    | TKEQUALS  : 
		value^.boolVal := Compare(opnd1^.strVal, opnd2^.strVal) =  0;
	    | TKSHARP, TKNOTEQUAL :
		value^.boolVal := Compare(opnd1^.strVal, opnd2^.strVal) #  0;
	    | TKLESS :
		value^.boolVal := Compare(opnd1^.strVal, opnd2^.strVal) <  0;
	    | TKLSEQUAL :
		value^.boolVal := Compare(opnd1^.strVal, opnd2^.strVal) <= 0;
	    | TKGREATER :
		value^.boolVal := Compare(opnd1^.strVal, opnd2^.strVal) >  0;
	    | TKGREQUAL :
		value^.boolVal := Compare(opnd1^.strVal, opnd2^.strVal) >= 0;
	    | else        ErrorName(stringToken[oper],
				    'Operator $ not allowed on string');
	    end;

	| DTINTEGER, DTCARDINAL:
	    case oper of
	    | TKPLUS	    : value^.cardVal := opnd1^.cardVal + opnd2^.cardVal
	    | TKMINUS       : value^.cardVal := opnd1^.cardVal - opnd2^.cardVal
	    | TKASTERISK    : value^.cardVal := opnd1^.cardVal * opnd2^.cardVal
	    | TKDIV, TKMOD  :
		    if opnd1^.cardVal < 0.0 then
			val1 := -opnd1^.cardVal;
			negate := true;
		    else
			val1 := opnd1^.cardVal;
			negate := false;
		    end;
		    if opnd2^.cardVal < 0.0 then
			val2 := -opnd2^.cardVal;
			negate := not negate;
		    else
			val2 := opnd2^.cardVal;
		    end;
		    if oper = TKDIV then
			value^.cardVal := CardDiv(val1,val2);
		    else
			value^.cardVal := CardMod(val1,val2);
		    end;
		    if negate then
			value^.cardVal := -value^.cardVal;
		    end;
	    | TKEQUALS  : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.cardVal = opnd2^.cardVal
	    | TKSHARP,
	      TKNOTEQUAL: value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.cardVal # opnd2^.cardVal
	    | TKLESS    : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.cardVal < opnd2^.cardVal
	    | TKLSEQUAL : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.cardVal <= opnd2^.cardVal
	    | TKGREATER : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.cardVal > opnd2^.cardVal
	    | TKGREQUAL : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.cardVal >= opnd2^.cardVal
	    | else ErrorName(stringToken[oper],
				'Operator $ not allowed on INTEGER or CARDINAL');
	    end;
	    if (value^.kind # DTBOOLEAN) and 
		((opnd2^.kind = DTINTEGER) or (value^.cardVal < 0.0)) then
		value^.kind := DTINTEGER;
	    end;

	| DTREAL,DTLONGREAL:
	    if (opnd1^.kind = DTLONGREAL) or (opnd2^.kind = DTLONGREAL) then
		value^.kind := DTLONGREAL;
	    end;
	    case oper of
	    | TKPLUS    : value^.realVal := opnd1^.realVal + opnd2^.realVal
	    | TKMINUS   : value^.realVal := opnd1^.realVal - opnd2^.realVal
	    | TKASTERISK: value^.realVal := opnd1^.realVal * opnd2^.realVal
	    | TKSLASH   : value^.realVal := opnd1^.realVal / opnd2^.realVal
	    | TKEQUALS  : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.realVal = opnd2^.realVal
	    | TKSHARP,
	      TKNOTEQUAL: value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.realVal # opnd2^.realVal
	    | TKLESS    : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.realVal < opnd2^.realVal
	    | TKLSEQUAL : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.realVal <= opnd2^.realVal
	    | TKGREATER : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.realVal > opnd2^.realVal
	    | TKGREQUAL : value^.kind := DTBOOLEAN;
			  value^.boolVal := opnd1^.realVal >= opnd2^.realVal
	    | else ErrorName(stringToken[oper],
				'Operator $ not allowed on REAL or LONGREAL');
	    end;

	| DTENUMERATION:
	    value^.kind := DTBOOLEAN;
	    if opnd1^.enumVal^.enumType # opnd2^.enumVal^.enumType then
		ErrorName(opnd1^.enumVal^.name,
			'Cannot compare $ with a different enumeration');
	    else
		case oper of
		| TKEQUALS  : value^.boolVal :=
			      opnd1^.enumVal^.enumOrd = opnd2^.enumVal^.enumOrd
		| TKSHARP,
		  TKNOTEQUAL: value^.boolVal :=
			      opnd1^.enumVal^.enumOrd # opnd2^.enumVal^.enumOrd
		| TKLESS    : value^.boolVal :=
		    	      opnd1^.enumVal^.enumOrd < opnd2^.enumVal^.enumOrd
		| TKLSEQUAL : value^.boolVal :=
			      opnd1^.enumVal^.enumOrd <= opnd2^.enumVal^.enumOrd
		| TKGREATER : value^.boolVal :=
			      opnd1^.enumVal^.enumOrd > opnd2^.enumVal^.enumOrd
		| TKGREQUAL : value^.boolVal :=
			      opnd1^.enumVal^.enumOrd >= opnd2^.enumVal^.enumOrd
		| else ErrorName(stringToken[oper],
				'Operator $ not allowed on enumerations');
		end;
	    end;

	| DTSET:
	    if (oper # TKIN) and
		    (opnd1^.setVal^.setType # opnd2^.setVal^.setType) then
		ErrorName(stringToken[oper],
		    'Cannot use $ on different constant set operand types');
	    else
		if oper in TokenSet{TKPLUS, TKMINUS, TKASTERISK, TKSLASH} then
		    new(value^.setVal);
		    value^.setVal^.setType := opnd1^.setVal^.setType;
		else
		    value^.kind := DTBOOLEAN;
		end;
		case oper of
		| TKEQUALS  : value^.boolVal := opnd1^.setVal^.value
						    = opnd2^.setVal^.value
		| TKSHARP,
		  TKNOTEQUAL: value^.boolVal := opnd1^.setVal^.value 
						    # opnd2^.setVal^.value
		| TKLSEQUAL : value^.boolVal := opnd1^.setVal^.value
						    <= opnd2^.setVal^.value
		| TKGREQUAL : value^.boolVal := opnd1^.setVal^.value
						    >= opnd2^.setVal^.value
		| TKPLUS    : value^.setVal^.value := opnd1^.setVal^.value
						    + opnd2^.setVal^.value
		| TKMINUS   : value^.setVal^.value := opnd1^.setVal^.value
						    - opnd2^.setVal^.value
		| TKASTERISK: value^.setVal^.value := opnd1^.setVal^.value
						    * opnd2^.setVal^.value
		| TKSLASH   : value^.setVal^.value := opnd1^.setVal^.value 
						    / opnd2^.setVal^.value
		| TKIN      :
		    atn := ActualType(opnd2^.setVal^.setType);
		    atn := ActualType(atn^.setRange);
		    btn := BaseType(atn);
		    if (btn^.kind # opnd1^.kind) and not
			((opnd1^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
			 and (btn^.kind in DataTypeSet{DTINTEGER,DTCARDINAL}))
		    then
			Error('Element does not match IN set type');
		    else
			value^.boolVal := 
			    trunc(OrdOf(opnd1) - LowerBoundOf(atn))
				in opnd2^.setVal^.value;
		    end;
		| else ErrorName(stringToken[oper],
				'Operator $ not allowed on sets');
		end;
	    end;

	| DTPROC :
	    value^.kind := DTBOOLEAN;
	    case oper of
	    | TKEQUALS  : value^.boolVal := opnd1^.procVal = opnd2^.procVal;
	    | TKSHARP,
	      TKNOTEQUAL: value^.boolVal := opnd1^.procVal # opnd2^.procVal;
	    | else        ErrorName(stringToken[oper],
				    'Operator $ not allowed on procedures');
	    end;

	| DTPOINTER :
	    (* Only pointer constant is NIL *)
	    value^.kind := DTBOOLEAN;
	    case oper of
	    | TKEQUALS  : value^.boolVal := true;
	    | TKSHARP,
	      TKNOTEQUAL: value^.boolVal := false;
	    | TKLESS    : value^.boolVal := false;
	    | TKLSEQUAL : value^.boolVal := true;
	    | TKGREATER : value^.boolVal := false;
	    | TKGREQUAL : value^.boolVal := true;
	    | else        ErrorName(stringToken[oper],
				    'Operator $ not allowed on NIL');
	    end;

	| else
	    ErrorName(stringToken[oper],
		'Invalid binary constant expression for operator $');
	end;
    end;
    return value;
end ConstBinOp;

(* debug *) procedure WriteConstant(f : File; con : ConstNode);
begin
    if con = nil then
	    Writef(f,'NIL CONSTANT');
    else
	case con^.kind of
	| DTCARDINAL,
	  DTINTEGER     : Writef(f, '%1.0F', con^.cardVal)
	| DTREAL,
	  DTLONGREAL    : Writef(f, '%21.15G', con^.realVal)
	| DTBOOLEAN     : 
	    if con^.boolVal then
		Writef(f, 'true');
	    else
   		Writef(f, 'false');
	    end;
	| DTCHAR	:
	    if con^.charVal in CharSet{' '..'~'} then
		Writec(f, con^.charVal);
	    else
		Writef(f, 'chr(%d)', ord(con^.charVal));
		end;
	| DTSTRING      : WriteString(f, con^.strVal)
	| DTENUMERATION : WriteString(f, con^.enumVal^.name)
	| DTPROC	: WriteString(f, con^.procVal^.name)
	| DTPOINTER     : Writef(f, 'nil')
	| DTSET		: Writef(f, 'set constant')
	end;
    end;
end WriteConstant;

procedure ConstParamConst(cn : ConstNode) : ConstParamNode;
var
    cpn : ConstParamNode;
begin
    new(cpn);
    cpn^.next := nil;
    cpn^.kind := CPCONST;
    cpn^.cpConst := cn;
    return cpn;
end ConstParamConst;


procedure ConstParamIdent(names : IdentList) : ConstParamNode;
var
    cpn : ConstParamNode;
    sym : Symbol;
    cn : ConstNode;
begin
    new(cpn);
    cpn^.next := nil;
    sym := QualifiedName(names);
    if sym = nil then
	cpn := nil;
    elsif names^.first # nil then
	ErrorName(sym^.name,'Qualification error on constant $');	
	cpn := nil;
    elsif sym^.kind = SYMENUM then
	new(cn);
	cn^.kind := DTENUMERATION;
	cn^.enumVal := sym^.symEnum;
	cpn^.kind := CPCONST;
	cpn^.cpConst := cn;
    elsif sym^.kind = SYMCONST then
	cpn^.kind := CPCONST;
	cpn^.cpConst := sym^.symConst;
    elsif sym^.kind = SYMTYPE then
	cpn^.kind := CPTYPE;
	cpn^.cpType := sym^.symType;
    elsif sym^.kind = SYMVAR then
	cpn^.kind := CPVAR;
	cpn^.cpVar := sym^.symVar;
    else
	Error('Invalid constant parameter');
	cpn := nil;
    end;
    return cpn;
end ConstParamIdent;


procedure AddToConstParamList(list : ConstParamList; newOne : ConstParamNode) 
	: ConstParamList;
begin
    if list = nil then
	new(list);
	list^.first := nil;
	list^.last := nil;
    end;
    if newOne = nil then
	(* empty element, do nothing *)
    elsif list^.last = nil then
	newOne^.next := nil;
	list^.first := newOne;
	list^.last := newOne;
    else
	newOne^.next := nil;
	list^.last^.next := newOne;
	list^.last:= newOne;
    end;
    return list;
end AddToConstParamList;
end Consts.
