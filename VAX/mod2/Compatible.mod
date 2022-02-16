implementation module Compatible;

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


from Symbols import
    TypeNode, ExprNode, ParamKind, DataType, DataTypeSet, EXPRCONST,
    ArrayKind, ParamKindSet, ParamNode,
    anyTypeNode, cardIntTypeNode, charConstTypeNode,
    realConstTypeNode, longrealTypeNode, realTypeNode, cardinalTypeNode,
    integerTypeNode,     addressTypeNode;

from TypeInfo import
    BaseType, StoredType, ActualType;

from Errors import
    ExprError;

$if modula2 then
from Machine import
    WORDSIZE, BYTESIZE, CHARSIZE;
from Globals import
    standardStringFlag;
from Symbols import
    ArrayKindSet, wordTypeNode, byteTypeNode, packedByteTypeNode;
from TypeInfo import 
    NewTypeNode, SizeOf;
from Decls import
    SameTypeParam;

$else

from Symbols import unknownSetTypeNode;
from Strings import AddString, AddChar, NonHashString;
from TypeInfo import NumberOf;
from CheckExpr import IntegerToReal;
$end

procedure Compatible (var dtn : TypeNode; den : ExprNode; var stn : TypeNode;
	sen : ExprNode) : TypeNode;
var
    src, dst, tn, etn : TypeNode;
    srcpn, dstpn : ParamNode;
    same : boolean;
$if pascal then
    i, arrayLength : cardinal;
$end
begin
    tn := nil;
    src := BaseType(stn);
    dst := BaseType(dtn);
    if (src = nil) or (dst = nil) then
	(* not much we can do *)
(* compatible if same *)
    elsif dst = src then
	if (dst^.kind = DTARRAY) and (dst^.arrayKind # ARRAYNORMAL) then
	    (* Not allowed to assign open arrays in toto *)
	else
	    tn := dst;
	end;
(*  or any type allowed *)
    elsif (dst = anyTypeNode) or (src = anyTypeNode) then
	tn := dst;
(*  or constant with integer or cardinal *)
    elsif (dst = cardIntTypeNode) and
	    (src^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
    then
	tn := src;
    elsif (src = cardIntTypeNode) and
	    (dst^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
    then
	tn := dst;
(*  or constant with char *)
    elsif (dst = charConstTypeNode) and (src^.kind = DTCHAR) then
	tn := src;
	dtn := src;
	if den # nil then
	    if den^.kind = EXPRCONST then
		den^.constType := src;
		den^.exprType := src;
	    end;
	end;
    elsif (src = charConstTypeNode) and (dst^.kind = DTCHAR) then
	tn := dst;
	stn := dst;
	if sen # nil then
	    if sen^.kind = EXPRCONST then
		sen^.constType := dst;
		sen^.exprType := dst;
	    end;
	end;
(*  or constant with real or longreal *)
    elsif (dst = realConstTypeNode) and
	    ((src = realTypeNode) or (src = longrealTypeNode))
    then
	tn := src;
	dtn := src;
	if den # nil then
	    if den^.kind = EXPRCONST then
		den^.constType := src;
		den^.exprType := src;
	    end;
	end;
    elsif (src = realConstTypeNode) and
	    ((dst = realTypeNode) or (dst = longrealTypeNode))
    then
	tn := dst;
	stn := dst;
	if sen # nil then
	    if sen^.kind = EXPRCONST then
		sen^.constType := dst;
		sen^.exprType := dst;
	    end;
	end;
(* Pascal character arrays of same size *)
$if pascal then
    elsif (src^.kind = DTARRAY) and (dst^.kind = DTARRAY) and
	    (src^.elementType^.kind = DTCHAR) and 
	    (dst^.elementType^.kind = DTCHAR) and
	    (src^.size = dst^.size) then
	tn := stn;
$end (* pascal *)
(* or string with array of char *)
    elsif (src^.kind = DTSTRING) and (dst^.kind = DTARRAY) then
	etn := BaseType(dst^.elementType);
	if (etn^.kind = DTCHAR) and (dst^.arrayKind = ARRAYNORMAL) then
	    if dtn^.size >= stn^.size then
		if (sen # nil) and (sen^.kind = EXPRCONST) then
$if pascal then
		    arrayLength := trunc(NumberOf(dtn^.indexType));
		    if stn^.stringLength < arrayLength then
			(* Make a blank-padded string *)
			AddString(sen^.exprConst^.strVal);
			for i := stn^.stringLength+1 to arrayLength do
			    AddChar(' ');
			end;
			sen^.exprConst^.strVal := NonHashString();
		    end;
$end
		    (* set string type to match array type *)
		    sen^.constType^.size := dtn^.size;
		    sen^.exprType := sen^.constType;
		end;
		tn := dtn;
		stn := dtn;
	    end;
	end;
    elsif (dst^.kind = DTSTRING) and (src^.kind = DTARRAY) then
	etn := BaseType(src^.elementType);
	if (etn^.kind = DTCHAR) and (src^.arrayKind = ARRAYNORMAL) then
	    if stn^.size >= dtn^.size then
		if (den # nil) and (den^.kind = EXPRCONST) then
$if pascal then
		    arrayLength := trunc(NumberOf(stn^.indexType));
		    if dtn^.stringLength < arrayLength then
			(* Make a blank-padded string *)
			AddString(den^.exprConst^.strVal);
			for i := dtn^.stringLength+1 to arrayLength do
			    AddChar(' ');
			end;
			den^.exprConst^.strVal := NonHashString();
		    end;
$end
		    (* set string type to match array type *)
		    den^.constType^.size := stn^.size;
		    den^.exprType := den^.constType;
		end;
		tn := stn;
		dtn := stn;
	    end;
	end;
(* string with string *)
    elsif (src^.kind = DTSTRING) and (dst^.kind = DTSTRING) then
	tn := dtn;
$if modula2 then
(* or char constant with array of char *)
    elsif (src = charConstTypeNode) and (dst^.kind = DTARRAY) then
	etn := BaseType(dst^.elementType);
	if (etn^.kind = DTCHAR) and (dst^.arrayKind = ARRAYNORMAL) then
	    (* set char constant to string type to match array type *)
	    tn := NewTypeNode(DTSTRING);
	    tn^.stringLength := 1;
	    tn^.size := dtn^.size;
	    tn^.alignment := dtn^.alignment;
	    stn := tn;
	    if sen # nil then
		if sen^.kind = EXPRCONST then
		    sen^.constType := tn;
		    sen^.exprType := tn;
		end;
	    end;
	end;
    elsif (dst = charConstTypeNode) and (src^.kind = DTARRAY) then
	etn := BaseType(src^.elementType);
	if (etn^.kind = DTCHAR) and (src^.arrayKind = ARRAYNORMAL) then
	    (* set char to string type to match array type *)
	    tn := NewTypeNode(DTSTRING);
	    tn^.stringLength := 1;
	    tn^.size := stn^.size;
	    tn^.alignment := stn^.alignment;
	    dtn := tn;
	    if den # nil then
		if den^.kind = EXPRCONST then
		    den^.constType := tn;
		    den^.exprType := tn;
		end;
	    end;
	end;
$end (* modula2 *)
(* address and cardinal or address and pointer can be intermixed *)
    elsif (dst=addressTypeNode) and 
	    (src^.kind in DataTypeSet{DTPOINTER,DTCARDINAL,DTINTEGER,DTPROC})
    then
	(* watch out for nil and non-pointer *)
	if den = nil then
	    (* pass to address parameter *)
	    tn := dst;
	elsif den^.kind # EXPRCONST then
	    (* address expression *)
	    tn := dst;
	(* otherwise, must be nil *)
	elsif src^.kind in DataTypeSet{DTPOINTER,DTPROC} then
	    (* only pointer and proc are OK *)
	    tn := dst;
	end;
    elsif (src=addressTypeNode) and 
	    (dst^.kind in DataTypeSet{DTPOINTER,DTCARDINAL,DTINTEGER,DTPROC})
    then
	(* watch out for nil and non-pointer *)
	if sen = nil then
	    tn := src;
	elsif sen^.kind # EXPRCONST then
	    (* address expression *)
	    tn := src;
	(* otherwise, must be nil *)
	elsif dst^.kind in DataTypeSet{DTPOINTER,DTPROC} then
	    (* only pointer and proc are OK *)
	    tn := src;
	end;
$if modula2 then
(* address and dynarray can be intermixed *)
    elsif (dst=addressTypeNode) and (src^.kind = DTDYNARRAY) then
	if den # nil then
	    if den^.kind = EXPRCONST then
		(* nil and dynarray *)
		tn := src;
		den^.exprType := stn;
		dtn := stn;
	    elsif stn^.size = WORDSIZE then
		(* address expression with nocount dynarray *)
		tn := dst;
	    end;
	elsif stn^.size = WORDSIZE then
	    (* address parameter with nocount dynarray *)
	    tn := dst;
	end;
    elsif (src=addressTypeNode) and (dst^.kind = DTDYNARRAY) then
	if sen # nil then
	    if sen^.kind = EXPRCONST then
		(* nil and dynarray *)
		tn := dst;
		sen^.exprType := dtn;
		stn := dtn;
	    elsif dtn^.size = WORDSIZE then
		(* address expression with nocount dynarray *)
		tn := src;
	    end;
	end;
(* procedure constants to procedure variables *)
    elsif (dst^.kind = DTPROC) and (src^.kind = DTPROC) then
	if src^.paramList = nil then
	    srcpn := nil;
	else
	    srcpn := src^.paramList^.first;
	end;
	if dst^.paramList = nil then
	    dstpn := nil;
	else
	    dstpn := dst^.paramList^.first;
	end;
	same := StoredType(dst^.funcType) = StoredType(src^.funcType);
	while same and (srcpn # nil) and (dstpn # nil) do
	    if SameTypeParam(dstpn^.paramType,srcpn^.paramType) then
		if (dstpn^.kind # srcpn^.kind) then
		    (* Special errors because Mike let people do this *)
		    if (dstpn^.kind = PARAMCONST) and
			    (srcpn^.kind = PARAMVALUE) then
			ExprError(sen,
  'Procedure with VALUE param cannot be passed to procedure with CONST param');
		    elsif (dstpn^.kind = PARAMVALUE) and
			    (srcpn^.kind = PARAMCONST) then
			ExprError(sen, 
  'Procedure with CONST param cannot be passed to procedure with VALUE param');
		     else
			same := false;
		    end;
		end;
	    else
		same := false;
	    end;
	    srcpn := srcpn^.next;
	    dstpn := dstpn^.next;
	end;
	if same and (srcpn = nil) and (dstpn = nil) then
	    tn := dtn;
	end;
$else (* pascal *)
    elsif (src = unknownSetTypeNode) and (dst^.kind = DTSET) then
	(* dst (and children, if necessary) type filled in by SetSetType *)
	tn := dst;
    elsif (dst = unknownSetTypeNode) and (src^.kind = DTSET) then
	(* src (and children, if necessary) type filled in by SetSetType *)
	tn := src;
$end
    end;
    return tn;
end Compatible;


procedure Assignable (dtn : TypeNode; var stn : TypeNode; sen : ExprNode)
	: TypeNode;
var
    src, dst, tn, etn : TypeNode;
begin
    tn := Compatible(dtn,nil,stn,sen);
    src := BaseType(stn);
    dst := BaseType(dtn);
    if (src = nil) or (dst = nil) then
	(* not much we can do *)
    elsif tn = nil then
(* check integer/cardinal operation *)
	if ((dst = integerTypeNode) or (dst = cardinalTypeNode)
			or (dst = cardIntTypeNode))
	    and ((src = integerTypeNode) or (src = cardinalTypeNode)
			or (src = cardIntTypeNode))
	then
	    tn := dst;
$if pascal then
	elsif (dst = longrealTypeNode) and (src^.kind = DTINTEGER) then
	    IntegerToReal(sen);
	    tn := longrealTypeNode;
$else (* modula2 *)
(* dynarray^ (of char) assignment with string *)
	elsif (dst^.kind = DTARRAY) and (src^.kind = DTSTRING) then
	    etn := BaseType(dst^.elementType);
	    if (dst^.arrayKind in ArrayKindSet{ARRAYNOCOUNT, ARRAYOPEN}) and
		    (etn^.kind = DTCHAR) then
		tn := src;
	    end;
(* dynarray^ (of char) assignment with single-character constant *)
	elsif (dst^.kind = DTARRAY) and (src = charConstTypeNode) then
	    etn := BaseType(dst^.elementType);
	    if (dst^.arrayKind in ArrayKindSet{ARRAYNOCOUNT, ARRAYOPEN}) and
		    (etn^.kind = DTCHAR) then
		(* make char constant into a string of one character *)
		tn := NewTypeNode(DTSTRING);
		tn^.stringLength := 1;
		tn^.size := 2*CHARSIZE;
		stn := tn;
		if sen # nil then
		    if sen^.kind = EXPRCONST then
			sen^.constType := tn;
			sen^.exprType := tn;
		    end;
		end;
	    end;
$end
	end;
    end;
    return tn;
end Assignable;


procedure Passable (dtn : TypeNode; kind : ParamKind; var stn : TypeNode;
	sen : ExprNode) : boolean;
var
    src, dst, tn, etn : TypeNode;
begin
    if kind in ParamKindSet{PARAMVALUE,PARAMCONST} then
	tn := Assignable(dtn,stn,sen);
        if tn = addressTypeNode then
            src := BaseType(stn);
            dst := BaseType(dtn);
            if (src^.kind in DataTypeSet{DTINTEGER,DTCARDINAL}) or
		    (dst^.kind in DataTypeSet{DTINTEGER,DTCARDINAL}) then
                return false; (* no integer as address param nor vice versa *)
	    else
		return true;
	    end;
	elsif tn # nil then
	    return true;
	end;
    elsif kind = PARAMVAR then
	src := StoredType(stn);
	dst := StoredType(dtn);
	if src = dst then
	    return true;
	elsif ActualType(src) = ActualType(dst) then
	    ExprError(sen,
		'VAR parameters must have identical size and alignment');
	    return true;    (* We already gave error message *)
	end;
    end;

(* Types not passable so far; check out the special compatibility rules *)
    src := BaseType(stn);
    dst := BaseType(dtn);
    if (src = nil) or (dst = nil) then
	return false;
$if modula2 then
(* check pass to a word or byte *)
    elsif (dst = wordTypeNode) and (SizeOf(src) <= WORDSIZE) then
	return true;
    elsif (dst = wordTypeNode) and (src = realConstTypeNode) and
	    (SizeOf(realTypeNode) <= WORDSIZE) and
	    (sen # nil) and (sen^.kind = EXPRCONST) then
	stn := realTypeNode;
	sen^.constType := stn;
	sen^.exprType := stn;
	return true;
    elsif (dst = byteTypeNode) and (SizeOf(src) <= BYTESIZE) then
	return true;
(* check pass to an address or pointer *)
    elsif (dst = addressTypeNode) and (src^.kind = DTPOINTER) then
	return true;
    elsif (src = addressTypeNode) and (dst^.kind = DTPOINTER) then
	return true;
(* check pass to an address or dynarrau *)
    elsif (dst = addressTypeNode) and (src^.kind = DTDYNARRAY)
	    and (src^.size = WORDSIZE) then
	return true;
    elsif (src = addressTypeNode) and (dst^.kind = DTDYNARRAY)
	    and (dst^.size = WORDSIZE) then
	return true;
$end
(* check open array *)
    elsif (dst^.kind = DTARRAY) and (src^.kind = DTARRAY) and
	    (dst^.arrayKind # ARRAYNORMAL) and
	    (StoredType(dst^.elementType) = StoredType(src^.elementType)) then
	return true;
    elsif (dst^.kind = DTARRAY) and (src^.kind = DTSTRING) then
	etn := BaseType(dst^.elementType);
	if (dst^.arrayKind # ARRAYNORMAL) and (etn^.kind = DTCHAR) then
	    return true;
	end;
$if modula2 then
    elsif (dst^.kind = DTARRAY) and (src = charConstTypeNode) then
	etn := BaseType(dst^.elementType);
	if (dst^.arrayKind # ARRAYNORMAL) and (etn^.kind = DTCHAR) then
	    (* make char constant into a string of one character *)
	    tn := NewTypeNode(DTSTRING);
	    tn^.stringLength := 1;
	    if standardStringFlag then
		tn^.size := CHARSIZE;
	    else
		tn^.size := 2*CHARSIZE;
	    end;
	    stn := tn;
	    if sen # nil then
		if sen^.kind = EXPRCONST then
		    sen^.constType := tn;
		    sen^.exprType := tn;
		end;
	    end;
	    return true;
	end;
(* array of word - any src is OK *)
    elsif (dst^.kind = DTARRAY) and (dst^.arrayKind = ARRAYOPEN) and
	    (StoredType(dst^.elementType) = wordTypeNode) then
	return true;	
(* array of byte - any src is OK *)
    elsif (dst^.kind = DTARRAY) and (dst^.arrayKind = ARRAYOPEN) and
	    (StoredType(dst^.elementType) = packedByteTypeNode) then
        return true;	
$end (* modula2 *)
    end;
    return false;
end Passable;

end Compatible.
