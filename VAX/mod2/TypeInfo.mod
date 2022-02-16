implementation module TypeInfo;

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

from Machine import
    HugeInteger, WORDSIZE, MAXINT, MININT, MAXCARD, MAXCHAR;

from Symbols import
    DataType, DataTypeSet, TypeNode, cardIntTypeNode, charTypeNode, 
    charConstTypeNode, booleanTypeNode, byteTypeNode, packedCharTypeNode,
    packedCharConstTypeNode, packedBooleanTypeNode, packedByteTypeNode, 
    constBYTESIZE, ARRAYNORMAL, SIZEUNSPECIFIED, ALIGNMENTUNSPECIFIED;

from TypeDefs import
    TypeWithSize;

from Errors import
    Error;

from Alloc import
    RoundUp;

procedure NewTypeNode (kind : DataType) : TypeNode;
var
    tn : TypeNode;
begin
    new(tn);
    tn^.kind := kind;
    tn^.next := nil;
    tn^.number := 0;
    tn^.size := SIZEUNSPECIFIED;
    tn^.alignment := ALIGNMENTUNSPECIFIED;
    tn^.name := nil;
    tn^.opaqueName := nil;
    tn^.theModule := nil;
$if pascal then
    tn^.containsFiles := false;
$end
    return tn;
end NewTypeNode;


procedure BaseType (tn : TypeNode) : TypeNode;
(* Chain up rename and subrange pointers. Used to get conceptual base
   type without regard to any size/alignment specs or subrange limits *)
begin
    while (tn # nil) do
	if tn^.kind = DTRENAME then
	    if tn^.renameType # nil then
		tn := tn^.renameType;
	    else
		return tn;
	    end;
	elsif tn^.kind = DTSUBRANGE then
	    tn := tn^.baseType;
	else
	    return tn;
	end;
    end;
    return tn;
end BaseType;


procedure ActualType (tn : TypeNode) : TypeNode;
(* Chain up rename pointers.  Try not to return nil.  Used to get
   conceptual type without regard to size/alignment. *)
begin
    if tn # nil then
	while (tn^.kind = DTRENAME) and (tn^.renameType # nil) do
	    tn := tn^.renameType;
	    end;
    end;
    return tn;
end ActualType;


procedure StoredType (tn : TypeNode) : TypeNode;
(* Chain up rename pointers as long as unspecified size/alignment,
   or if size/alignment same as renamed size/alignment.  Try not
   to return nil. Used to get physical type, including storage
   and alignment specifications *)
begin
    if tn # nil then
	while (tn^.kind = DTRENAME) and (tn^.renameType # nil) and
	      (tn^.size = tn^.renameType^.size) and
	      ((tn^.alignment = ALIGNMENTUNSPECIFIED) or 
		    (tn^.alignment = tn^.renameType^.alignment)) do
	    tn := tn^.renameType;
	end;
    end (* if *);
    return tn;
end StoredType;


(* NumberOf returns the number of elements in a range *)
(*	0 if nil (unbounded array parameter), -1 if invalid type *)
procedure NumberOf (tn : TypeNode) : HugeInteger;
begin
    while (tn # nil) and (tn^.kind = DTRENAME) do
	    tn := tn^.renameType;
    end;
    if tn = nil then
	return 0.0;
    elsif tn^.kind = DTSUBRANGE then
	return tn^.subMaxOrd - tn^.subMinOrd + 1.0;
    elsif tn^.kind = DTBOOLEAN then
	return 2.0;
    elsif tn^.kind = DTCHAR then
	return MAXCHAR + 1.0;
    elsif tn^.kind = DTENUMERATION then
	return longfloat(tn^.enumMax-tn^.enumMin+1);
    elsif tn^.kind = DTANY then
        return 1.0;
    else
	return -1.0;
    end;
end NumberOf;


(* LowerBoundOf returns the value of the first element in the range *)
procedure LowerBoundOf (tn : TypeNode) : HugeInteger;
begin
    while (tn # nil) and (tn^.kind = DTRENAME) do
	tn := tn^.renameType;
    end;
    if tn = nil then
	return 0.0;
    elsif tn^.kind = DTSUBRANGE then
	return tn^.subMinOrd;
    elsif tn = cardIntTypeNode then
	return 0.0;
    elsif tn^.kind = DTINTEGER then
	return MININT;
    elsif tn^.kind = DTENUMERATION then
	return longfloat(tn^.enumMin);
    else
	return 0.0;
    end;
end LowerBoundOf;


(* UpperBoundOf returns the value of the last element in a range *)
procedure UpperBoundOf (tn : TypeNode) : HugeInteger;
begin
    while (tn # nil) and (tn^.kind = DTRENAME) do
	tn := tn^.renameType;
    end;
    if tn = nil then
	return MAXINT;
    elsif tn^.kind = DTSUBRANGE then
	return tn^.subMaxOrd;
    elsif (tn = cardIntTypeNode) or (tn^.kind = DTINTEGER) then
	return MAXINT;
    elsif tn^.kind = DTCARDINAL then
	return MAXCARD;
    elsif tn^.kind = DTBOOLEAN then
	return 1.0;
    elsif tn^.kind = DTCHAR then
	return MAXCHAR;
    elsif tn^.kind = DTENUMERATION then
	return longfloat(tn^.enumMax);
    else
	return MAXINT;
    end;
end UpperBoundOf;


procedure SizeOf (tn : TypeNode) : MemoryOffset;
(* Return size in bits of the type *)
begin
    (* fix AlignmentOf if this is changed *)
    if tn^.size = 0 then
	Error('Cannot determine size of type');
    end;
    return tn^.size;
end SizeOf;


procedure ElementType(tn : TypeNode) : TypeNode;
begin
    (* watch for automatically packed types: char, charConst, boolean *)
    tn := StoredType(tn);
    if tn = charTypeNode then
	return packedCharTypeNode;
    elsif tn = charConstTypeNode then
	return packedCharConstTypeNode;
    elsif tn = booleanTypeNode then
	return packedBooleanTypeNode;
    elsif tn = byteTypeNode then
	return packedByteTypeNode;
    elsif tn^.kind = DTSUBRANGE then
	if StoredType(tn^.baseType)^.kind in DataTypeSet{DTBOOLEAN,DTCHAR} then
	    (* pack subranges of char and boolean(?) *)
	    return TypeWithSize(tn, constBYTESIZE);
	end;
    end;
    return tn;
end ElementType;

procedure AlignmentOf (tn : TypeNode) : MemoryOffset;
(* Return alignment in bits of the type.  Use alignment that was
   specified; or default for unspecified.  Default is WORDSIZE for
   objects at least one word large, otherwise smallest 2^n >= size. *)   
var
    alignment : MemoryOffset;
    atn : TypeNode;
begin
    atn := tn;
    alignment := ALIGNMENTUNSPECIFIED;
    while (atn # nil) and (alignment = ALIGNMENTUNSPECIFIED) do
	alignment := atn^.alignment;
	if atn^.kind = DTRENAME then
	    atn := atn^.renameType;
	elsif atn^.kind = DTSUBRANGE then
	    atn := atn^.baseType;
	else
	    atn := nil;
	end;
    end;
    if alignment = ALIGNMENTUNSPECIFIED then
        if tn^.size >= WORDSIZE then
            alignment := WORDSIZE;
        else
            alignment := 1;
            while (alignment < tn^.size) do
                alignment := alignment * 2;
            end;
        end;
    end;
    return alignment;
end AlignmentOf;


procedure WordSizeOf (tn : TypeNode) : MemoryOffset;
begin
    return RoundUp(tn^.size,WORDSIZE);
end WordSizeOf;

(* ReferenceByPoint: pass as reference parameter and reference using an
    address *)
procedure ReferenceByPoint(tn : TypeNode) : boolean;
var
    atn : TypeNode;
begin
    atn := BaseType(tn);
    if atn^.kind = DTARRAY then
        (* open arrays are passed as multiple parameters, otherwise always
	   by address *)
	return atn^.arrayKind = ARRAYNORMAL;
    elsif SizeOf(tn) <= WORDSIZE then
        return false;
    elsif atn^.kind = DTLONGREAL then 
        return false;
    end;
    return true;
end ReferenceByPoint;

end TypeInfo.
