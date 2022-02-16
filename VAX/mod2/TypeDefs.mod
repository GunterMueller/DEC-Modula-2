implementation module TypeDefs;

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
    HugeInteger, WORDSIZE, BYTESIZE, MAXINT, MININT, UNITSIZE;

from Globals import
    TargetMachine, TraceDecls, DEBUG, target, INDENT;

from Tokens import
    Token, TokenSet;

from Strings import
    String, WriteString;

from Symbols import
    TypeNode, FieldList, FieldNode, VariantList, VariantNode, IdentList,
    ConstSetNode, ConstSetList, ConstNode,
    EnumNode, EnumList, ONECASE, Scope, PointerKind,
    Symbol, IdentNode, AllocationNode, Address, SymbolKind, MEMNORMAL,
    DataType, DataTypeSet, stringDataType, ArrayKind, FieldKind,
    integerTypeNode, charTypeNode, MemoryOffset, anyTypeNode,
    booleanTypeNode, indexableTypes,
    currScope, StartScope, EndScope, DefineSymbol, LookUpSymbol,
    packOrder, PackOrder;

from Errors import
    Error, ErrorName, ErrorNumber;

from TypeInfo import
    NewTypeNode, BaseType, ActualType, ElementType,
    NumberOf, LowerBoundOf, UpperBoundOf, SizeOf, AlignmentOf;

from Alloc import
    RoundUp, AllocateMemory, InitAllocationNode;

from Consts import
    OrdOf, AddToConstSetList;

$if modula2 then
from Machine import MAXCARD, MAXCARDI;
from Globals import standardCardinalFlag;
from Symbols import cardinalTypeNode, cardIntTypeNode;
from Errors import ErrorUnsigned;
$else
from Machine import FILESIZE, HALFSIZE;
$end


(* grammar+ *) procedure PointerType (toType : TypeNode; option : Token) 
    : TypeNode;
(* Create a TypeNode(DTPOINTER) referencing toType. *)
var
    tn, ttn : TypeNode;
begin
    tn := NewTypeNode(DTPOINTER);
    tn^.size := WORDSIZE;
    tn^.toType := toType;
$if pascal then
    tn^.containsFiles := false;
$end
    case option of
    | TKATPASCAL :
	tn^.ptrKind := PTRPASCAL;
    | TKATNOCHECK :
	tn^.ptrKind := PTRNOCHECK;
$if modula2 then
    | TKPOINTER :
	tn^.ptrKind := PTRMODULA;
    | TKATC :
	tn^.ptrKind := PTRC;
    | TKATNILCHECK :
	tn^.ptrKind := PTRNILCHECK;
    | TKATLOCAL :
	tn^.ptrKind := PTRLOCAL;
	Error('Pointer cannot be @LOCAL');
$end
    end;
    ttn := ActualType(toType);
    if (ttn # nil) and (ttn^.kind = DTARRAY)
	    and (ttn^.arrayKind # ARRAYNORMAL) then
	Error('Open array type is valid only for parameters');
    end;
    return tn;
end PointerType;


(* grammar *) procedure PointerForwardType (name : String; option : Token) 
	: TypeNode;
(* Create a TypeNode(DTPOINTER) pointing to the type identified by
   name.  If name has not been defined, enter it as a DTRENAME,
   and return a TypeNode(DTPOINTER) pointing to the TypeNode(DTRENAME). *)
var
    tn, otn : TypeNode;
    sym : Symbol;
begin
    tn := nil;
    sym := LookUpSymbol(name,currScope);
    if sym # nil then
	if sym^.kind # SYMTYPE then
	    ErrorName(name,'$ must be a type for pointer type definition');
	    tn := PointerType(anyTypeNode, option);
	else
	    tn := PointerType(sym^.symType,option);
	end;
    else    (* create a TypeNode(DTRENAME) for name *)
	if not DefineSymbol(sym,name,currScope,ONECASE,SYMTYPE) then
	    ErrorName(name,'PointerForwardType: Unexpected error entering $?');
	else
	    (* treat toType as a rename (indirect) type *)
	    otn := NewTypeNode(DTRENAME);
	    otn^.size := 0;
	    otn^.renameType := nil;
	    tn := PointerType(otn,option);
	    sym^.symType := otn;
	end;
    end;
    return tn;
end PointerForwardType;


(* grammar+ *) procedure SetType (setRange : TypeNode; order : Token) 
		: TypeNode;
(* Create a TypeNode(DTSET) with element type setRange. *)
var
    tn : TypeNode;
begin
    setRange := ActualType(setRange);
    tn := NewTypeNode(DTSET);
    tn^.setRange := setRange;
    if not (setRange^.kind in DataTypeSet{DTCHAR, DTBOOLEAN, DTENUMERATION, 
				    DTSUBRANGE, DTANY}) then
	Error('Invalid type for set elements');
    end;
    tn^.size := RoundUp(trunc(NumberOf(setRange)),WORDSIZE);
    if order = TKLEFTTORIGHT then
        tn^.bitOrder := PACKLEFTTORIGHT;
    elsif order = TKRIGHTTOLEFT then
        tn^.bitOrder := PACKRIGHTTOLEFT;
    else
        tn^.bitOrder := packOrder;
    end;
    if (tn^.size > WORDSIZE) and (tn^.bitOrder # packOrder) then
        Error('Multi-word sets may not have non-standard bit order');
    end;
    return tn;
end SetType;


(*
    ArrayType: construct an array type
    Variations:
    indexType	elementType	kind	    option
    not nil	not open array				regular array
    nil		not open array				1-dimensional open array
    nil		not open array		    NOCOUNT	nocount open array
    nil		open array				n-dimensional open array
    nil		not open array	SUBARRAY		1-dimensional subarray
    nil		open array	SUBARRAY		n-dimensional subarray
    nil		subarray				error
    nil		anything	SUBARRAY    NOCOUNT	error
    Note: normal array (non-nil indexType) cannot be NOCOUNT, SUBARRAY or
	have open or subarray element type. For open or sub array, elementType
	cannot be subarray.   NOCOUNT can occur only on 1-dimensional open
	array, and a NOCOUNT array cannot be a component of an open array.
*)
(* grammar *) procedure ArrayType(indexType : TypeNode; elementType : TypeNode;
				    kind, option : Token) : TypeNode;
var
    atn, etn, itn : TypeNode;
    elementSize   : MemoryOffset;
begin
    atn := NewTypeNode(DTARRAY);
    atn^.indexType := indexType;
    (* watch for automatically packed types: char, charConst, boolean *)
    elementType := ElementType(elementType);
    atn^.elementType := elementType;
    (* Compute elementType's size rounded up to its alignment *)
    elementSize := RoundUp(SizeOf(elementType), AlignmentOf(elementType));
    atn^.elementSize := elementSize;
$if pascal then
    atn^.containsFiles := (elementType # nil) and elementType^.containsFiles;
$end
    if (elementSize mod BYTESIZE) # 0 then
       Error('Array element size rounded to alignment must be a byte multiple');
    end;
    atn^.descripCount := 0;
    if indexType = nil then
	(* open array parameter *)
	assert(kind in TokenSet{TKARRAY,TKSUBARRAY});
	(* decide kind of array and check for illegal element types *)
	if option = TKNOCOUNT then
	    atn^.arrayKind := ARRAYNOCOUNT;
	    if (elementType^.kind = DTARRAY) and
		    (elementType^.arrayKind # ARRAYNORMAL) then
		Error('NOCOUNT array must be one-dimensional');
	    elsif kind = TKSUBARRAY then
		Error('SUBARRAY cannot be specified with NOCOUNT');
	    end;
	else
	    if elementType^.kind = DTARRAY then
		if elementType^.arrayKind = ARRAYSUBARRAY then
		    Error('SUBARRAY permitted only on first dimension');
		elsif elementType^.arrayKind = ARRAYNOCOUNT then
		    Error('Open or dynarray cannot contain a NOCOUNT array')
		end;
	    end;
	    if kind = TKARRAY then
		atn^.arrayKind := ARRAYOPEN;
	    else
		atn^.arrayKind := ARRAYSUBARRAY;
	    end;
	end;

(* ||| This should be shrunk considerably...right now almost direct transcript
    of old code.  Use descripCount of element better. *)
	(* figure number of arguments (sans base address) in descriptor *)
	etn := atn;
	while (etn # nil) and (etn^.kind = DTARRAY) and
		(etn^.arrayKind # ARRAYNORMAL) do
	    case atn^.arrayKind of
	    | ARRAYOPEN :
		(* 1 word for number of elements in dimension *)
		inc(atn^.descripCount);
	    | ARRAYSUBARRAY :
		(* 1 word for number of elements, 1 for "stride" multiplier *)
		inc(atn^.descripCount, 2);
	    | else
		exit while;
	    end (* case *);
	    etn := etn^.elementType;
	end;
	atn^.size := (atn^.descripCount + 1 (* for base address *)) * WORDSIZE;
	if DEBUG and TraceDecls then
	    Writef(output,
		'ArrayType %n size = %d\n', atn^.arrayKind, atn^.size);
	end;
    else
	itn := ActualType(indexType);
	if kind = TKSUBARRAY then
	    Error('SUBARRAY cannot have bounds');
	elsif option = TKNOCOUNT then
	    Error('NOCOUNT cannot be specified with bounds?');
	elsif elementType^.kind = DTARRAY then
	    if elementType^.arrayKind # ARRAYNORMAL then
		Error('Open array cannot be element of ordinary array');
	    end;
	elsif not (itn^.kind
	    in DataTypeSet{DTCHAR, DTBOOLEAN, DTENUMERATION, DTSUBRANGE, DTANY})
	then
	    Error('Invalid type for array subscript');
	end;
	atn^.arrayKind := ARRAYNORMAL;
        atn^.alignment := AlignmentOf(elementType);
	if atn^.alignment < UNITSIZE then
            atn^.alignment := UNITSIZE;
        end;
	atn^.size := trunc(NumberOf(indexType)) * elementSize;
    end;
    (* Default Titan to allocate arrays that have word alignment.  Users can
       override using @align *)
    if target # TARGETVAX then
	if atn^.alignment < WORDSIZE then
            atn^.alignment := WORDSIZE;
	end;
    end;
    return atn;
end ArrayType;


$if modula2 then
(* grammar *) procedure DynArrayType (elementType : TypeNode; arrayKind, ptrKind : Token) : TypeNode;
var
    tn, atn : TypeNode;
begin
    tn := NewTypeNode(DTDYNARRAY);
    atn := ArrayType(nil,elementType,TKARRAY,arrayKind);
    tn^.dynArrayType := atn;
    tn^.size := atn^.size;
    tn^.alignment := atn^.alignment;
    case ptrKind of
    | TKPOINTER :
	tn^.dynArrayKind := PTRMODULA;
    | TKATPASCAL :
	tn^.dynArrayKind := PTRPASCAL;
    | TKATC :
	tn^.dynArrayKind := PTRC;
    | TKATNOCHECK :
	tn^.dynArrayKind := PTRNOCHECK;
    | TKATNILCHECK :
	tn^.dynArrayKind := PTRNILCHECK;
    | TKATLOCAL :
	tn^.dynArrayKind := PTRLOCAL;
    end;
    return tn;
end DynArrayType;
$end (* modula2 *)

(* grammar *) procedure EmptyFieldList () : FieldList;
    var list : FieldList;
begin
    new(list);
    list^.first := nil;
    list^.last := nil;
    return list;
end EmptyFieldList;


(* grammar+ *) procedure AddToVariantList (list : VariantList; newOne : VariantNode)
	: VariantList;
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
end AddToVariantList;


(* grammar *) procedure MakeFieldList(idents : IdentList; fieldType : TypeNode)
		: FieldList;
(* Create a FieldList of all the names, filling in appropriate info
   and giving them all type fieldType. *)
    var	fl : FieldList;
	fn : FieldNode;
	id : IdentNode;
begin
    (* put first field on list. *)
    new(fn);
    fn^.kind := FIELDNORMAL;
    fn^.name := idents^.first^.name;
    fn^.fieldType := fieldType;
    fn^.containingVariant := nil;
    fn^.next := nil;
    new(fl);
    fl^.first := fn;
    fl^.last := fn;
    (* do additional fields, if more idents *)
    id := idents^.first^.next;
    while id # nil do
	new(fn);
	fn^.kind := FIELDNORMAL;
	fn^.name := id^.name;
	fn^.fieldType := fieldType;
	fn^.containingVariant := nil;
	fn^.next := nil;
	fl^.last^.next := fn;
	fl^.last := fn;
	id := id^.next;
    end;
    return fl;
end MakeFieldList;


(* grammar *) procedure AppendFieldList(some, more : FieldList) : FieldList;
begin
    if (some = nil) or (some^.first = nil) then
	some := more;
    elsif (more = nil) or (more^.first = nil) then
	(* nothing to add *)
    else
	some^.last^.next := more^.first;
	some^.last := more^.last;
    end;
    return some;
end AppendFieldList;


(* grammar *) procedure MakeVariant (tag : ConstSetList; fieldList : FieldList)
		: VariantNode;
var
    vn : VariantNode;
begin
    new(vn);
    vn^.elsePart := (tag = nil); (* else tag field filled in by DefineFields *)
    vn^.tag := tag;
    vn^.fieldList := fieldList;
    vn^.tagField := nil;
    return vn;
end MakeVariant;

(* grammar *) procedure MakeTagField (ident : String; fieldType : TypeNode;
	variantList : VariantList; elseVariant : VariantNode) : FieldList;
var
    fl : FieldList;
    fn : FieldNode;
begin
    new(fn);
    fn^.name := ident;
    fn^.fieldType := fieldType;
    fn^.kind := FIELDTAG;
    fn^.containingVariant := nil;
    fn^.variantList := AddToVariantList(variantList,elseVariant);
    new(fl);
    fl^.first := fn;
    fl^.last := fn;
    fn^.next := nil;
    return fl;
end MakeTagField;


procedure DefineFields(fieldList : FieldList; scope : Scope; 
			an : AllocationNode; recType : TypeNode; 
			containingVariant : VariantNode; 
			var alignment : MemoryOffset);
var
    fn		    : FieldNode;
    vn		    : VariantNode;
    sym		    : Symbol;
    address	    : Address;
    fieldAlign      : MemoryOffset;
    atn		    : TypeNode;
    saveNormal, 
    maxNormal       : MemoryOffset;
    saveAlignment, 
    maxAlignment    : MemoryOffset;
    elseTag	    : ConstSetList;
    csn, tcsn       : ConstSetNode;
begin
    fn := fieldList^.first;
    while fn # nil do
	fn^.recType := recType;
	fn^.containingVariant := containingVariant;
	if fn^.name # nil then
	    if DefineSymbol(sym,fn^.name,scope,ONECASE,SYMFIELD) then
		sym^.symField := fn;
		atn := ActualType(fn^.fieldType);
		if atn # nil then
$if pascal then
		    if atn^.containsFiles then
			recType^.containsFiles := true
		    end;
$end
		    if (atn^.kind = DTARRAY)
			    and (atn^.arrayKind # ARRAYNORMAL) then
		        ErrorName(fn^.name,
			     '$ is a field name: open array type not allowed');
		    end;
		end;
		fieldAlign := AlignmentOf(fn^.fieldType);
		AllocateMemory(an,MEMNORMAL,SizeOf(fn^.fieldType),
		    fieldAlign,nil,address);
		fn^.offset := address.offset;
                fn^.size := an^.current[MEMNORMAL]-fn^.offset;
                if recType^.fieldOrder = packOrder then
                    (* don't enforce restrictions *)
               elsif fn^.size <= WORDSIZE then
                    if (fn^.offset div WORDSIZE) #
			    ((fn^.offset+fn^.size-1) div WORDSIZE) then
                        ErrorName(fn^.name,
			    'Field $ may not cross word boundary');
                    end;
                else
                    if (fn^.offset mod WORDSIZE) # 0 then
                        ErrorName(fn^.name,
				    'Multi-word field $ must be word aligned');
                    end;
                end;
		if DEBUG and TraceDecls then
		    Writef(output,'Field ');
		    WriteString(output, fn^.name);
		    Writef(output,' offset %d', fn^.offset);
		end;
		if fieldAlign > alignment then
		    alignment := fieldAlign;
		end;
	    else
		ErrorName(fn^.name,'Field name $ reused in record');
	    end;
	else
	    fn^.offset := -1;
	end;
	if fn^.kind = FIELDTAG then
	    vn := fn^.variantList^.first;
	    saveNormal := an^.current[MEMNORMAL];
	    maxNormal := saveNormal;
	    saveAlignment := alignment;
	    maxAlignment := saveAlignment;
	    elseTag := nil;
	    while vn # nil do
		vn^.tagField := fn;
		if vn^.elsePart then
		    (* Fill in vn^.tag as union of all specified tags *)
		    assert(vn^.next = nil, "Else tag part not last?");
		    vn^.tag := elseTag;
		else
		    (* Add this tag list to elseTag *)
		    csn := vn^.tag^.first;
		    while csn # nil do
			new(tcsn);
			tcsn^ := csn^;
			elseTag := AddToConstSetList(elseTag, tcsn);
			csn := csn^.next;
		    end;
		end;
		DefineFields(vn^.fieldList,scope,an,recType,vn,alignment);
		if maxNormal < an^.current[MEMNORMAL] then
		    maxNormal := an^.current[MEMNORMAL];
		end;
		an^.current[MEMNORMAL] := saveNormal;
		if maxAlignment < alignment then
		    maxAlignment := alignment;
		end;
		alignment := saveAlignment;
		vn := vn^.next;
	    end;
	    an^.current[MEMNORMAL] := maxNormal;
	    alignment := maxAlignment;
	end;
	fn := fn^.next;
    end;
    fieldList^.size := RoundUp(an^.current[MEMNORMAL],alignment);
end DefineFields;

procedure FixPackOrder(fieldList : FieldList; size : MemoryOffset);
var
    fn : FieldNode;
    vn : VariantNode;
begin
    if size > WORDSIZE then
        size := WORDSIZE;
    end;
    fn := fieldList^.first;
    while fn # nil do
        if fn^.name # nil then 
            if fn^.size <= WORDSIZE then 
                if (fn^.offset div WORDSIZE) #
                        ((fn^.offset+fn^.size-1) div WORDSIZE) then 
                    ErrorName(fn^.name,
	'Field $ cannot cross word boundary with non-standard packing order');
                end;
                (* adjust offset for left-to-right order *)
                fn^.offset := fn^.offset + size - 2 * (fn^.offset mod size)
                        - fn^.size;
            end;
        end;
        if fn^.kind = FIELDTAG then 
            vn := fn^.variantList^.first;
            while vn # nil do 
                FixPackOrder(vn^.fieldList,size);
                vn := vn^.next;
            end;
        end;
        fn := fn^.next;
    end;
end FixPackOrder;


(* grammar *) procedure RecordType (fieldList : FieldList; order : Token)
		: TypeNode;
var
    tn : TypeNode;
    an : AllocationNode;
begin
    if (fieldList = nil) or (fieldList^.first = nil) then
	Error('No fields for record');
    end;
    tn := NewTypeNode(DTRECORD);
    tn^.fieldList := fieldList;
    tn^.recScope := StartScope(false);
    EndScope;
    if order = TKLEFTTORIGHT then
        tn^.fieldOrder := PACKLEFTTORIGHT;
    elsif order = TKRIGHTTOLEFT then
        tn^.fieldOrder := PACKRIGHTTOLEFT;
    else
        tn^.fieldOrder := packOrder;
    end;
    an := InitAllocationNode();
    tn^.alignment := 1;
    DefineFields(fieldList, tn^.recScope, an, tn, nil,tn^.alignment);
    tn^.size := fieldList^.size;
    if tn^.fieldOrder # packOrder then
        FixPackOrder(fieldList,tn^.size);
    end;
    if tn^.alignment < UNITSIZE then
        tn^.alignment := UNITSIZE;
    end;
    return tn;
end RecordType;


$if pascal then
(* grammar *) procedure FileType (fileType : TypeNode) : TypeNode;
(* Create a TypeNode(DTFILE) with element type fileType. *)

    var
	tn : TypeNode;

begin (* FileType *)
    tn := NewTypeNode(DTFILE);
    tn^.fileType := fileType;
    tn^.isTextFile := (fileType = nil) or (fileType^.kind = DTCHAR);
    tn^.containsFiles := true;
    if (fileType # nil) and fileType^.containsFiles then
	Error('Files cannot be members of files');
    end;
    tn^.size := RoundUp(FILESIZE + SizeOf(fileType), WORDSIZE);
    return tn;
end FileType;


procedure PascalSize(low, high : integer) : MemoryOffset;
(* Only called for subranges and enumerations *)
begin
    if (low >= -128) and (high <= 127) then
	return BYTESIZE;
    elsif (low >= -32768) and (high <= 32767) then
	return HALFSIZE;
    else
	return WORDSIZE;
    end;
end PascalSize;
$end (* if pascal *)

    
(* grammar+ *) procedure MakeSubrange (low, up : HugeInteger; 
				       baseType : TypeNode) : TypeNode;
var
    tn, rtn : TypeNode;
begin
    tn := NewTypeNode(DTSUBRANGE);
    tn^.subMaxOrd := up;
    tn^.subMinOrd := low;
    tn^.baseType := baseType;
    tn^.alignment := baseType^.alignment;
$if modula2 then
    tn^.size := baseType^.size;
$else
    case target of
    | TARGETVAX :
	tn^.size := PascalSize(trunc(low), trunc(up));
    | TARGETTITAN, TARGETTITANM :
	(* Basic size (for var decls) is WORDSIZE, small size is for records
	   and arrays *)
	tn^.size := WORDSIZE;
    end;
$end
    return tn;
end MakeSubrange;


(* grammar *) procedure SubrangeType (lower, upper : ConstNode; baseType : TypeNode)
	: TypeNode;
var
    low, up : HugeInteger;
begin
    baseType := BaseType(baseType);
    if (lower = nil) or (upper = nil) then
	(* do nothing *)
    elsif (lower^.kind # upper^.kind) and
	    (not (lower^.kind in DataTypeSet{DTINTEGER,DTCARDINAL}) or
	    not (upper^.kind in DataTypeSet{DTINTEGER,DTCARDINAL}))
    then
	Error('Subrange lower and upper bounds are not the same type');
    elsif not (lower^.kind in indexableTypes) then
	ErrorName(stringDataType[lower^.kind],
		'$ is an invalid type for subrange');
    else
	low := OrdOf(lower);
	up := OrdOf(upper);
	case lower^.kind of
	| DTINTEGER, DTCARDINAL:
$if modula2 then
	    if low < 0.0 then
		if low < MININT then
		    ErrorNumber('Minimum lower bound for integer subrange is %',
			trunc(MININT));
		end;
		if up > MAXINT then
		    ErrorNumber('Maximum upper bound for integer subrange is %',
			trunc(MAXINT));
		end;
		if (baseType # nil) and (baseType^.kind # DTINTEGER) then
		    Error('Subrange includes values not in declared base type');
		end;
		baseType := integerTypeNode;
	    elsif standardCardinalFlag or (up > MAXINT) then
		if up > MAXCARD then
		    ErrorUnsigned(
			'Maximum upper bound for unsigned subrange is %',
			MAXCARDI);
		end;
		if (baseType # nil) and (baseType^.kind # DTCARDINAL) then
		    Error('Subrange includes values not in declared base type');
		end;
		baseType := cardinalTypeNode;
	    else
		if baseType = nil then
		    baseType := cardIntTypeNode;
		elsif not (baseType^.kind in DataTypeSet{DTINTEGER,DTCARDINAL})
		then
		    Error('Subrange includes values not in declared base type');
		end;
	    end;
$else (* pascal *)
	    if low < MININT then
		ErrorNumber('Minimum lower bound for integer subrange is %',
		    trunc(MININT));
	    end;
	    if up > MAXINT then
		ErrorNumber('Maximum upper bound for integer subrange is %',
		    trunc(MAXINT));
	    end;
	    baseType := integerTypeNode;
$end
	    
	| DTCHAR:
	    if (baseType # nil) and (baseType^.kind # DTCHAR) then
		Error('Subrange includes values not in base type');
	    end;
	    baseType := charTypeNode;
	
	| DTENUMERATION:
	    if lower^.enumVal^.enumType # upper^.enumVal^.enumType
	    then
		Error
	    ('Subrange lower and upper bounds are not the same enumeration');
	    end;
	    if (baseType # nil) and (baseType # lower^.enumVal^.enumType) then
		Error('Subrange includes values not in base type');
	    end;
	    baseType := lower^.enumVal^.enumType;
	
	| DTBOOLEAN:
	    if (baseType # nil) and (baseType^.kind # DTBOOLEAN) then
		Error('Subrange includes values not in base type');
	    end;
	    baseType := booleanTypeNode;
	
	end;
	if low > up then
	    Error('Start of subrange greater than end');
	end;
    end;
    if baseType = nil then
	return MakeSubrange(0.0, 1.0, anyTypeNode);
    else
	return MakeSubrange(low,up,baseType);
    end;
end SubrangeType;


(* grammar *) procedure AddToEnumList (list : EnumList; newOne : EnumNode)
		: EnumList;
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
end AddToEnumList;


(* grammar *) procedure EnumerationType(enumList : EnumList) : TypeNode;
var
    tn, rtn : TypeNode;
    sym     : Symbol;
    enum    : EnumNode;
    enumOrd : integer;
    error   : boolean;
begin
    error := false;
    tn := NewTypeNode(DTENUMERATION);
    tn^.enumMax := trunc(MININT);
    tn^.enumMin := trunc(MAXINT);
    tn^.enumList := enumList;
    tn^.enumContiguous := true;
    tn^.nameTable := nil;
    enumOrd := 0;
    enum := enumList^.first;
    while enum # nil do
	if DefineSymbol(sym,enum^.name,currScope,ONECASE,SYMENUM) then
	    if enum^.enumValue then
		(* Non-contiguous if specify a gap *)
		if (enumList^.first # enum) and (enumOrd # enum^.enumOrd) then
		    tn^.enumContiguous := false;
	        end;
		enumOrd := enum^.enumOrd;
	    else
		enum^.enumOrd := enumOrd;
	    end;
	    enum^.enumType := tn;
	    enum^.enumSym := sym;
	    sym^.symEnum := enum;
	    if enum^.enumOrd > tn^.enumMax then
		tn^.enumMax := enum^.enumOrd;
	    end;
	    if enum^.enumOrd < tn^.enumMin then
		tn^.enumMin := enum^.enumOrd;
	    end;
	else
	    ErrorName(enum^.name,'Enumeration constant $ redefined');
	    error := true;
	end;
	enumOrd := enumOrd + 1;
	enum := enum^.next;
    end;
$if modula2 then
    tn^.size := WORDSIZE;
$else
    case target of
    | TARGETVAX :
	tn^.size := PascalSize(tn^.enumMin, tn^.enumMax);
    | TARGETTITAN, TARGETTITANM :
	(* Basic size (for var decls) is WORDSIZE, small size is for records
	   and arrays *)
	tn^.size := WORDSIZE;
    end;
$end
    return tn;
end EnumerationType;


(* grammar *) procedure MakeEnumNode(name : String; value : ConstNode) : EnumNode;
var
    enum : EnumNode;
    cardVal : HugeInteger;
begin
    new(enum);
    enum^.name := name;
    if value = nil then
	enum^.enumValue := false;
    else
	enum^.enumValue := true;
	cardVal := OrdOf(value);
	if cardVal > MAXINT then
            ErrorName(name,'Enumeration value for $ must be <= MAXINT');
            enum^.enumOrd := 0;
        else
            enum^.enumOrd := trunc(cardVal);
        end;
    end;
    return enum;
end MakeEnumNode;


(* grammar+ *) procedure TypeWithSize(tn : TypeNode; size : ConstNode)
		: TypeNode;
var
    stn,btn : TypeNode;
    minValue, maxValue, testValue : HugeInteger;
    testSize : MemoryOffset;
    signed : boolean;
begin
    stn := NewTypeNode(DTRENAME);
    stn^.renameType := tn;
    stn^.size := trunc(OrdOf(size));
$if pascal then
    stn^.containsFiles := (tn # nil) and tn^.containsFiles;
$end
    btn := BaseType(tn);
    if stn^.size = 0 then 
        stn^.alignment := 1;
	case btn^.kind of
	| DTINTEGER, DTCARDINAL, DTSUBRANGE, DTENUMERATION :
	    signed := false;
	    minValue := LowerBoundOf(tn);
            if minValue < 0.0 then 
                signed := true;
                minValue := -minValue - 1.0; (* fix bound for 2's complement *)
            end;
            maxValue := abs(UpperBoundOf(tn));
            if minValue > maxValue then 
                maxValue := minValue;
            end;
            testValue := 2.0;
            testSize := 1;
            while testValue-1.0 < maxValue do 
                testValue := testValue + testValue;
                testSize := testSize + 1;
            end;
            if signed then 
                testSize := testSize + 1;       (* leave room for the sign *)
            end;
            stn^.size := testSize;
        | DTBOOLEAN :
            stn^.size := 1;
        | DTCHAR :
            stn^.size := BYTESIZE;
        | DTSET :
            stn^.size := trunc(NumberOf(btn^.setRange));
	| else 
            stn^.size := btn^.size;
        end;
    end;
    if stn^.size <= 0 then
	Error('Size specified must be positive');
	stn^.size := WORDSIZE;
    end;
    case btn^.kind of
    | DTINTEGER, DTBOOLEAN, DTCARDINAL, DTSUBRANGE, DTENUMERATION, 
      DTWORD, DTCHAR, DTBYTE :
	if stn^.size > WORDSIZE then
	    ErrorNumber('Size for one-word types cannot be larger than % bits',
		WORDSIZE);
	    stn^.size := WORDSIZE;
	end;
    | DTARRAY :
	if btn^.arrayKind # ARRAYNORMAL then
	    Error('Cannot change the size of a non-fixed-size array type');
	    stn := btn;
	end;
    | DTRECORD, DTSET, DTOPAQUE :
	(* Okay to change *)
    | else
	ErrorName(stringDataType[btn^.kind], 
	    'Cannot change the size of a $ type');
	stn := btn;
    end;
    if DEBUG and TraceDecls then
        Writef(output,'TypeWithSize: %d\n', stn^.size);
    end;
    return stn;
end TypeWithSize;

$if modula2 then
(* grammar *) procedure TypeWithAlign(tn : TypeNode; alignment : ConstNode) : TypeNode;
var
    stn : TypeNode;
begin
    stn := NewTypeNode(DTRENAME);
    stn^.renameType := tn;
    stn^.size := tn^.size;
$if pascal then
    stn^.containsFiles := (tn # nil) and tn^.containsFiles;
$end
    stn^.alignment := trunc(OrdOf(alignment));
    if stn^.alignment <= 0 then
	Error('Invalid alignment specified');
	stn^.alignment := WORDSIZE;
    elsif (tn^.kind = DTARRAY) and (tn^.arrayKind # ARRAYNORMAL) then
	Error('Cannot change the alignment of a non-fixed-size array type');
	stn := tn;
    end;
    return stn;
end TypeWithAlign;
$end (* modula2 *)

procedure PrintType(tn:TypeNode;indent:integer);
    var i : cardinal;
begin
    for i := 1 to indent do
	Writec(output, ' ');
    end;
    if tn = nil then
	Writef(output, 'nil pointer\n');
    else
	Writef(output, '%n\n', tn^.kind);
	case tn^.kind of
	    | DTPOINTER :
		PrintType(tn^.toType,indent+INDENT);
		
	    | DTSET :
		PrintType(tn^.setRange,indent+INDENT);
		
	    | DTRENAME :
		PrintType(tn^.renameType,indent+INDENT);
		
	    | DTOPAQUE :
		for i := 1 to indent+INDENT do
		    Writec(output, ' ');
		end;
		WriteString(output,tn^.opaqueName);
		Writec(output, '\n');
		
	    | DTARRAY :
		PrintType(tn^.indexType,indent+INDENT);
		PrintType(tn^.elementType,indent+INDENT);
		
	    | DTRECORD :
		for i := 1 to indent+INDENT do
		    Writec(output, ' ');
		end;
		Writef(output,'Record fields\n');
		
	    | DTSUBRANGE :
		for i := 1 to indent+INDENT do
		    Writec(output, ' ');
		end;
		Writef(output, '%1.0F..%1.0F\n', tn^.subMinOrd, tn^.subMaxOrd);
		PrintType(tn^.baseType,indent+INDENT);
		
	end;
    end;
end PrintType;


end TypeDefs.
