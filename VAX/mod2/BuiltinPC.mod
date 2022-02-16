implementation module BuiltinPC;

(*****************************************************************************
 *									     *
 *   Copyright 1985, 1986, 1987, 1988, 1989 Digital Equipment Corporation    *
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

from Machine import
    WORDSIZE, BYTESIZE, UNITSPERWORD;

from Symbols import
    LabelNumber, EvalMode, PointerKind, PointerKindSet, GlobalSymKind,
    ProcNode, ExprNode, ExprList, DataType, DataTypeSet, BuiltinProcType,
    integerTypeNode, charTypeNode, addressTypeNode, longrealTypeNode,  
    cardIntTypeNode, TypeNode, NULLLABEL, MemoryOffset, ArrayKind;

from TypeInfo import
    SizeOf, BaseType;

from PCodeOps import
    PCodeOp;

from PCode import
    GenOp, Lab, I, GenOpL, GenOpTL, GenOpT, NewLabel, C, X, EndLine;

from GenPC import
    GenExpr, GenIndirectVar, GenConstInteger, GenCondition, GenStore,
    GenLibCall;

$if modula2 then
from Machine import UNITSIZE;
from Globals import internalCallFlag, OptNcall;
from Symbols import realTypeNode, cardinalTypeNode, fileTypeNode,
    ioStringTypeNode;
from TypeInfo import AlignmentOf;
from PCode import GenT;
from GenPC import GenCall, GenVar;

$else

from Machine import LONGREALSIZE;
from Symbols import booleanTypeNode;
from TypeInfo import LowerBoundOf, UpperBoundOf;
from GenPC import GenConstBoolean;

var
    mathLibProcNames	: array [BIPsin..BIParctan], boolean of 
				array [0..7] of char;
$end


const
$IF vms THEN
    EOFOFFSET = 100;	(* offset of EOF flag in IO buffer *)
$ELSE
    EOFOFFSET = 132;	(* offset of EOF flag in IO buffer *)
$END

procedure GenBuiltin(const proc : ProcNode; const params : ExprList);
var
    p, p1, p2, p3, p4, p5, p6, conen, sizeen : ExprNode;
    pt1, pt2, tn, atn : TypeNode;
    size : MemoryOffset;
    numParamWords, numDimensions : integer;
    ptrCheck : PointerKind;
    akind    : ArrayKind;
    dimensionsSize : MemoryOffset;
    nilLabel, lab : LabelNumber;
    maybeNil : boolean;
    procNode : ProcNode;
    i : integer;
    bip : BuiltinProcType;

    procedure GenWritef(      p		    : ExprNode; 
			      numParamWords : cardinal; 
			const name	    : array of char);
	var tn : TypeNode;
    begin (* GenWritef *)
	GenExpr(p, EVALGET);	    (* C file *)
	GenOp(PCPAR); I(numParamWords); EndLine;
	inc(numParamWords);
	p := p^.next;
	GenExpr(p, EVALPOINT);      (* Format string *)
	GenOp(PCPAR); I(numParamWords); EndLine;
	inc(numParamWords);
	p := p^.next;
	repeat
	    tn := BaseType(p^.exprType);
	    if tn^.kind in DataTypeSet{DTSTRING, DTARRAY} then
		GenExpr(p, EVALPOINT);
	    else
		GenExpr(p, EVALGET);
	    end;
	    GenOp(PCPAR); I(numParamWords); EndLine;
	    if tn^.kind = DTLONGREAL then
		inc(numParamWords, 2);
	    else
		inc(numParamWords);
	    end;
	    p := p^.next;
	until p = nil;
	GenLibCall(name, nil, 0, numParamWords);
    end GenWritef;

begin (* GenBuiltin *)
    p1 := nil;
    p2 := nil;
    pt1 := nil;
    pt2 := nil;
    if (params # nil) and (params^.first # nil) then
	p1 := params^.first;
	pt1 := p1^.exprType;
	if p1^.next # nil then
	    p2 := p1^.next;
	    pt2 := p2^.exprType
	end;
    end;

    case proc^.builtin of
    | BIPABS :
	GenExpr(p1,EVALGET);
	GenOpTL(PCABS,pt1);
    
    | BIPASSERT :
	lab := NewLabel();
	GenCondition(p1,lab,NULLLABEL);
	GenExpr(p2,EVALPOINT);
	GenOp(PCCHK); C('A'); EndLine;
	Lab(lab); GenOpL(PCLAB);

    | BIPCHR :
	GenExpr(p1,EVALGET);
	GenOpTL(PCTYP,charTypeNode);
 
    | BIPMIN :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	GenOpTL(PCMIN,pt1);
	
$if modula2 then
    | BIPCAP :
	GenExpr(p1,EVALGET);
	GenOpTL(PCCAP,pt1);
    
    | BIPDEC, BIPINC:
	p3 := p2^.next;
	pt1 := BaseType(p3^.exprType);
	if AlignmentOf(p3^.exprType) mod UNITSIZE = 0 then
	    GenExpr(p1,EVALPOINT);
	    GenExpr(p2,EVALGET);
	    if proc^.builtin = BIPINC then
		GenOp(PCAD2)
	    else
		GenOp(PCSB2)
	    end;
	    GenT(pt1); X; I(SizeOf(p3^.exprType));
	    EndLine;
	else (* turn into addition *)
	    GenExpr(p1, EVALPOINT);
	    GenOp(PCSAV); I(1); X; C('c'); EndLine;
	    GenIndirectVar(p3^.exprType, EVALGET);
	    GenExpr(p2,EVALGET);
	    if proc^.builtin = BIPINC then
		GenOpT(PCADD, pt1); 
	    else
		GenOpT(PCSUB, pt1);
	    end;
	    X; I(SizeOf(pt1)); EndLine;
	    GenOp(PCUSE); I(1); X; C('m'); EndLine;
	    GenIndirectVar(p3^.exprType, EVALPUT);
	end;

    | BIPINCL, BIPEXCL :
	GenExpr(p1,EVALPOINT);
	GenExpr(p2,EVALGET);
	if proc^.builtin = BIPINCL then
	    GenOp(PCSIN)
	else
	    GenOp(PCSEX)
	end;
	p3 := p2^.next;
	GenT(p3^.exprType); X; I(SizeOf(p3^.exprType));
	EndLine;

    | BIPFLOAT :
	GenExpr(p1,EVALGET);
	GenOpTL(PCFLT,realTypeNode);
    
    | BIPLONGFLOAT :
	GenExpr(p1,EVALGET);
	GenOpTL(PCFLT,longrealTypeNode);
    
    | BIPHALT :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('runtime__term', nil, 0, 1);
    
    | BIPMAX :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	GenOpTL(PCMAX,pt1);
$end    
    
    | BIPNEW
$if modula2 then
	, BIPLOCAL
$end
	    :
	(* param list is address, check, proc, dimensions, size, sizes *)
	bip := proc^.builtin;
$if modula2 then
	p3 := p2^.next;
	p4 := p3^.next;
	maybeNil := (bip = BIPNEW) and
	    (p3^.exprConst^.procVal^.builtin # BIPALLOCATE);
$else
	p4 := p2^.next;
	maybeNil := false;
$end
	p5 := p4^.next;
	ptrCheck := VAL(PointerKind, trunc(p2^.exprConst^.cardVal));
	numDimensions := trunc(p4^.exprConst^.cardVal);
	size := trunc(p5^.exprConst^.cardVal);
	akind := ARRAYNOCOUNT;

	(* get address of pointer, save it if need be *)
	(* if dynarray, compute size, use address to store sizes *)
	(* pass address and size *)
	(* if Modula-2 pointer, use address to update pointer *)

	if (numDimensions > 0) then
	    (* Dynamic array computations *)
	    atn := p1^.exprType^.dynArrayType;
	    dimensionsSize := SizeOf(atn) - WORDSIZE;
	    akind := atn^.arrayKind;
	    p1^.exprType := addressTypeNode;
	    (* address of pointer variable *)
	    GenExpr(p1, EVALPOINT);
	    (* save variable address *)
	    GenOp(PCSAV); I(0); X; C('m'); EndLine;
	    sizeen := p5^.next;
	    for i := 1 to numDimensions do
		(* get value of expression *)
		GenExpr(sizeen, EVALGET);
		(* Store size in descriptor unless ARRAYNOCOUNT *)
		if atn^.arrayKind # ARRAYNOCOUNT then
		    (* save it *)
		    GenOp(PCSAV); I(1); X; C('c'); EndLine;
		    (* get address of pointer variable *)
		    GenOp(PCUSE); I(0); X; C('c'); EndLine;
		    GenOpT(PCINC,addressTypeNode); X; 
			I(i*WORDSIZE);	EndLine;
		    (* store size in descriptor *)
		    GenIndirectVar(cardIntTypeNode,EVALPUT);
		    (* get count *)
		    GenOp(PCUSE); I(1); X; C('m'); EndLine;
		end;
		if i > 1 then
		    (* multiply by previous values *)
		    GenOp(PCUSE); I(2); X; C('m'); EndLine;
		    GenOpT(PCMUP,cardIntTypeNode); X; 
			I(WORDSIZE); EndLine;
		end;
		(* save away for next time *)
		GenOp(PCSAV); I(2); X; C('m'); EndLine;
		atn := atn^.elementType;
		sizeen := sizeen^.next;
	    end;
	    (* generate size *)
	    GenConstInteger(size);
	    (* multiply by number of elements *)
	    GenOp(PCUSE); I(2); X; C('m'); EndLine;
	    GenOpT(PCMUP,cardIntTypeNode); X; I(WORDSIZE);
	    EndLine;
	    if ptrCheck = PTRMODULA then
		(* add one word to size *)
		GenConstInteger(UNITSPERWORD);
		GenOpT(PCADD, cardIntTypeNode); X; I(WORDSIZE); EndLine;
(* |||
		GenOpT(PCINC,cardIntTypeNode); X; I(UNITSPERWORD);
		EndLine;
*)
	    end;
	    (* save size *)
	    GenOp(PCSAV); I(1); X; C('m'); EndLine;
	    if bip = BIPNEW then
		(* start call *)
		GenOp(PCMST); I(2); EndLine;
	    end;
	    (* pass address *)
	    if (ptrCheck = PTRMODULA) or (maybeNil and (akind # ARRAYNOCOUNT))
	    then (* need it later *)
		GenOp(PCUSE); I(0); X; C('c'); EndLine;
	    else (* last use *)
		GenOp(PCUSE); I(0); X; C('m'); EndLine;
	    end;
	    if bip = BIPNEW then
		GenOp(PCPAR); I(0); EndLine;
	    end;
	    (* pass size *)
	    GenOp(PCUSE); I(1); X; C('m'); EndLine;
	    if bip = BIPNEW then
		GenOp(PCPAR); I(1); EndLine;
	    end;
	elsif (ptrCheck = PTRMODULA) then
	    (* Normal pointer computations, save pointer address for later *)
	    GenExpr(p1,EVALPOINT);
	    GenOp(PCSAV); I(0); X; C('m'); EndLine;
	    if bip = BIPNEW then
		GenOp(PCMST); I(2); EndLine;
		GenOp(PCUSE); I(0); X; C('c'); EndLine;
		GenOp(PCPAR); I(0); EndLine;
		GenConstInteger(size + UNITSPERWORD);
		GenOp(PCPAR); I(1); EndLine;
	    else (* BIPLOCAL *)
		GenOp(PCUSE); I(0); X; C('c'); EndLine;
		GenConstInteger(size + UNITSPERWORD);
	    end;
	else
	    (* Normal pointer computations, don't save pointer address *)
	    if bip = BIPNEW then
		GenOp(PCMST); I(2); EndLine;
		GenExpr(p1,EVALPOINT);
		GenOp(PCPAR); I(0); EndLine;
		GenConstInteger(size);
		GenOp(PCPAR); I(1); EndLine;
	    else (* BIPLOCAL *)
		GenExpr(p1,EVALPOINT);
		GenConstInteger(size);
	    end;
	end;
$if modula2 then
	if bip = BIPNEW then
	    procNode := p3^.exprConst^.procVal;
	    if procNode^.builtin = BIPALLOCATE then
		(* builtin new *)
		if ptrCheck = PTRPASCAL then
		    GenLibCall('NEW', nil, 0, 2);
		elsif ptrCheck = PTRC then
		    GenLibCall('modmalloc', nil, 0, 2);
		else
		    GenLibCall('Storage_ALLOCATE', nil, 0, 2);
		end;
	    else
		(* user defined ALLOCATE *)
		GenCall (((procNode^.extern = GSNORMAL) and internalCallFlag)
			or ((procNode^.internalProc) and not OptNcall),
		    procNode^.globalName, procNode^.procType, 2);
	    end;
	else (* BIPLOCAL *)
	    GenOpL(PCSAL);
	end;
$else (* pascal *)
	GenLibCall('NEW', nil, 0, 2);
$end
	if ptrCheck = PTRMODULA then
	    (* Modula-2 checking, store check value and increment address *)
	    if maybeNil then
		nilLabel := NewLabel();
		(* get returned pointer value *)
		GenOp(PCUSE); I(0); X; C('c'); EndLine;
		GenIndirectVar(addressTypeNode,EVALGET);
		(* Is it NIL ? *)
		GenOp(PCLDC); C('n'); X; I(WORDSIZE); EndLine;
		GenOpT(PCEQU, addressTypeNode); X; I(WORDSIZE); EndLine;
		GenOp(PCTJP); Lab(nilLabel); EndLine;
	    end;
	    (* store address+wordsize at address *)
	    (* get returned pointer value *)
	    GenOp(PCUSE); I(0); X; C('c'); EndLine;
	    GenIndirectVar(addressTypeNode,EVALGET);
	    GenOp(PCSAV); I(1); X; C('c'); EndLine;
	    (* increment address *)
	    GenOpT(PCINC,addressTypeNode); X; I(WORDSIZE); EndLine;
	    (* save incremented address *)
	    GenOp(PCSAV); I(2); X; C('c'); EndLine;
	    (* get original address *)
	    GenOp(PCUSE); I(1); X; C('m'); EndLine;
	    (* save incremented address at original address *)
	    GenIndirectVar(addressTypeNode, EVALPUT);
	    (* get incremented address *)
	    GenOp(PCUSE); I(2); X; C('m'); EndLine;

	    if maybeNil then
		if akind # ARRAYNOCOUNT then
		    (* get pointer address *)
		    GenOp(PCUSE); I(0); X; C('c'); EndLine;
		    (* store value *)
		    GenIndirectVar(addressTypeNode,EVALPUT);
		    lab := NewLabel();
		    GenOp(PCUJP); Lab(lab); EndLine;
		    Lab(nilLabel); GenOpL(PCLAB);
		    (* Zero out descriptor *)
		    GenOp(PCLDC); C('n'); X; I(dimensionsSize);	EndLine;
		    GenOp(PCUSE); I(0); X; C('m'); EndLine;
		    GenOpT(PCINC, addressTypeNode); X; I(WORDSIZE); EndLine;
		    GenOpT(PCSTO, addressTypeNode); X; I(dimensionsSize);
			EndLine;
		    Lab(lab); GenOpL(PCLAB);
		else
		    (* get pointer address *)
		    GenOp(PCUSE); I(0); X; C('m'); EndLine;
		    (* store value *)
		    GenIndirectVar(addressTypeNode,EVALPUT);
		    Lab(nilLabel); GenOpL(PCLAB);
		end;
	    else
		(* get pointer address *)
		GenOp(PCUSE); I(0); X; C('m'); EndLine;
		(* store value *)
		GenIndirectVar(addressTypeNode,EVALPUT);
	    end;

	elsif maybeNil and (akind # ARRAYNOCOUNT) then
	    (* Store zeros into dimensions if NIL returned *)
	    nilLabel := NewLabel();
	    (* get returned pointer value *)
	    GenOp(PCUSE); I(0); X; C('c'); EndLine;
	    GenIndirectVar(addressTypeNode,EVALGET);
	    (* Is it NIL ? *)
	    GenOp(PCLDC); C('n'); X; I(WORDSIZE); EndLine;
	    GenOpT(PCEQU, addressTypeNode); X; I(WORDSIZE); EndLine;
	    GenOp(PCFJP); Lab(nilLabel); EndLine;

	    (* Zero out descriptor *)
	    GenOp(PCLDC); C('n'); X; I(dimensionsSize);	EndLine;
	    GenOp(PCUSE); I(0); X; C('m'); EndLine;
	    GenOpT(PCINC, addressTypeNode); X; I(WORDSIZE); EndLine;
	    GenOpT(PCSTO, addressTypeNode); X; I(dimensionsSize);
		EndLine;
	    Lab(nilLabel); GenOpL(PCLAB);
	end;
    
    | BIPDISPOSE :
	(* param list is address, check, proc, dimensions, size *)
	p3 := p2^.next;
$if modula2 then
	procNode := p3^.exprConst^.procVal;
$end
	p4 := p3^.next;
	p5 := p4^.next;
	ptrCheck := VAL(PointerKind, trunc(p2^.exprConst^.cardVal));
	numDimensions := trunc(p4^.exprConst^.cardVal);
	size := trunc(p5^.exprConst^.cardVal);
	(* get address of pointer, save it *)
	(* if dynarray, use address to compute size *)
	(* save size *)
	(* if Modula-2 pointer, use address to update pointer *)
	(* pass address and size *)
	(* address of pointer variable *)
	if (numDimensions > 0) then
	    atn := p1^.exprType^.dynArrayType;
	    p1^.exprType := addressTypeNode;
	end;
	GenExpr(p1,EVALPOINT);
	if p1^.doCheck and
	    (ptrCheck in PointerKindSet{PTRMODULA, PTRPASCAL, PTRNILCHECK})
	then
	    (* make sure pointer is valid before disposing it *)
	    GenOp(PCCHK); C('d'); X;
		if ptrCheck = PTRMODULA then
		    C('m');
		elsif ptrCheck = PTRPASCAL then
		    C('p');
		else
		    C('n');
		end;
	    EndLine;
	end;
	(* save variable address *)
	GenOp(PCSAV); I(0); X; C('m'); EndLine;
	sizeen := p5^.next;
	for i := 1 to numDimensions do
	    (* Get number of elements in this dimension *)
	    if atn^.arrayKind = ARRAYNOCOUNT then
		GenExpr(sizeen, EVALGET);
		sizeen := sizeen^.next;
	    else
		(* get address of pointer variable *)
		GenOp(PCUSE); I(0); X; C('c'); EndLine;
		GenOpT(PCINC,addressTypeNode); X; 
		I(i*WORDSIZE);
		EndLine;
		GenIndirectVar(cardIntTypeNode,EVALGET);
	    end;
	    if i > 1 then
		(* multiply by previous values *)
		GenOpT(PCMUP,cardIntTypeNode); X; I(WORDSIZE);
		EndLine;
	    end;
	    atn := atn^.elementType;
	end;
	(* generate size *)
	GenConstInteger(size);
	if numDimensions > 0 then
	    (* multiply by number of elements *)
	    GenOpT(PCMUP,cardIntTypeNode); X; I(WORDSIZE);
	    EndLine;
	end;
	if ptrCheck = PTRMODULA then
	    (* add one word to size *)
	    GenConstInteger(UNITSPERWORD);
	    GenOpT(PCADD, cardIntTypeNode); X; I(WORDSIZE); EndLine;
	    
(* |||
	    GenOpT(PCINC,cardIntTypeNode); X; I(UNITSPERWORD);
	    EndLine;
*)
	end;
	(* save size *)
	GenOp(PCSAV); I(1); X; C('m'); EndLine;
	if ptrCheck = PTRMODULA then
	    (* get address *)
	    GenOp(PCUSE); I(0); X; C('c'); EndLine;
	    (* decrement pointer to original beginning of area *)
	    GenConstInteger(WORDSIZE div BYTESIZE);
	    GenOpT(PCSB2,cardIntTypeNode); X; I(WORDSIZE); 
		EndLine;
	end;
	(* call dispose routine *)
	GenOp(PCMST); I(2); EndLine;
	(* pass address of pointer variable *)
	GenOp(PCUSE); I(0); X; C('m'); EndLine;
	GenOp(PCPAR); I(0); EndLine;
	(* pass size *)
	GenOp(PCUSE); I(1); X; C('m'); EndLine;
	GenOp(PCPAR); I(1); EndLine;
$if modula2 then
	if procNode^.builtin = BIPDEALLOCATE then
	    (* builtin dispose *)
	    if ptrCheck = PTRPASCAL then
		GenLibCall('DISPOSE', nil, 0, 2);
	    elsif ptrCheck = PTRC then
		GenLibCall('free', nil, 0, 2);
	    else
		GenLibCall('Storage_DEALLOCATE', nil, 0, 2);
	    end;
	else
	    (* user defined dispose *)
	    GenCall (((procNode^.extern = GSNORMAL) and internalCallFlag)
		    or ((procNode^.internalProc) and not OptNcall),
		procNode^.globalName,procNode^.procType,2);
	end;
$else (* pascal *)
	if p3^.exprConst^.cardVal = 1.0 then
	    (* Close any open files being disposed *)
	    GenLibCall('DFDISPOSE', nil, 0, 2);
	else
	    GenLibCall('DISPOSE', nil, 0, 2);
	end;
$end

$if modula2 then
    | BIPALLOCATE, BIPDEALLOCATE :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1,EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2,EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	if proc^.builtin = BIPALLOCATE then
	    GenLibCall('MEMORY_ALLOCATE', nil, 0, 2);
	else
	    GenLibCall('MEMORY_DEALLOCATE', nil, 0, 2);
	end;
$end (* modula2 *)
    
    | BIPODD :
	GenExpr(p1,EVALGET);
	GenOpL(PCODD);
    
    | BIPORD :
	GenExpr(p1,EVALGET);
	GenOpL(PCORD);
    
    | BIPTRUNC :
	GenExpr(p1,EVALGET);
	GenOpTL(PCTRC,pt1);
    
$if modula2 then
    (* CED - 8/13/87  Used only in Colorado Springs.  Don't ask. *)
    | BIPXFC :
	GenExpr(p1,EVALGET);	(* one parameter *)
	GenOp(PCXFC);		(* what follows is the return type *)
	    GenT(integerTypeNode); X; I(WORDSIZE); X;
	    I(1);		(* there is only one argument for the *)
	    EndLine;		(* micro-code procedure *)
    
    | BIPVAL :
	GenExpr(p2,EVALGET);
	GenOpTL(PCTYP,pt1);
    
    | BIPADR :
	GenExpr(p1,EVALPOINT);
    
    | BIPWRITEF, BIPSWRITEF :
	p := p2^.next;
	numParamWords := 2;
	while p # nil do
	    inc(numParamWords);
	    if p^.exprType^.kind in DataTypeSet{DTREAL,DTLONGREAL} then
		inc(numParamWords);
	    end;
	    p := p^.next;
	end;
	GenOp(PCMST); I(numParamWords); EndLine;
	if proc^.builtin = BIPWRITEF then
	    GenExpr(p1,EVALGET);
	else
	    GenExpr(p1, EVALPOINT);
	end;
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2,EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	numParamWords := 2;
	p := p2^.next;
	while p # nil do
	    tn := BaseType(p^.exprType);
	    case tn^.kind of
	    | DTARRAY,DTSTRING :
		p^.exprType := addressTypeNode;
		GenExpr(p,EVALPOINT);
	    | DTBOOLEAN, DTENUMERATION :
		GenOp(PCMST); I(2); EndLine;
		GenExpr(p, EVALGET);
		(* If first(EnumerationType) > 0 - you need to fix that
		to get a 0 offset into the EnumNameTable - CED 4/7/87 *)
		if tn^.kind = DTENUMERATION then
		    if tn^.enumMin > 0 then
			GenConstInteger(tn^.enumMin);
			GenOpT(PCSUB,integerTypeNode);
			X;
			I(WORDSIZE);
			EndLine;
		    end;
		end;
		GenOp(PCPAR); I(0); EndLine;
		GenVar(tn^.nameTable, EVALPOINT);
		GenOp(PCPAR); I(1); EndLine;
		GenLibCall('NAM', addressTypeNode, WORDSIZE, 2);
	    | DTREAL :
		(* make float be double *)
		GenExpr(p,EVALGET);
		GenOpTL(PCFLT, longrealTypeNode);
	    | else
		GenExpr(p,EVALGET);
	    end (* case tn^.kind *);
	    GenOp(PCPAR); I(numParamWords); EndLine;
	    inc(numParamWords);
	    if tn^.kind in DataTypeSet{DTREAL, DTLONGREAL} then
		inc(numParamWords);
	    end;
	    p := p^.next;
	end;
	if proc^.builtin = BIPWRITEF then
	    GenLibCall('fprintf', nil, 0, numParamWords);
	else
	    GenLibCall('sprintf', nil, 0, numParamWords);
	end;
    
    | BIPREADF, BIPSREADF :
	p := p2^.next;
	numParamWords := 2;
	while p # nil do
	    inc(numParamWords);
	    p := p^.next;
	end;
	GenOp(PCMST); I(numParamWords); EndLine;
	numParamWords := 0;
	if proc^.builtin = BIPREADF then
	    GenExpr(p1, EVALGET);
	else
	    GenExpr(p1, EVALPOINT);
	end;
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2,EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	numParamWords := 2;
	p := p2^.next;
	while p # nil do
	    if p^.exprType = ioStringTypeNode then
		p^.exprType := addressTypeNode;
		GenExpr(p, EVALPOINT);
		p^.exprType := ioStringTypeNode;
	    else
		GenExpr(p,EVALPOINT);
	    end;
	    GenOp(PCPAR); I(numParamWords); EndLine;
	    inc(numParamWords);
	    p := p^.next;
	end;
	if proc^.builtin = BIPREADF then
	    GenLibCall('fscanf', integerTypeNode, WORDSIZE, numParamWords);
	else
	    GenLibCall('sscanf', integerTypeNode, WORDSIZE, numParamWords);
	end;
    
    | BIPWRITES :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p2, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('fputs', nil, 0, 2);

    | BIPWRITEB :
	p3 := p2^.next;
	GenOp(PCMST); I(4); EndLine;
	GenExpr(p2,EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenConstInteger(1);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p3,EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(3); EndLine;
	GenLibCall('fwrite', nil, 0, 4);

    | BIPREADB :
	p3 := p2^.next;
	GenOp(PCMST); I(4); EndLine;
	GenExpr(p2,EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenConstInteger(1);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p3,EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(3); EndLine;
	GenLibCall('fread', integerTypeNode, WORDSIZE, 4);
    
    | BIPWRITEC :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p2,EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('fputc', nil, 0, 2);
    
    | BIPREADC :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('fgetc', charTypeNode, BYTESIZE, 1);
	GenStore(p2,charTypeNode);
	GenExpr(p1,EVALGET);
$IF vms THEN	
	(* VMS has an extra level of indirection for IO - CED 4/1/87 *)
	GenOpT(PCIND,addressTypeNode); X; I(WORDSIZE); EndLine;
$END
	GenOpT(PCINC,addressTypeNode); X; I(EOFOFFSET); EndLine;
	GenOpT(PCIND,cardinalTypeNode); X; I(2); EndLine;
	GenOpTL(PCNEG,integerTypeNode);
    
    | BIPREADS :
	GenOp(PCMST); I(3); EndLine;
	GenExpr(p2, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	p3 := p2^.next;
	GenExpr(p3, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenLibCall('fgets', integerTypeNode, WORDSIZE, 3);
	(* Result is non-zero if read went okay *)
	GenConstInteger(0);
	GenOpT(PCNEQ, integerTypeNode); X; I(WORDSIZE); EndLine;

    | BIPOPENF :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1,EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2,EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('fopen', fileTypeNode, WORDSIZE, 2);
    
    | BIPCLOSEF :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('fclose', nil, 0, 1);
    
    | BIPNEWPROCESS :
	p3 := p2^.next;
	p4 := p3^.next;
	GenOp(PCMST); I(4); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2,EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p3,EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenExpr(p4,EVALPOINT);
	GenOp(PCPAR); I(3); EndLine;
	GenLibCall('SYSTEM_newprocess', nil, 0, 4);
    
    | BIPTRANSFER :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1,EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2,EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('SYSTEM_transfer', nil, 0, 2);
    
    | BIPCPUTIME :
	GenOp(PCMST); I(0); EndLine;
	GenLibCall('SYSTEM_cputime', integerTypeNode, WORDSIZE, 0);
    
    | BIPBITNOT :
	GenExpr(p1,EVALGET);
	GenOp(PCBIT); C('n'); EndLine;
    
    | BIPBITAND :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	GenOp(PCBIT); C('a'); EndLine;
    
    | BIPBITOR :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	GenOp(PCBIT); C('o'); EndLine;
    
    | BIPBITXOR :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	GenOp(PCBIT); C('x'); EndLine;
    
    | BIPBITSHIFTLEFT :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	GenOp(PCBIT); C('l'); EndLine;
    
    | BIPBITSHIFTRIGHT :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	GenOp(PCBIT); C('r'); EndLine;
    
    | BIPBITEXTRACT :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	p3 := p2^.next;
	GenExpr(p3,EVALGET);
	GenOp(PCBIT); C('e'); EndLine;
    
    | BIPBITINSERT :
	GenExpr(p1,EVALGET);
	GenExpr(p2,EVALGET);
	p3 := p2^.next;
	GenExpr(p3,EVALGET);
	p4 := p3^.next;
	GenExpr(p4,EVALGET);
	GenOp(PCBIT); C('i'); EndLine;
    
$else (* pascal *)
    | BIPwritec :
	p3 := p2^.next;
	GenOp(PCMST); I(3); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p3, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p3^.next, EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenLibCall('WRITEC', nil, 0, 3);

    | BIPfputc :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('fputc', nil, 0, 2);

    | BIPwritef :
	numParamWords := 3;
	p3 := p2^.next;
	p4 := p3^.next;
	p := p4^.next;
	repeat
	    if p^.exprType^.kind = DTLONGREAL then
		inc(numParamWords, 2);
	    else
		inc(numParamWords);
	    end;
	    p := p^.next;
	until p = nil;
	GenOp(PCMST); I(numParamWords); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenWritef(p2^.next, 1, 'WRITEF');

    | BIPfprintf :
	numParamWords := 2;
	p := p2^.next;
	repeat
	    if p^.exprType^.kind = DTLONGREAL then
		inc(numParamWords, 2);
	    else
		inc(numParamWords);
	    end;
	    p := p^.next;
	until p = nil;
	GenOp(PCMST); I(numParamWords); EndLine;
	GenWritef(p1, 0, 'fprintf');

    | BIPwriteln :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('WRITLN', nil, 0, 1);

    | BIPwrites :
	p3 := p2^.next;
	p4 := p3^.next;
	p5 := p4^.next;
	GenOp(PCMST); I(5); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p3, EVALPOINT);		(* Address of string *)
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p4, EVALGET);		(* Length of string *)
	GenOp(PCPAR); I(2); EndLine;
	GenExpr(p5, EVALGET);		(* Size of data (1 byte) *)
	GenOp(PCPAR); I(3); EndLine;
	GenExpr(p5^.next, EVALGET);     (* Unix file *)
	GenOp(PCPAR); I(4); EndLine;
	GenLibCall('WRITES', nil, 0, 5);

    | BIPfwrite :
	p3 := p2^.next;
	GenOp(PCMST); I(4); EndLine;
	GenExpr(p1, EVALPOINT);		(* Address of string *)
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALGET);		(* Length of string *)
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p3, EVALGET);		(* Size of data (1 byte) *)
	GenOp(PCPAR); I(2); EndLine;
	GenExpr(p3^.next, EVALGET);     (* Unix file *)
	GenOp(PCPAR); I(3); EndLine;
	GenLibCall('fwrite', nil, 0, 4);

    | BIPmax :
	GenOp(PCMST); I(3); EndLine;
	GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p2^.next, EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenLibCall('MAX', integerTypeNode, WORDSIZE, 3);

    | BIPnam :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('NAM', addressTypeNode, WORDSIZE, 2);

    | BIPreadln :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('READLN', nil, 0, 1);

    | BIPfnil :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('FNIL', addressTypeNode, WORDSIZE, 1);

    | BIPread4 :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('READ4', integerTypeNode, WORDSIZE, 1);

    | BIPread8 :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('READ8', longrealTypeNode, LONGREALSIZE, 1);

    | BIPreade :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2^.next, EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('READE', integerTypeNode, WORDSIZE, 2);

    | BIPreadc :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('READC', charTypeNode, BYTESIZE, 1);

    | BIPget :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('GET', nil, 0, 1);
	
    | BIPput :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('PUT', nil, 0, 1);
	
    | BIPunit :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	GenOp(PCPAR); I(0); EndLine;
        GenLibCall('UNIT', addressTypeNode, WORDSIZE, 1);

    | BIPpage :
	GenOp(PCMST); I(1); EndLine;  (* for PAGE *)
	GenOp(PCMST); I(1); EndLine;  (* for UNIT *)
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('UNIT', addressTypeNode, WORDSIZE, 1);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('PAGE', nil, 0, 1);
	
    | BIPflush :
	if p1 = nil then
	    GenOp(PCMST); I(0); EndLine;
	    GenLibCall('PFLUSH', nil, 0, 0);
	else
	    GenOp(PCMST); I(1); EndLine;
	    GenExpr(p1, EVALPOINT);
	    GenOp(PCPAR); I(0); EndLine;
	    GenLibCall('FLUSH', nil, 0, 1);
	end;

    | BIPeof :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('TEOF', booleanTypeNode, BYTESIZE, 1);

    | BIPeoln :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('TEOLN', booleanTypeNode, BYTESIZE, 1);

    | BIPlinelimit :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('LLIMIT', nil, 0, 2);

    | BIPstlimit :
	GenOp(PCMST); I(1); EndLine;
        GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(0); EndLine;
        GenLibCall('STLIM', nil, 0, 1);

    | BIPremove :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('REMOVE', nil, 0, 2);
	
    | BIPreset, BIPrewrite :
	p3 := p2^.next;
	GenOp(PCMST); I(4); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	if pt2^.kind = DTINTEGER then (* reset(f) case *)
	    GenConstInteger(0);
	else
	    GenExpr(p2, EVALPOINT);
	end;
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p3, EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenExpr(p3^.next, EVALGET);
	GenOp(PCPAR); I(3); EndLine;
	if proc^.builtin = BIPreset then
	    GenLibCall('RESET', nil, 0, 4);
	else
	    GenLibCall('REWRITE', nil, 0, 4);
	end;

    | BIPpack,
      BIPunpack :
	p3 := p2^.next;
	p4 := p3^.next;
	p5 := p4^.next;
	p6 := p5^.next;
	GenOp(PCMST); I(7); EndLine;
	GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p3, EVALPOINT);
	GenOp(PCPAR); I(2); EndLine;
	GenExpr(p4, EVALGET);
	GenOp(PCPAR); I(3); EndLine;
	GenExpr(p5, EVALGET);
	GenOp(PCPAR); I(4); EndLine;
	GenExpr(p6, EVALGET);
	GenOp(PCPAR); I(5); EndLine;
	GenExpr(p6^.next, EVALGET);
	GenOp(PCPAR); I(6); EndLine;
	if proc^.builtin = BIPpack then
	    GenLibCall('PACK', nil, 0, 7);
	else
	    GenLibCall('UNPACK', nil, 0, 7);
	end;
	
    | BIPargc :
	GenExpr(p1, EVALGET);
	
    | BIPargv :
	GenOp(PCMST); I(3); EndLine;
	GenExpr(p1, EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALPOINT);
	GenOp(PCPAR); I(1); EndLine;
	GenExpr(p2^.next, EVALGET);
	GenOp(PCPAR); I(2); EndLine;
	GenLibCall('ARGV', nil, 0, 3);

    | BIPsqr :
	GenExpr(p1, EVALGET);
	(* Duplicate top of stack *)
	GenOp(PCSAV); I(0); X; C('c'); EndLine;
	GenOp(PCUSE); I(0); X; C('m'); EndLine;
	GenOpT(PCMUP, pt1); X; I(SizeOf(pt1)); EndLine;

    | BIPsqrt, BIPsin, BIPcos, BIPexp, BIPln, BIParctan :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall(mathLibProcNames[proc^.builtin, p1^.doCheck], 
	    longrealTypeNode, LONGREALSIZE, 2);

    | BIPlongfloat :
	GenExpr(p1,EVALGET);
	GenOpTL(PCFLT,longrealTypeNode);
    
    | BIPround :
	GenExpr(p1, EVALGET);
	if pt1^.kind = DTLONGREAL then
	    GenOpTL(PCRND, pt1);
	(* else DTINTEGER, no need to round *)
	end;

    | BIPexpo :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1,EVALGET);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('EXPO', integerTypeNode, WORDSIZE, 2);

    | BIPsucc, BIPpred :
	if p1^.doCheck then
	    GenOp(PCMST); I(3); EndLine;
	    GenExpr(p1, EVALGET);
	    GenOp(PCPAR); I(0); EndLine;
	    GenConstInteger(trunc(LowerBoundOf(pt1)));
	    GenOp(PCPAR); I(1); EndLine;
	    GenConstInteger(trunc(UpperBoundOf(pt1)));
	    GenOp(PCPAR); I(2); EndLine;
	    if proc^.builtin = BIPsucc then
		GenLibCall('SUCC', pt1, SizeOf(pt1), 3);
	    else
		GenLibCall('PRED', pt1, SizeOf(pt1), 3);
	    end;
	else
	    GenExpr(p1, EVALGET);
	    GenConstInteger(1);
	    if proc^.builtin = BIPsucc then
		GenOpT(PCADD, pt1); 
	    else
		GenOpT(PCSUB, pt1);
	    end;
	    X; I(SizeOf(pt1)); EndLine;
	end;

    | BIPcard :
	GenOp(PCMST); I(2); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenExpr(p2, EVALGET);
	GenOp(PCPAR); I(1); EndLine;
	GenLibCall('CARD', integerTypeNode, WORDSIZE, 2);

    | BIPseed :
	(* Get current value of _seed and set aside *)
	p3 := p2^.next;
	GenExpr(p3, EVALGET);
	GenOp(PCSAV); I(0); X; C('m'); EndLine;
	(* Store new value *);
	GenExpr(p1, EVALGET);
	GenStore(p2, integerTypeNode);
	(* Leave old value on stack *)
	GenOp(PCUSE); I(0); X; C('m'); EndLine;

    | BIPrandom :
	GenExpr(p1,EVALGET);
	(* Throw away value *)
	GenOp(PCSAV); I(0); X; C('d'); EndLine;
	GenOp(PCMST); I(0); EndLine;
	GenLibCall('RANDOM', longrealTypeNode, LONGREALSIZE, 0);
	
    | BIPundefined :
	GenExpr(p1,EVALGET);
	(* Throw away value, and push FALSE *)
	GenOp(PCSAV); I(0); X; C('d'); EndLine;
	GenConstBoolean(false);

    | BIPdate :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('DATE', nil, 0, 1);

    | BIPtime :
	GenOp(PCMST); I(1); EndLine;
	GenExpr(p1, EVALPOINT);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('TIME', nil, 0, 1);

    | BIPclock :
	GenOp(PCMST); I(0); EndLine;
	GenLibCall('CLCK', integerTypeNode, WORDSIZE, 0);

    | BIPsysclock :
	GenOp(PCMST); I(0); EndLine;
	GenLibCall('SCLCK', integerTypeNode, WORDSIZE, 0);

    | BIPwallclock :
	GenOp(PCMST); I(1); EndLine;
	GenConstInteger(0);
	GenOp(PCPAR); I(0); EndLine;
	GenLibCall('time', integerTypeNode, WORDSIZE, 1);

    | BIPhalt :
	GenOp(PCMST); I(0); EndLine;
	GenLibCall('HALT', nil, 0, 0);
	
    | BIPnull :
	(* nothing at all *);

$end

    end (* case *);
end GenBuiltin;

$if pascal then
begin (* BuiltinPC *)
(* Note that sin, cos, and atan never return errors *)
    mathLibProcNames[BIPsin,    false] := 'sin';
    mathLibProcNames[BIPsin,    true ] := 'sin';
    mathLibProcNames[BIPcos,    false] := 'cos';
    mathLibProcNames[BIPcos,    true ] := 'cos';
    mathLibProcNames[BIPexp,    false] := 'exp';
    mathLibProcNames[BIPexp,    true ] := 'EXP';
    mathLibProcNames[BIPln,     false] := 'log';
    mathLibProcNames[BIPln,     true ] := 'LN';
    mathLibProcNames[BIPsqrt,   false] := 'sqrt';
    mathLibProcNames[BIPsqrt,   true ] := 'SQRT';
    mathLibProcNames[BIParctan, false] := 'atan';
    mathLibProcNames[BIParctan, true ] := 'atan';
$end
end BuiltinPC.
