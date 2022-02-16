implementation module Alloc;

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
    Writef, output;

from MemLib import
    ALLOCATE;

from Machine import
    WORDSIZE;

from Strings import
    String;

from Globals import
    TraceDecls, DEBUG;

from Symbols import
    MemoryType, MemoryOffset, GlobalSymKind, FormerNode, GlobalVarNode, 
    AllocationNode, ProcNode, Address;

var
    globalVarNumber : integer;


procedure InitAllocationNode (): AllocationNode;
var
    an : AllocationNode;
begin
    new(an);
    an^.current[MEMNORMAL] := 0;
    an^.maximum[MEMNORMAL] := 0;
    an^.current[MEMFAST] := 0;
    an^.maximum[MEMFAST] := 0;
    an^.current[MEMPARAM] := 0;
    an^.maximum[MEMPARAM] := 0;
    return an;
end InitAllocationNode;


procedure RoundUp(const value, multiple : MemoryOffset) : MemoryOffset;
begin
    return multiple * ((value + multiple - 1) div multiple);
end RoundUp;


procedure AllocateGlobal(const name	    : String; 
			       size	    : MemoryOffset;
			 const extern       : GlobalSymKind;
			 const defineMemory : boolean;
			 const value        : FormerNode;
			 var   address      : Address); 
(* MEMGLOBAL is always word-aligned. *)
var
    gvn : GlobalVarNode;
begin
    size := RoundUp(size,WORDSIZE);
    new(gvn);
    gvn^.globalName := name;
    gvn^.number := globalVarNumber;
    globalVarNumber := globalVarNumber + 1;
    gvn^.size := size;
    gvn^.extern := extern;
    gvn^.defineMemory := defineMemory;
    gvn^.used := false;
    gvn^.shared := false;
    gvn^.value := value;
    address.kind := MEMGLOBAL;
    address.gvn := gvn;
    if globalVarList^.first = nil then
	globalVarList^.first := gvn;
    else
        globalVarList^.last^.next := gvn;
    end;
    globalVarList^.last := gvn;
    gvn^.next := nil;
end AllocateGlobal;


(* MEMFAST and MEMPARAM are always word-aligned, MEMNORMAL is bit-aligned 
   as specified. *)

procedure AllocateMemory(const an        : AllocationNode; 
			       mt        : MemoryType;
			       size      : MemoryOffset;
			 const alignment : MemoryOffset; 
			 const proc      : ProcNode;
			 var   address   : Address); 
var
    offset : MemoryOffset;
begin
    assert(mt # MEMGLOBAL);
    offset := an^.current[mt];
    if mt = MEMNORMAL then
	offset := RoundUp(offset,alignment);
    else
	size := RoundUp(size,WORDSIZE);
	offset := RoundUp(offset,WORDSIZE);
    end;
    address.kind := mt;
    address.proc := proc;
    if proc # nil then
	address.level := proc^.displayLevel;
    else
	address.level := 0;
    end;
    address.offset := offset;
    address.upLevelAddr := false;
    if DEBUG and TraceDecls then
	Writef(output, 'AllocateMemory: size=%d, align=%d, offset=%d, %n\n',
		size, alignment, offset, mt);
    end;
    an^.current[mt] := offset + size;
    if an^.current[mt] > an^.maximum[mt] then
	an^.maximum[mt] := an^.current[mt];
    end;
end AllocateMemory;


begin (* Alloc *)
(* Initialize globalVarList and globalVarNumber. *)
    new(globalVarList);
    globalVarList^.first := nil;
    globalVarList^.last  := nil;
    globalVarNumber := 1;
end Alloc.
