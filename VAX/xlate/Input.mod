implementation module Input;

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


import ASCII;
from ASCII import CharSet;

from Error import Error;

from Machine import RefillBuffer, buff, nextchar, chptr, numchread, endOfFile;

from PCodeOps import
    PCodeOp, PCodeOpName, pCodeOpNames;

from Types import 
    operandsize, operandstring, opcodefieldsize, opdsizeptr, 
    opdsizearray, opdtype, opdptr;

from Vars import
    pclabel, pclbsize, opcode, opcodestr, opd, opdsizes, opdcount, opd11,
    npclabel, npclbsize, nopcode, nopcodestr, nopd, nopdsizes, nopdcount,
    line;

from Hash import 
    HashRange, codeForLetter;

const
    opdsep = ',';

(* opcode hash table *)
type
    HashTable = array HashRange of PCodeOp;
var
    hashTable : HashTable;    

procedure @inline ReadChar();
begin
    (* if buffer is empty, refill it *)
    if chptr >= numchread then
	RefillBuffer();
    end;
    
    (* buffer is sure to be full at this point *)
    (* get the next character from the buffer *)
    nextchar := buff[chptr];
    chptr := chptr + 1;
end ReadChar;


procedure Advance();
var
    j : integer;
    topd : opdptr;
    topdsizes : opdsizeptr;
begin
    line := line + 1;
    pclbsize := npclbsize;
    for j := 1 to pclbsize+1 do
	pclabel[j] := npclabel[j];
    end;
    opcode := nopcode;
    opcodestr := nopcodestr;
    opdcount := nopdcount;
    topd := opd;
    opd := nopd;
    nopd := topd;
    topdsizes := opdsizes;
    opdsizes := nopdsizes;
    nopdsizes := topdsizes;
    opd11 := opd^[1][1];
end Advance;

procedure @inline ReadLabel(var pclabel:operandstring; var pclbsize : integer);
begin
    pclbsize:=0;
    while (nextchar#ASCII.TAB) do
	pclbsize:= pclbsize+1;
	pclabel[pclbsize] := nextchar;
	ReadChar();
    end;
    pclabel[pclbsize+1] := 0C;
end ReadLabel;            (* end of readlabel *)

var
    numcalls, numprobes : integer;

procedure @inline ReadOpcode(var opcode:PCodeOp; var opcodestr: PCodeOpName);
var
    extraHash   : cardinal;
    tableIndex  : cardinal;
begin
    (* read opcode values into opcodestr in string form *)
    assert(nextchar=ASCII.TAB, 'Tab expected');
    ReadChar();
    opcodestr[1] := nextchar;
    ReadChar();
    opcodestr[2] := nextchar;
    ReadChar();
    opcodestr[3] := nextchar;
    ReadChar;

(* look up the opcode in the hash table *)
    tableIndex := codeForLetter[opcodestr[1]] +
		  codeForLetter[opcodestr[3]] +
		  ORD(opcodestr[2]) - ORD('a');
    
    if tableIndex <= LAST(HashRange) THEN
	opcode := hashTable[tableIndex];
    else
        opcode := PCZZZ;
    end;

end ReadOpcode;

procedure @inline ReadOperand(var opd : operandstring; var opdsize : integer);
var
    k, ordch, i : integer;
begin
    k:=0;
    if (nextchar='"') then
	(* read in a string, ends with ", watch for \ *)
	ReadChar();
	while nextchar # '"' do
	    k:=k+1;
	    if nextchar = '\\' then
		ReadChar();
		if nextchar in CharSet{'0'..'7'} then
		    (* read \nnn *)
		    ordch := 0;
		    i := 1;
		    while (i <= 3) and (nextchar in CharSet{'0'..'7'}) do
			ordch := ordch * 8 + ord(nextchar) - ord('0');
			ReadChar();
			i := i + 1;
		    end;
		    opd[k] := chr(ordch);
		else
		    (* accept \? *)
		    opd[k] := nextchar;
		    ReadChar();
		end;
	    else
		(* regular character *)
		opd[k] := nextchar;
		ReadChar();
	    end;
	end;
	(* skip trailing " *)
	ReadChar();
    else
	(* not a string operand *)
	while (nextchar#opdsep) and (nextchar # ASCII.LF) do
	    k:=k+1;
	    opd[k] := nextchar;
	    ReadChar();
	end;
    end;
    if (k>=operandsize) then
	Error('ReadOperand: operand too long');
    end;
    opd[k+1] := 0C;  
    opdsize:=k;
end ReadOperand;

procedure @inline ReadOperands( var opd      : opdtype; 
			var opdsizes : opdsizearray; 
			var opdcount : integer);
begin
    opdcount:=0;
    if nextchar=ASCII.TAB then
	ReadChar();
    end;
    if nextchar # ASCII.LF then
	loop
	    opdcount := opdcount + 1 ;
	    ReadOperand(opd[opdcount],opdsizes[opdcount]);
	    (* skip operand separator *)
	    if (nextchar=opdsep) then
		ReadChar();
	    else
		exit;
	    end;
	end;
	(* flush rest of line *)
	while nextchar # ASCII.LF do
	    ReadChar();
	end;
    end;
    (* Read LF *)
    ReadChar();
end ReadOperands;

procedure PreRead;
var
    i : integer;
begin
    while not endOfFile and (nextchar = '#') do
	(* Skip over comment line *)
	while nextchar # ASCII.LF do
	    ReadChar();
	end;
	ReadChar();
    end;
    if not endOfFile then
	ReadLabel(npclabel,npclbsize);
	ReadOpcode(nopcode,nopcodestr);
	ReadOperands(nopd^,nopdsizes^,nopdcount);
    end;
end PreRead;

procedure EnterOpcode(codeString : PCodeOpName; codeType   : PCodeOp);
var
   tableIndex : integer;     (* the index to the opcode look up table *)
		     
begin
    tableIndex := codeForLetter[codeString[1]] +
		  codeForLetter[codeString[3]] +
		  ORD(codeString[2]) - ORD('a');
    
    assert((tableIndex <= LAST(HashRange)) and 
	    (hashTable[tableIndex] = PCZZZ),
	'New codeLetter array should be computed with HashGen.mod');
    
    hashTable[tableIndex] := codeType;
end EnterOpcode;

    var i  : HashRange;
	op : PCodeOp;
begin

    line:=0;

     
    new(opd);
    new(opdsizes);
    new(nopd);
    new(nopdsizes);
    nopcodestr[opcodefieldsize] := 0C;

(* initialize opcode lookup table *)
    for i := 0 to LAST(HashRange) do
	hashTable[i] := PCZZZ;
    end;
    for op := FIRST(PCodeOp) to LAST(PCodeOp) do
	EnterOpcode(pCodeOpNames[op], op);
    end;

end Input.
