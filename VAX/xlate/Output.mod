implementation module Output;

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


from io import SWritef, Writef, Writec, Writes;

from ASCII import TAB, CharSet;

from EES import Reg, RegSet, regString, TRegReg;

from Error import Error;

from Machine import 
    outputfile;

from Consts import
    BYTESPERWORD;

from Types import 
    opcodefieldsize, operandstring, LabelNumber;

from Vars import
    pclabel, pclbsize, opcodestr, opd, opdcount, opdsizes,
    numcomblocks, comtable, line, curblockid;

procedure @inline C(const c : char);
begin
    Writec(outputfile, c);
end C;

(* To allow comments in the assembler (MACRO32) file on VMS - CED 9/29/87 *)
procedure @inline Comment;
begin
$IF vms THEN
    C(';');
$ELSE
    C('#');
$END;
end Comment;

procedure @inline X; 
begin
    Writec(outputfile, ',');
end X;

procedure @inline L; 
begin
    Writec(outputfile, '\n');
end L;

procedure @inline S(const s : array @nocount of char);
begin
    Writes(outputfile, s);
end S;

procedure @inline Op(const v : array @nocount of char);
begin
    Writec(outputfile, '\t');
    Writes(outputfile, v);
    Writec(outputfile, '\t');
end Op;

procedure @inline I(const i : integer);
begin
    if (i >= 0) and (i < 10) then
	Writec(outputfile, chr(ord('0') + i));
    else
	Writef(outputfile, '%d', i);
    end;
end I;

procedure R (const v : Reg);
begin
    if v > SAVEDREG then
	if v > SAVEDDREG then
	    C('d'); I(curblockid); C('+'); I((ord(v)-ord(SD0))*BYTESPERWORD);
	else
	    C('r'); I(curblockid); C('+'); I((ord(v)-ord(SR0))*BYTESPERWORD);
	end;
	S('(fp)');
    elsif v in RegSet{rt0..rt9} then
	S(regString[TRegReg(v)]);
    else
	S(regString[v]);
    end;
end R;

procedure @inline Lab(const ln : LabelNumber);
begin
    Writef(outputfile, 'L%d', ln);
end Lab;

procedure @inline LabelC(const ln : LabelNumber);
begin
(* |||     Writef(outputfile, '\t.align\t2,1\nL%d:', ln);*)
    Writef(outputfile, 'L%d:', ln);
end LabelC;

procedure @inline LabelCE(const ln : LabelNumber);
begin
    Writef(outputfile, 'L%d:', ln);
end LabelCE;

procedure @inline JumpToLabel(const jumpName : array of char;
			      const ln : LabelNumber);
begin
    Writef(outputfile, '\t%s\tL%d\n', jumpName, ln);
end JumpToLabel;

procedure @inline LitI(const i : integer);
begin
    Lit; I(i);
end LitI;

procedure @inline Bssz;
begin
$if VMS then
    Op('.blkb');
$else
    Op('.space');
$end
end Bssz;

procedure @inline TextSeg;
begin
$if VMS then
    Op('.psect'); S('$code'); L;
$else
    Op('.text'); L;
$end;
end TextSeg;

procedure @inline DataSeg;
begin
$if VMS then
    Op('.psect'); S('data'); L;
$else
    Op('.data'); L;
$end
end DataSeg;

procedure @inline Lit;
begin
$if VMS then
    C('#');
$else
    C('$');
$end
end Lit;

procedure @inline Indirect;
begin
$if VMS then
    C('@');
$else
    C('*');
$end
end Indirect;

procedure @inline GN;
begin
$if UNIX then
    C('_');
$end
end GN;

procedure SOEscape(var s:operandstring; const len:integer);
var i : integer;
begin
$if VMS then
    for i := 1 to len do
	if s[i] = '"' then
	    S('"/"/"');
	elsif not (s[i] in CharSet{' '..'~'}) then
	    S('"<');
	    I(ord(s[i]));
	    S('>"');
	else
	    C(s[i]);
	end;
    end;
$else (* UNIX *)
    for i := 1 to len do
	if (s[i] in CharSet{'\\','"'}) then
	    C('\\'); C(s[i]);
	elsif not (s[i] in CharSet{' '..'~'}) then
	    C('\\');
	    I((ord(s[i]) div 64) mod 8);
	    I((ord(s[i]) div 8) mod 8);
	    I(ord(s[i]) mod 8);
	else
	    C(s[i]);
	end;
    end;
$end
end SOEscape;

procedure error(const n:integer);
var     numberString : array [0..9] of char;
begin
    SWritef(numberString, '%d', n);
    Error(numberString);
end error;    (*end of error*)


procedure echopcodeline;
var i, j : integer;
begin
    Comment; C(' '); S('line'); C(' ');
    I(line); C(':');
    if pclbsize > 0 then S(pclabel);end;
    C(TAB);
    for i := 1 to opcodefieldsize do
	C(opcodestr[i]);
    end;
    C(TAB);
    for i:=1 to opdcount do
	for j:=1 to opdsizes^[i] do
	    if (opd^[i][j] >= ' ') then
		C(opd^[i][j]);
	    end;
	end;
	if (i<opdcount) then C(',');end;
    end;
    L;
end echopcodeline;


procedure WriteName (const n : integer );
var
  i  :  integer;
  index  :  integer;
  found  :  boolean;
begin
    i := 0;
    found := false;
    while (i < numcomblocks) and not found do
	if comtable[i].block = n then
	   index := i;
	   found := true;
	end;
	i := i + 1;
    end;
    if not found then
	Error('WriteName: name not found');
    else
	GN;
	i := 1;
	while comtable[index].name[i] <> 0C do
	    C(comtable[index].name[i]);
	    i := i + 1;
	end;
    end;
end WriteName;

procedure writelabel(const k:integer);
var     i : integer;
begin
    C('L');
    for i:=2 to opdsizes^[k] do
	C(opd^[k][i]);
    end;
end writelabel;

end Output.
