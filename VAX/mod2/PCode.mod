implementation module PCode;

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
    Writef, Writec, Writes;

from Tokens import
    Token;

from Strings import
    String, WriteString;

from Symbols import
    MemoryType, LabelNumber, TypeNode, SetValue, DataType, cardIntTypeNode;

from TypeInfo import
    BaseType, NumberOf;

from PCodeOps import
    PCodeOp, pCodeOpNames;

from GenCode import
    codeFile;

var
    typeChar : array DataType of char;
    labelNumber : LabelNumber;
    memTypeChar : array MemoryType of char;

procedure InitPcode;
var
    t : Token;
begin

    typeChar[DTPOINTER]     := 'a';
    typeChar[DTRECORD]      := 'q';
    typeChar[DTARRAY]       := 's';
    typeChar[DTDYNARRAY]    := 'a';
    typeChar[DTINTEGER]     := 'i';
    typeChar[DTBOOLEAN]     := 'b';
    typeChar[DTCHAR]	    := 'c';
    typeChar[DTRENAME]      := '?';
    typeChar[DTOPAQUE]      := '?';
    typeChar[DTSTRING]      := 's';
    typeChar[DTREAL]	    := 'r';
    typeChar[DTLONGREAL]    := 'R';
    typeChar[DTSET]	    := 'S';
    typeChar[DTCARDINAL]    := 'j';
    typeChar[DTSUBRANGE]    := '?';
    typeChar[DTENUMERATION] := 'j';
    typeChar[DTPROC]	    := 'p';
    typeChar[DTWORD]	    := 'i';
    typeChar[DTBYTE]	    := 'c';
    typeChar[DTANY]	    := '?';

    memTypeChar[MEMGLOBAL]  := 'c';
    memTypeChar[MEMNORMAL]  := 'm';
    memTypeChar[MEMFAST]    := 't';
    memTypeChar[MEMPARAM]   := 'p';

    for t := first(Token) to last(Token) do
	operPcode[t] := PCZZZ;
    end;

    operPcode[TKPLUS]       :=	PCADD;
    operPcode[TKMINUS]      :=	PCSUB;
    operPcode[TKASTERISK]   :=	PCMUP;
    operPcode[TKSLASH]      :=	PCDIV;
    operPcode[TKAMPERSAND]  :=	PCAND;
    operPcode[TKEQUALS]     :=	PCEQU;
    operPcode[TKSHARP]      :=	PCNEQ;
    operPcode[TKLESS]       :=	PCLES;
    operPcode[TKGREATER]    :=	PCGRT;
    operPcode[TKNOTEQUAL]   :=	PCNEQ;
    operPcode[TKLSEQUAL]    :=	PCLEQ;
    operPcode[TKGREQUAL]    :=	PCGEQ;
    operPcode[TKAND]	    :=	PCAND;
    operPcode[TKDIV]	    :=	PCDIV;
    operPcode[TKIN]	    :=	PCINN;
    operPcode[TKMOD]	    :=	PCMOD;
    operPcode[TKNOT]	    :=	PCNOT;
    operPcode[TKOR]	    :=	PCIOR;

    labelNumber := 10000;
end InitPcode;

procedure @inline NewLabel (): LabelNumber;
begin
    labelNumber := labelNumber + 1;
    return labelNumber;
end NewLabel;

procedure @inline Lab(const l : LabelNumber);
begin
    Writef(codeFile, 'l%d',l);
end Lab;

procedure @inline GenOp(const op : PCodeOp);
begin
    Writef(codeFile, '\t%s\t', pCodeOpNames[op]);
end GenOp;

procedure GenT(const tn : TypeNode);
var
    bt : TypeNode;
begin
    bt := BaseType(tn);
    if bt = nil then
	Writec(codeFile, 'P');
    elsif bt = cardIntTypeNode then
	Writec(codeFile, 'k');
    else
	Writec(codeFile, typeChar[bt^.kind]);
    end;
end GenT;

procedure @inline GenMt(const mt : MemoryType);
begin
    Writec(codeFile, memTypeChar[mt]);
end GenMt;

procedure @inline I(const v:integer);
begin
if (v >= 0) and (v < 10) then
    Writec(codeFile, chr(ord('0') + v));
else
    Writef(codeFile, '%d', v);
end;
end I;

procedure @inline GenOpL(const op : PCodeOp);
begin
    Writef(codeFile, '\t%s\t\n', pCodeOpNames[op]);
end GenOpL;

procedure @inline GenOpTL(const op : PCodeOp; const tn : TypeNode);
begin
    Writef(codeFile, '\t%s\t', pCodeOpNames[op]);
    GenT(tn);
    Writec(codeFile, '\n');
end GenOpTL;

procedure @inline GenOpT(const op : PCodeOp; const tn : TypeNode);
begin
    Writef(codeFile, '\t%s\t', pCodeOpNames[op]);
    GenT(tn);
end GenOpT;

procedure @inline C(const c : char);
begin
    Writec(codeFile, c);
end C;

procedure @inline GenReal(const r : longreal);
begin
$IF vms THEN
    Writef(codeFile, '%1.17G', r);
$ELSE
    Writef(codeFile, '%1.17#G', r);
$END
end GenReal;

procedure @inline GenString(const s : String);
begin
    WriteString(codeFile, s);
end GenString;

procedure @inline W(const s : array of char);
begin
    Writes(codeFile, s);
end W;

procedure GenSet(const s : SetValue);
var
    i, last : integer;
    setSize : integer;
    tn : TypeNode;
begin
    tn := BaseType(s^.setType);
    setSize := trunc(NumberOf(tn^.setRange));
    I(setSize);
    X;
    last := setSize - 1;
    while (last >= 0) and not (last in s^.value) do
	last := last - 1;
    end;
    I(last+1);
    if last >= 0 then
	X;
	for i := 0 to last do
	    if i in s^.value then
		C('1');
	    else
		C('0');
	    end;
	end;
    end;
end GenSet;

procedure @inline X;
begin
    Writec(codeFile,',');
end X;

procedure @inline EndLine;
begin
    Writec(codeFile, '\n');
end EndLine;

end PCode.
