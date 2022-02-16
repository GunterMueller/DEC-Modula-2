implementation module Errors;

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

from Strings import
    String, WriteString, AddChar, NonHashString, NonHashText;

from Scanner import
    LineWrite;

from Symbols import
    currFile, currLine, ConstNode, DataType;

type
    ErrorLevel = (WARNING, ERROR);
    NumberType = (NONE, SIGNEDNUMBER, UNSIGNEDNUMBER);

procedure GeneralError(const fileName    : String;
		       const lineNumber  : integer;
		       const name        : String; 
		       const msg	 : array of char; 
		       const number      : integer;
		             printNumber : NumberType;
		       const errorLevel  : ErrorLevel);
var
    j : integer;
    printName: boolean;

begin
    Writef(output, 'File ');
    if fileName = nil then
	Writef(output, 'input');
    else
	WriteString(output, fileName);
    end;
    if lineNumber = -1 then
	Writef(output, ', line (end of file): ');
    else
	Writef(output, ', line %d: ', lineNumber);
    end;
    if errorLevel = WARNING then
	Writef(output, '(Warning) ');
    end;

    j := 0;
    printName := name # nil;
    while (j <= high(msg)) and (msg[j] # 0C) do
	if (msg[j] = '$') then
	    if printName then
		WriteString(output, name);
		printName := false;
	    end;
	elsif (msg[j] = '%') and (printNumber # NONE) then
	    if printNumber = SIGNEDNUMBER then
		Writef(output, '%d', number);
	    else
		Writef(output, '%u', unsigned(number));
	    end;
	    printNumber := NONE;
	else
	    Writec(output, msg[j]);
	end;
	j := j + 1;
    end;

    if printName then
	Writef(output, ': ');
	WriteString(output, name);
    end;
    if printNumber = SIGNEDNUMBER then
	Writef(output, '%d', number);
    elsif printNumber = UNSIGNEDNUMBER then
	Writef(output, '%u', unsigned(number));
    end;
    Writec(output, '\n');
    LineWrite();
    if errorLevel > WARNING then
	numberOfErrors := numberOfErrors + 1;
	totalErrors := totalErrors + 1;
    end;
end GeneralError;

procedure SilentError();
begin
    totalErrors := totalErrors + 1;
end SilentError;

procedure ErrorName(const name : String; const msg : array of char);
begin
    GeneralError(currFile, currLine, name, msg, 0, NONE, ERROR);
end ErrorName;

procedure ErrorNumber(const msg : array of char; const number : integer);
begin
    GeneralError(currFile, currLine, nil, msg, number, SIGNEDNUMBER, ERROR);
end ErrorNumber;

procedure ErrorNameNumber(const name   : String;
			  const msg    : array of char;
			  const number : integer);
begin
    GeneralError(currFile, currLine, name, msg, number, SIGNEDNUMBER, ERROR);
end ErrorNameNumber;

procedure ErrorUnsigned(const msg : array of char; const number : unsigned);
begin
    GeneralError(currFile, currLine, nil, msg, integer(number), UNSIGNEDNUMBER,
	ERROR);
end ErrorUnsigned;

procedure Error(const msg : array of char);
begin
    GeneralError(currFile, currLine, nil, msg, 0, NONE, ERROR);
end Error;

procedure Warning(const msg : array of char);
begin
    GeneralError(currFile, currLine, nil, msg, 0, NONE, WARNING);
end Warning;

procedure ExprWarning(const en : ExprNode; const msg : array of char);
begin
GeneralError(en^.fileName, en^.lineNumber, nil, msg, 0, NONE, WARNING);
end ExprWarning;

procedure ExprError(const en : ExprNode; const msg : array of char);
begin
    GeneralError(en^.fileName, en^.lineNumber, nil, msg, 0, NONE, ERROR);
end ExprError;

procedure ExprErrorName(const en   : ExprNode; 
			const name : String; 
			const msg  : array of char);
begin
    GeneralError(en^.fileName, en^.lineNumber, name, msg, 0, NONE, ERROR);
end ExprErrorName;

procedure ExprErrorNameNumber(const en     : ExprNode; 
			      const name   : String; 
			      const msg    : array of char;
			      const number : integer);
begin
    GeneralError(en^.fileName, en^.lineNumber, name, msg, number, SIGNEDNUMBER,
	ERROR);
end ExprErrorNameNumber;

procedure ExprErrorNumber(const en     : ExprNode; 
			  const msg    : array of char; 
			  const number : integer);
begin
    GeneralError(en^.fileName, en^.lineNumber, nil, msg, number, SIGNEDNUMBER,
	ERROR);
end ExprErrorNumber;

procedure ExprErrorConst(const en   : ExprNode;
			       msg  : array of char;
			 const cn   : ConstNode);
var
    i           : cardinal;
    name	: String;
    number      : integer;
    numberType  : NumberType;
begin
    name := nil;
    numberType := NONE;
    case cn^.kind of
    | DTCHAR   :
	AddChar('"');
	AddChar(cn^.charVal);
	AddChar('"');
	name := NonHashString();
    | DTINTEGER :
	number := trunc(cn^.cardVal);
	numberType := SIGNEDNUMBER;
    | DTCARDINAL :
	number := trunc(cn^.cardVal);
	numberType := UNSIGNEDNUMBER;
    | DTBOOLEAN :
	if cn^.boolVal then
	    name := NonHashText('"TRUE"')
	else
	    name := NonHashText('"FALSE"');
	end;
    | DTENUMERATION :
	name := cn^.enumVal^.name;
    | else
	assert(false, 'Error ConstNode not an ordinal type?');
    end;
    if numberType # NONE then
	(* Change $ to % *)
	i := 0;
	while (i <= high(msg)) and (msg[i] # 0C) do
	    if msg[i] = '$' then
		msg[i] := '%';
	    end;
	    inc(i);
	end;
    end;
    GeneralError(en^.fileName, en^.lineNumber, name, msg, number, numberType,
	ERROR);
end ExprErrorConst;
    
procedure StmtError(const stn : StmtNode; const msg : array of char);
begin
    GeneralError(stn^.fileName, stn^.lineNumber, nil, msg, 0, NONE, ERROR);
end StmtError;

procedure StmtWarning(const stn : StmtNode; const msg : array of char);
begin
    GeneralError(stn^.fileName, stn^.lineNumber, nil, msg, 0, NONE, WARNING);
end StmtWarning;

procedure StmtErrorName(const stn  : StmtNode; 
		        const name : String; 
			const msg  : array of char);
begin
    GeneralError(stn^.fileName, stn^.lineNumber, name, msg, 0, NONE, ERROR);
end StmtErrorName;

procedure ProcErrorName(const proc : ProcNode;
			const name : String;
			const msg  : array of char);
begin
    GeneralError(proc^.fileName, proc^.lineNumber, name, msg, 0, NONE, ERROR);
end ProcErrorName;

end Errors.
