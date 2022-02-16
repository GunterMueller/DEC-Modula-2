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

implementation module strings;

procedure Compare(const a, op, b : array of Char) : Boolean;
type
    CharSet = set of Char;
    Comparison = (COMPEQ, COMPNE, COMPGT, COMPLT);
var
    i, length : Cardinal;
    comparison : Comparison;
    equalOK : boolean;
    lasta, lastb : Char;
begin
    (* encode comparison operator *)
    if number(op) >= 1 then
	if op[0] = '=' then
	    comparison := COMPEQ;
	    equalOK := TRUE;
	elsif op[0] = '#' then
	    comparison := COMPNE;
	    equalOK := FALSE;
	elsif op[0] = '<' then
	    if number(op) >= 2 then
		if op[1] = '>' then
		    comparison := COMPNE;
		    equalOK := FALSE;
		else
		    comparison := COMPLT;
		    equalOK := op[1] = '=';
		end;
	    else
		comparison := COMPLT;
		equalOK := FALSE;
	    end;
	elsif op[0] = '>' then
	    comparison := COMPGT;
	    equalOK := (number(op) >= 2) and (op[1] = '=');
	else
	    assert(false,"strings.Compare: bad comparison");
	end;
    else
	assert(false,"strings.Compare: bad comparison");
    end;
    (* figure out min of two lengths *)
    (* set lasta and lastb to be last characters of a and b if same length *)
    (* set to a null for the shorter one and the corresponding character for *)
    (* the longer one if the lengths differ (Note: chars are unsigned) *)
    length := Number(b);
    if length = Number(a) then
	if length = 0 then
	    return equalOK;
	end;
	lasta := a[length-1];
	lastb := b[length-1];
    elsif length < Number(a) then
	lasta := a[length];
	lastb := 0C;
    else
	length := Number(a);
	lasta := 0C;
	lastb := b[length];
    end;

    i := 0;
    (* scan through the strings, comparing for equal *)
    loop
	(* stop if reached the end *)
	if i >= length then
	    exit;
	end;
	(* stop at first non-equal character *)
	if a[i] # b[i] then
	    (* compare these characters instead of planned ones *)
	    lasta := a[i];
	    lastb := b[i];
	    exit;
	end;
	(* stop if both null (above if assured a[i] = b[i]) *)
	if a[i] = 0C then
	    (* ensure later code thinks strings are equal *)
	    lasta := 0C;
	    lastb := 0C;
	    exit;
	end;
	i := i + 1;
    end;
    (* do comparison requested *)
    case comparison of
	COMPEQ :
	    return lasta = lastb;
	|
	COMPNE :
	    return lasta # lastb;
	|
	COMPGT:
	    return (lasta > lastb) or (equalOK and (lasta = lastb));
	|
	COMPLT:
	    return (lasta < lastb) or (equalOK and (lasta = lastb));
	|
    end;
end Compare;

procedure Assign(var toString : array of Char; const fromString : array of Char);
var
    i : cardinal;
begin
    i := 0;
    (* loop assigning characters *)
    loop
	(* stop at null or end of fromString *)
	if (i+1 > Number(fromString)) or (fromString[i] = 0C) then
	    exit;
	end;
	toString[i] := fromString[i];
	i := i + 1;
    end;
    (* if there is room in toString, insert null *)
    if i < Number(toString) then
	toString[i] := 0C;
    end;
end Assign;

procedure Append(var toString : array of Char; const fromString : array of Char);
var
    fi, ti : cardinal;
begin
    ti := 0;
    fi := 0;
    (* find end of toString *)
    while toString[ti] # 0C do
	ti := ti + 1;
    end;
    loop
	if (fi+1 > Number(fromString)) or (fromString[fi] = 0C) then
	    exit;
	end;
	toString[ti] := fromString[fi];
	fi := fi + 1;
	ti := ti + 1;
    end;
    if ti < Number(toString) then
	toString[ti] := 0C;
    end;
end Append;

procedure Length(const string : array of Char) : cardinal;
var
    length : cardinal;
begin
    for length := 0 to number(string)-1 do
	if string[length] = 0C then
	    return(length);
	end;
    end;
    return number(string);
end Length;

end strings.
