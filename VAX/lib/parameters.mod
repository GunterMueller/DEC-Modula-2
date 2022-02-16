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

IMPLEMENTATION MODULE parameters;
FROM SYSTEM IMPORT MAXINT;
TYPE
    ArgPtr = POINTER @NOCHECK TO ARRAY [0..MAXINT] OF CHAR;
VAR
    (* these variables  are set by runtime initialization *)
    argc : CARDINAL;
    argv, envp : POINTER @NOCHECK TO ARRAY [0..MAXINT] OF ArgPtr;

PROCEDURE GetParameter(num : CARDINAL; VAR value : ARRAY OF CHAR;
	VAR length: INTEGER);
VAR
    i : CARDINAL;
BEGIN
    IF num >= NumParameters THEN
	length := -1;	(* error, no such parameter *)
    ELSE
	i := 0;
	WHILE (i < NUMBER(value)) AND (argv^[num]^[i] # 0C) DO
	    value[i] := argv^[num]^[i];
	    i := i + 1;
	END;
	IF i < NUMBER(value) THEN
	    value[i] := 0C;
	END;
	length := i;
    END;
END GetParameter;

PROCEDURE GetEnvironment(const name : ARRAY OF CHAR; VAR value : ARRAY OF CHAR;
	VAR length: INTEGER);
VAR
    v, i, l : CARDINAL;
BEGIN
    v := 0;
    LOOP
	IF envp^[v] = NIL THEN
	    length := -1;
	    RETURN;
	END;
	i := 0;
	WHILE (i < NUMBER(name)) AND (name[i] = envp^[v]^[i]) DO
	    i := i + 1;
	END;
	IF (envp^[v]^[i] = '=') AND
		((i >= NUMBER(name)) OR (name[i] = 0C))
	THEN
	    EXIT;
	END;
	v := v + 1;
    END;
    i := i + 1;
    l := 0;
    WHILE (l < NUMBER(value)) AND (envp^[v]^[i+l] # 0C) DO
	value[l] := envp^[v]^[i+l];
	l := l + 1;
    END;
    IF l < NUMBER(value) THEN
	value[l] := 0C;
    END;
    length := l;
END GetEnvironment;

BEGIN
    NumParameters := argc;
END parameters.
