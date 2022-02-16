implementation module Util;

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


from SYSTEM import MININT, MAXINT;

from Consts import ADDRSIZE, BOOLSIZE, REALSIZE, INTSIZE, CHARSIZE;

from Error import Error;

from Machine import atoi;

from Types import pcodetype;

procedure @inline Int(const opd : array @nocount of char) : integer;
begin
    return atoi(opd);
end Int;


procedure datasize(t:pcodetype;s:integer):integer;
begin
    case t of
    | taddress  : return ADDRSIZE
    | tboolean  : return BOOLSIZE 
    | tchar     : return CHARSIZE 
    | treal     : return REALSIZE
    | tlongreal : return 2*REALSIZE
    | tinteger,
      tcardinal : return INTSIZE       
    | tproc     : return ADDRSIZE       
    | tset, 
      tstring, 
      trecord   : return s        
    | tundefined: return s       
    end;
end datasize;


procedure power(a,b:integer):integer;
var value : integer;
begin
    if (b<0) then
	Error('power: exponent < 0');
    else
	value := 1;
	while b <> 0 do
	    if odd(b) then
		value := value * a;
	    end;
	    b := b div 2;
	    a := a * a;
	end;
    end;
    return value;
end power;            (* end of power *)


procedure PowerOfTwo(n : integer) : integer;
var
    value, p : integer;
begin
    if n <= 0 then
	value := MININT; (* = 2^31 *)
	p := 31;
    elsif maxint - n <= n then
	value := MAXINT div 2 + 1; (* = (maxint+1) div 2  = 2^30 *)
	p := 30;
    else
	p := 0;
	value := 1;
	while (value < n) do
	    value := value + value;
	    p := p + 1;
	end;
    end;
    if value = n then
	return p;
    else
	return -1;
    end;
end PowerOfTwo;
end Util.
