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

implementation module math;
import longmath;

procedure sin(x : real) : real;
begin
    return float(longmath.sin(longfloat(x)));
end sin;

procedure cos(x : real) : real;
begin
    return float(longmath.cos(longfloat(x)));
end cos;

procedure atan(x : real) : real;
begin
    return float(longmath.atan(longfloat(x)));
end atan;

procedure atan2(x, y : real) : real;
begin
    return float(longmath.atan2(longfloat(x),longfloat(y)));
end atan2;

procedure exp(x : real) : real;
begin
    return float(longmath.exp(longfloat(x)));
end exp;

procedure sqrt(x : real) : real;
begin
    return float(longmath.sqrt(longfloat(x)));
end sqrt;

procedure log(x : real) : real;
begin
    return float(longmath.log(longfloat(x)));
end log;

procedure ldexp(x : real; exp : integer) : real;
begin
    return float(longmath.ldexp(longfloat(x),exp));
end ldexp;

procedure longsin(x : longreal) : longreal;
begin
    return longmath.sin(x);
end longsin;

procedure longcos(x : longreal) : longreal;
begin
    return longmath.cos(x);
end longcos;

procedure longatan(x : longreal) : longreal;
begin
    return longmath.atan(x);
end longatan;

procedure longatan2(x, y : longreal) : longreal;
begin
    return longmath.atan2(x,y);
end longatan2;

procedure longexp(x : longreal) : longreal;
begin
    return longmath.exp(x);
end longexp;

procedure longsqrt(x : longreal) : longreal;
begin
    return longmath.sqrt(x);
end longsqrt;

procedure longlog(x : longreal) : longreal;
begin
    return longmath.log(x);
end longlog;

procedure longldexp(x : longreal; exp : integer) : longreal;
begin
    return longmath.ldexp(x,exp);
end longldexp;

end math.
