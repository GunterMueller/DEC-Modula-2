implementation module OptBuiltin;

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


from Symbols import
    ExprNode, ExprList, EvalMode, ProcNode, BuiltinProcType, addressTypeNode,
    OptTime;

from OCount import
    CountExpr;

from Optim import
    optTime, MarkOptExpr;

from OTree import
    OptExpr, Latest;

$if modula2 then
from Optim import MarkOptAll;
$else
from Symbols import DataType;
$end

procedure OptBuiltin(const parent : ExprNode; 
		     const proc   : ProcNode; 
		     const params : ExprList) : OptTime;

    procedure OptRestOfParams(p : ExprNode; const mode : EvalMode) : OptTime;
	var time : OptTime;
    begin
	time := 0;
	while p # nil do
	    time := Latest(time, OptExpr(p,parent));
	    if mode = EVALPUT then
		MarkOptExpr(p);
	    end;
	    p := p^.next;
	end;
	return time;
    end OptRestOfParams;

var
    time : OptTime;
    p, p1, p2, p3, p4 : ExprNode;
    nump : integer;

begin (* OptBuiltin *)
    p1 := nil;
    p2 := nil;
    nump := 0;
    if (params # nil) then
	p1 := params^.first;
        p := p1;
	while p # nil do
	    nump := nump + 1;
	    p := p^.next;
	end;
	if nump > 1 then
	    p2 := p1^.next;
	end;
    end;

    case proc^.builtin of
$if modula2 then
    | BIPCPUTIME : (* no parameters *)
	time := -optTime;   (* function with side-effects *)
$end
    
    | BIPABS, BIPCHR, BIPODD, BIPORD, BIPTRUNC, BIPXFC
$if modula2 then
      , BIPCAP, BIPFLOAT, BIPLONGFLOAT, BIPBITNOT, BIPHALT
$end
		: (* all one value parameter *)
	time := OptExpr(p1,parent);
    
    | BIPASSERT, BIPMIN :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));

$if modula2 then
    | BIPMAX, BIPBITAND, BIPBITOR, BIPBITXOR,
      BIPBITSHIFTLEFT, BIPBITSHIFTRIGHT : (* all two value parameters *)
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
    
    | BIPBITEXTRACT : (* three value parameters *)
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3,parent));
    
    | BIPBITINSERT : (* three value parameters *)
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3,parent));
	p4 := p3^.next;
	time := Latest(time, OptExpr(p4,parent));
    
    | BIPDEC, BIPINC, BIPINCL, BIPEXCL :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3,parent));
	MarkOptExpr(p1);
$end
    
    | BIPDISPOSE, BIPNEW
$if modula2 then
      , BIPLOCAL
$end
	    :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	time := Latest(time, OptRestOfParams(p2^.next, EVALGET));
	MarkOptExpr(p1);
    
$if modula2 then
    | BIPALLOCATE, BIPDEALLOCATE :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	MarkOptExpr(p1);
    
    | BIPVAL :
	time := OptExpr(p1,parent);
	time := OptExpr(p2,parent);
    
    | BIPADR :
	time := OptExpr(p1,parent);
    
    | BIPOPENF :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	time := -optTime;   (* function with side-effects *)
    
    | BIPCLOSEF :
	time := OptExpr(p1,parent);
    
    | BIPWRITEF :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	time := Latest(time, OptRestOfParams(p2^.next, EVALGET));
    
    | BIPREADF :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	time := Latest(time, OptRestOfParams(p2^.next, EVALPUT));
	time := -optTime;   (* function with side-effects *)
    
    | BIPSWRITEF :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	time := Latest(time, OptRestOfParams(p2^.next, EVALGET));
	MarkOptExpr(p1);
    
    | BIPSREADF :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	time := Latest(time, OptRestOfParams(p2^.next, EVALPUT));
	time := -optTime;   (* function with side-effects *)
    
    | BIPWRITES :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));

    | BIPWRITEC :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
    
    | BIPREADC :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	MarkOptExpr(p2);
	time := -optTime;   (* function with side-effects *)
    
    | BIPWRITEB :
	p3 := p2^.next;
	time := OptExpr(p2,parent);
	time := Latest(time, OptExpr(p3,parent));
	time := Latest(time, OptExpr(p1,parent));
    
    | BIPREADB, BIPREADS :
	p3 := p2^.next;
	time := OptExpr(p2, parent);
	time := Latest(time, OptExpr(p3,parent));
	time := Latest(time, OptExpr(p1,parent));
	MarkOptExpr(p2);
	time := -optTime;   (* function with side-effects *)
    
    | BIPNEWPROCESS :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3,parent));
	p4 := p3^.next;
	time := Latest(time, OptExpr(p4,parent));
	MarkOptExpr(p4);
    
    | BIPTRANSFER :
	time := OptExpr(p1,parent);
	time := Latest(time, OptExpr(p2,parent));
	MarkOptAll;
	
$else

    | BIPwriteln :
	time := OptExpr(p1, parent);

    | BIPwritec :
	time := OptExpr(p1, parent);
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3, parent));
	time := Latest(time, OptExpr(p3^.next, parent));

    | BIPfputc :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));

    | BIPwritef :
	time := OptExpr(p1, parent);
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3, parent));
	p4 := p3^.next;
	time := Latest(time, OptExpr(p4, parent));
	time := Latest(time, OptRestOfParams(p4^.next, EVALGET));

    | BIPfprintf :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));
	time := Latest(time, OptRestOfParams(p2^.next, EVALGET));

    | BIPwrites :
	time := OptExpr(p1, parent);
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3, parent));
	time := Latest(time, OptRestOfParams(p3^.next, EVALGET));

    | BIPfwrite :
	time := OptExpr(p1, parent);
	time := Latest(time, OptRestOfParams(p2, EVALGET));

    | BIPmax :
	time := OptRestOfParams(p1, EVALGET);

    | BIPnam :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));

    | BIPput, BIPget, BIPunit, BIPfnil :
	time := OptExpr(p1, parent);

    | BIPread4, BIPread8, BIPreadc, BIPreadln :
	time := OptExpr(p1, parent);
	time := -optTime;   (* function with side-effects *)

    | BIPreade :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2^.next, parent));
	time := -optTime;   (* function with side-effects *)

    | BIPpage :
	time := OptExpr(p1, parent);

    | BIPflush :
	if p1 = nil then
	    time := 0;
	else
	    time := OptExpr(p1, parent);
	end;

    | BIPeof, BIPeoln :
	time := OptExpr(p1, parent);
	time := - optTime;  (* Function with side effect *)
	
    | BIPlinelimit, BIPremove :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));

    | BIPstlimit :
	time := OptExpr(p1, parent);

    | BIPreset, BIPrewrite :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));
 	p3 := p2^.next;
	time := Latest(time, OptExpr(p3, parent));
	time := Latest(time, OptExpr(p3^.next, parent));
	MarkOptExpr(p1);

    | BIPpack :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3, parent));
	time := Latest(time, OptRestOfParams(p3^.next, EVALGET));
	MarkOptExpr(p3);

    | BIPunpack:
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));
	p3 := p2^.next;
	time := Latest(time, OptExpr(p3, parent));
	time := Latest(time, OptRestOfParams(p3^.next, EVALGET));
	MarkOptExpr(p2);

    | BIPargc :
	time := OptExpr(p1, parent);

    | BIPargv :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));
	time := Latest(time, OptExpr(p2^.next, parent));
	MarkOptExpr(p2);

    | BIPcard :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));

    | BIPseed :
	time := OptExpr(p1, parent);
	time := Latest(time, OptExpr(p2, parent));
	time := Latest(time, OptExpr(p2^.next, parent));
	MarkOptExpr(p2);
	time := - optTime; (* function with side-effect *)

    | BIPsqr, BIPsqrt, BIPsin, BIPcos, BIPexp, BIPln, BIParctan, BIPround,
      BIPexpo, BIPsucc, BIPpred, BIPundefined, BIPlongfloat :
	time := OptExpr(p1, parent);

    | BIPrandom :
	time := OptExpr(p1, parent);
	time := - optTime;  (* function with side-effect *)

    | BIPdate, BIPtime :
	time := OptExpr(p1, parent);
	MarkOptExpr(p1);

    | BIPclock, BIPsysclock, BIPwallclock :
	time := - optTime;  (* function with side-effect *)

    | BIPhalt, BIPnull :
	time := 0;

$end

    end (* case *);
    return time;
end OptBuiltin;


procedure CountBuiltin(const proc : ProcNode; const params : ExprList);

    procedure CountRestOfParams(p : ExprNode; const mode : EvalMode);
    begin
	while p # nil do
	    if (mode = EVALGET) and (p^.exprType # addressTypeNode) then
		CountExpr(p,EVALGET);
	    else
		CountExpr(p,EVALPOINT);
	    end;
	    p := p^.next;
	end;
    end CountRestOfParams;


var
    p, p1, p2, p3, p4 : ExprNode;

begin (* CountBuiltin *)
    p1 := nil;
    p2 := nil;
    if (params # nil) then
	p1 := params^.first;
	if p1 # nil then
	    p2 := p1^.next;
	end;
    end;

    case proc^.builtin of
    | BIPABS, BIPCHR, BIPODD, BIPORD, BIPTRUNC, BIPXFC
$if modula2 then
      , BIPCAP, BIPFLOAT, BIPLONGFLOAT, BIPBITNOT, BIPHALT
$end
	    :
	CountExpr(p1,EVALGET);
    
    | BIPASSERT, BIPMIN :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALGET);
    
$if modula2 then
    | BIPMAX, BIPBITAND, BIPBITOR, BIPBITXOR,
      BIPBITSHIFTLEFT, BIPBITSHIFTRIGHT :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALGET);
    
    | BIPBITEXTRACT :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALGET);
	p3 := p2^.next;
	CountExpr(p3,EVALGET);
    
    | BIPBITINSERT :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALGET);
	p3 := p2^.next;
	CountExpr(p3,EVALGET);
	p4 := p3^.next;
	CountExpr(p4,EVALGET);
    
    | BIPDEC, BIPINC, BIPEXCL, BIPINCL :
	CountExpr(p1,EVALPOINT);
	CountExpr(p2,EVALGET);
	p3 := p2^.next;
	CountExpr(p3,EVALGET);
$end
    
    | BIPNEW, BIPDISPOSE
$if modula2 then
      , BIPLOCAL
$end
	    :
	CountExpr(p1,EVALPOINT);
	CountExpr(p2,EVALGET);
	CountRestOfParams(p2^.next, EVALGET);
    
$if modula2 then
    | BIPALLOCATE, BIPDEALLOCATE :
	CountExpr(p1,EVALPOINT);
	CountExpr(p2,EVALGET);
    
    | BIPVAL :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALGET);
    
    | BIPADR :
	CountExpr(p1,EVALPOINT);
    
    | BIPCPUTIME :
	(* nothing *)

    | BIPWRITEF :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALPOINT);
	CountRestOfParams(p2^.next, EVALGET);
    
    | BIPSWRITEF :
	CountExpr(p1,EVALPOINT);
	CountExpr(p2,EVALPOINT);
	CountRestOfParams(p2^.next, EVALGET);
    
    | BIPREADF :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALPOINT);
	CountRestOfParams(p2^.next, EVALPUT);
    
    | BIPSREADF :
	CountExpr(p1,EVALPOINT);
	CountExpr(p2,EVALPOINT);
	CountRestOfParams(p2^.next, EVALPUT);
    
    | BIPWRITES :
	CountExpr(p1, EVALGET);
	CountExpr(p2, EVALPOINT);

    | BIPWRITEC :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALGET);
    
    | BIPREADC :
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALPUT);
    
    | BIPWRITEB, BIPREADB, BIPREADS :
	CountExpr(p2,EVALPOINT);
	p3 := p2^.next;
	CountExpr(p3,EVALGET);
	CountExpr(p1,EVALGET);
    
    | BIPOPENF :
	CountExpr(p1,EVALPOINT);
	CountExpr(p2,EVALPOINT);
    
    | BIPCLOSEF :
	CountExpr(p1,EVALGET);

    | BIPNEWPROCESS :
	p3 := p2^.next;
	p4 := p3^.next;
	CountExpr(p1,EVALGET);
	CountExpr(p2,EVALGET);
	CountExpr(p3,EVALGET);
	CountExpr(p4,EVALPUT);

    | BIPTRANSFER :
	CountExpr(p1,EVALPUT);
	CountExpr(p2,EVALPUT);

$else (* pascal *)

    | BIPwriteln :
	CountExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));

    | BIPwritec :
	CountExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	p3 := p2^.next;
	CountExpr(p3, EVALGET);
	CountExpr(p3^.next, EVALGET);

    | BIPfputc :
	CountExpr(p1, EVALGET);
	CountExpr(p2, EVALGET);

    | BIPwritef :
	CountExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	p3 := p2^.next;
	CountExpr(p3, EVALGET);
	p4 := p3^.next;
	CountExpr(p4, EVALPOINT);
	CountRestOfParams(p4^.next, EVALGET);

    | BIPfprintf :
	CountExpr(p1, EVALGET);
	CountExpr(p2, EVALPOINT);
	CountRestOfParams(p2^.next, EVALGET);

    | BIPwrites :
	CountExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	p3 := p2^.next;
	CountExpr(p3, EVALPOINT);
	CountRestOfParams(p3^.next, EVALGET);

    | BIPfwrite :
	CountExpr(p1, EVALPOINT);
	CountRestOfParams(p2, EVALGET);

    | BIPmax :
	CountRestOfParams(p1, EVALGET);

    | BIPnam :
	CountExpr(p1, EVALGET);
	CountExpr(p2, EVALPOINT);

    | BIPfnil, BIPput, BIPget, BIPunit,
      BIPread4, BIPread8, BIPreadc, BIPreadln :
	CountExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	
    | BIPreade :
	CountExpr(p1, VAL(EvalMode, trunc(p2^.exprConst^.cardVal)));
	CountExpr(p2^.next, EVALPOINT);

    | BIPpage, BIPeof, BIPeoln :
	CountExpr(p1, EVALPOINT);
	
    | BIPflush :
	if p1 # nil then
	    CountExpr(p1, EVALPOINT);
	end;

    | BIPlinelimit, BIPremove :
	CountExpr(p1, EVALPOINT);
	CountExpr(p2, EVALGET);

    | BIPstlimit :
	CountExpr(p1, EVALGET);

    | BIPreset, BIPrewrite:
	CountExpr(p1, EVALPOINT);
	CountExpr(p2, EVALPOINT);
	p3 := p2^.next;
	CountExpr(p3, EVALGET);
	CountExpr(p3^.next, EVALGET);

    | BIPpack, BIPunpack :
	CountExpr(p1, EVALGET);
	CountExpr(p2, EVALPOINT);
	p3 := p2^.next;
	CountExpr(p3, EVALPOINT);
	CountRestOfParams(p3^.next, EVALGET);   (* p4..p7 *)
	
    | BIPargc :
	CountExpr(p1, EVALGET);

    | BIPargv :
	CountExpr(p1, EVALGET);
	CountExpr(p2, EVALPUT);
	CountExpr(p2^.next, EVALGET);

    | BIPcard :
	CountExpr(p1, EVALPOINT);
	CountExpr(p2, EVALGET);

    | BIPseed :
	CountExpr(p1, EVALGET);
	CountExpr(p2, EVALPUT);
	CountExpr(p2^.next, EVALGET);

    | BIPsqr, BIPsqrt, BIPsin, BIPcos, BIPexp, BIPln, BIParctan, BIPlongfloat, 
      BIPround, BIPexpo, BIPsucc, BIPpred, BIPundefined, BIPrandom :
	CountExpr(p1, EVALGET);

    | BIPdate, BIPtime :
	CountExpr(p1, EVALPUT);

    | BIPclock, BIPsysclock, BIPwallclock, BIPhalt, BIPnull :
	(* no paramters *)

$end

    end (* case *);
end CountBuiltin;

end OptBuiltin.
