implementation module EES;

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


from Consts import WORDSIZE, BYTESPERWORD;

from Error import Error;

from MemRef import FreeStack;

from Output import Op, S, R, GN, C, X, L, I, Comment;

from Types import pcodetype;

from Vars import curlev, regmask, rmemoff, dmemoff, curblockid;

type
    RegGroup = (RGSINGLE, RGPAIR1, RGPAIR2);
var
    typeString : array pcodetype of ShortString;
    boolString : array boolean of ShortString;
    groupString : array RegGroup of ShortString;
    kindString : array EESKind of ShortString;
    stateString : array RegState of ShortString;

type
    RegRecord = record
	suitable : RegStateSet;
	regGroup : RegGroup;
	maskbit : integer;
	eesElement : EESElement;  (* ||| hack - too many variant violations *)
	case state : RegState of
	| REGDISP : level : Level; param : boolean; inUse : boolean;
	end;
    end;

    IntRegRec = record 
	case boolean of
	| true : int : integer;
	| false : reg : Reg;
	end;
    end;

var
    regTable : array Reg of RegRecord;

procedure ActiveReg(r : Reg) : boolean;
begin
    return not (r in RegSet{NULLREG,rt0..rt9});
end ActiveReg;

procedure MemTReg(offset : integer) : Reg;
var
    ir : IntRegRec;
begin
    ir.int := ord(rt0) + offset div WORDSIZE;
    return ir.reg;
end MemTReg;

procedure TRegReg(r : Reg) : Reg;
var
    ir : IntRegRec;
begin
    ir.int := ord(firsttreg) + ord(r) - ord(rt0);
    return ir.reg;
end TRegReg;

procedure ClearAddress(e : EESElement);
begin
    with ees[e] do
	(*ees[e].*)addrMemType := ' ';
	(*ees[e].*)addrOffset := 0;
	(*ees[e].*)addrBlock := 0;
	(*ees[e].*)addrLevel := 0;
	(*ees[e].*)indirect := false;
    end;
end ClearAddress;

procedure ClearEES(e : EESElement);
begin
    with ees[e] do
	(*ees[e].*)kind := EESDATA;
	(*ees[e].*)dreg := NULLREG;
	(*ees[e].*)breg := NULLREG;
	(*ees[e].*)sreg := NULLREG;
	(*ees[e].*)sunits := 0;
	(*ees[e].*)constInt := 0;
	(*ees[e].*)ptype:=tundefined;
	(*ees[e].*)size:=0;
	ClearAddress(e);
	(*ees[e].*)smemoffset:=0;
	(*ees[e].*)smemsize:=0;
	(*ees[e].*)inUse:=false;
	(*ees[e].*)indirect:=false;
    end;
end ClearEES;

procedure Push(kind : EESKind);
begin
    if top >= EESSTACKSIZE then
	Error('Push: top > EESSTACKSIZE');
    else
	top:=top+1;
	ClearEES(top);
	ees[top].kind := kind;
	ees[top].inUse := true;
	ees[top].callNest := callNest;
    end;
end Push;

procedure Pop(num:integer);
var
    e : EESElement;
begin
    if (top<num) then
	Error('Pop: top < num');
    else
	for e := top to top-num+1 by -1 do
	    if ees[e].kind = EESDATA then
		FreeReg(ees[e].dreg);
	    elsif ees[e].kind in EESKindSet{EESADDR,EESVAR} then
		FreeReg(ees[e].sreg);
		FreeReg(ees[e].breg);
	    end;
	    if ees[e].smemsize <> 0 then
		FreeStack(ees[e].smemoffset,ees[e].smemsize);
	    end;
	    ClearEES(top);
	end;
	top := top - num;
    end;
end Pop;

procedure ClearReg;
const
$if vms then
    FIRSTVOLATILEREG = r2;
    FIRSTMASK = 2;
$else (* unix *)
    FIRSTVOLATILEREG = r6;
    FIRSTMASK = 32;
$end

var
    r, rr : Reg;
    mask : integer;
begin
    for r := FIRST(Reg) to LAST(Reg) do
	regTable[r].suitable := RegStateSet{};
	regTable[r].state := REGALLOC;
	regTable[r].regGroup := RGSINGLE;
(* ||| Replace all this arithmetic by set operations *)
	if r in RegSet{FIRSTVOLATILEREG..r11} then
	    mask := FIRSTMASK;
	    for rr := FIRSTVOLATILEREG to r do
		mask := mask + mask;
	    end;
	    regTable[r].maskbit := mask;
	else
	    regTable[r].maskbit := 0;
	end;
    end;
    regTable[RETURNREG].suitable := RegStateSet{REGRETURN};
    regTable[RETURNREG].state := REGFREE;
    for r := FIRSTREG to LASTREG do
	regTable[r].suitable := RegStateSet{REGTEMP,REGDISP,REGEES};
	regTable[r].state := REGFREE;
    end;
    regTable[SPREG].suitable := RegStateSet{REGSP};
    regTable[SPREG].state := REGFREE;
    for r := FIRSTVREG to LASTVREG do
	regTable[r].suitable := RegStateSet{REGEES};
	regTable[r].state := REGFREE;
    end;
    for r := SR0 to SR9 do
	regTable[r].suitable := RegStateSet{REGEES};
	regTable[r].state := REGFREE;
    end;
    for r := SD0 to SD9 do
	regTable[r].suitable := RegStateSet{REGEES};
	regTable[r].state := REGFREE;
    end;
end ClearReg;

procedure CheckFreeReg;
var
    r, rr : Reg;
    mask : integer;
begin
    for r := FIRSTREG to LASTREG do
	if regTable[r].state = REGFREE then
	elsif (regTable[r].state = REGDISP) and not regTable[r].inUse
	then
	else
	    Error('Register still in use');
	end;
    end;
    for r := FIRSTVREG to LASTVREG do
	if regTable[r].state = REGFREE then
	elsif (regTable[r].state = REGDISP) and not regTable[r].inUse
	then
	else
	    Error('Register still in use');
	end;
    end;
end CheckFreeReg;

procedure StealReg(stealr, replacer : Reg);
    var stealrp1, replacerp1 : Reg;
begin
    if regTable[stealr].state in RegStateSet{REGDISP,REGFREE} then
	(* don't need to steal it *)
    elsif regTable[stealr].regGroup <> RGSINGLE then
	replacerp1 := VAL(Reg, ord(replacer)+1);
	stealrp1 := VAL(Reg, ord(stealr)+1);
	if (regTable[stealr].regGroup <> RGPAIR1)
		or (regTable[stealrp1].regGroup <> RGPAIR2) then
	    Error('StealReg: error in reg pairing');
	end;
	ees[regTable[stealr].eesElement].dreg := replacer;
	(* steal reg pair, moving value to virtual reg *)
	Op('movq'); R(stealr); X; R(replacer); L;
	regTable[replacer].state := regTable[stealr].state;
	regTable[replacer].eesElement := regTable[stealr].eesElement;
	regTable[replacer].regGroup := regTable[stealr].regGroup;
	regTable[replacerp1].state := regTable[stealrp1].state;
	regTable[replacerp1].eesElement := regTable[stealrp1].eesElement;
	regTable[replacerp1].regGroup := regTable[stealrp1].regGroup;
	(* free second reg of stolen pair *)
	regTable[stealrp1].state := REGFREE;
	regTable[stealr].regGroup := RGSINGLE;
	regTable[stealrp1].regGroup := RGSINGLE;
    else
	if ees[regTable[stealr].eesElement].dreg = stealr then
	    ees[regTable[stealr].eesElement].dreg := replacer;
	elsif ees[regTable[stealr].eesElement].breg = stealr then
	    ees[regTable[stealr].eesElement].breg := replacer;
	elsif ees[regTable[stealr].eesElement].sreg = stealr then
	    ees[regTable[stealr].eesElement].sreg := replacer;
	else
	    Error('StealReg: could not find register usage');
	end;
	(* steal reg, moving value to virtual reg *)
	Op('movl'); R(stealr); X; R(replacer); L;
	regTable[replacer].state := regTable[stealr].state;
	regTable[replacer].eesElement := regTable[stealr].eesElement;
    end;
end StealReg;

(* AllocReg:  allocate a register. *)
(*  Make sure it can handle specified type.  Allocate two regs if necessary *)
(*  Assign it to specified stack element *)
(*  NOTE:  All searching must be done from lowest to highest in order to handle *)
(*	register pairs properly *)
procedure AllocReg(state : RegState; e : EESElement; ptype : pcodetype) : Reg;
var
    r, stealr : Reg;
    found, tworegs : boolean;
begin
    tworegs := ptype = tlongreal;
    if state = REGRETURN then
	(* return reg must be in special place *)
	if (regTable[RETURNREG].state <> REGFREE) or
		(tworegs and 
		    (regTable[VAL(Reg, ord(RETURNREG)+1)].state <> REGFREE))
	then
	    Error('AllocReg: return reg not available');
	end;
	r := RETURNREG;
    elsif state = REGSP then
	(* Want stack pointer "allocated" *)
	if (regTable[SPREG].state # REGFREE) then
	    Error('AllocReg: sp not available');
	elsif tworegs then
	    Error('AllocReg: sp only one register');
	end;
	r := SPREG;
    else
	(* look for a reg in normal ones *)
        r := FIRSTREG;
	found := false;
	while (r <= LASTEESREG) and not found do
	    if (regTable[r].state = REGFREE) and
		(not tworegs or ((r < LASTEESREG) and 
		    (regTable[VAL(Reg, ord(r)+1)].state = REGFREE)))
	    then
		found := true;
	    else
		inc(r);
	    end;
	end;
	if not found then
	    (* look for an unused display reg *)
	    r := FIRSTREG;
	    found := false;
	    while (r <= LASTEESREG) and not found do
		if ((regTable[r].state = REGDISP) and not regTable[r].inUse) or
			(regTable[r].state = REGFREE)
		then
		    if not tworegs then
			found := true;
		    elsif r = LASTEESREG then
			(* can't split value between regs and memory *)
		    elsif (regTable[VAL(Reg, ord(r)+1)].state = REGFREE) or
			((regTable[VAL(Reg, ord(r)+1)].state = REGDISP) and
			    not regTable[VAL(Reg, ord(r)+1)].inUse)
		    then
			found := true;
		    end;
		end;
		if not found then
		    inc(r);
		end;
	    end;
	end;
	if not found then
	    (* didn't find a real reg, allocate a virtual one *)
	    r := FIRSTVREG;
	    found := false;
	    while (r <= LASTVREG) and not found do
		(* always make sure next reg is available, in case we need it *)
		(*   in order to steal the first of a pair *)
		if (regTable[r].state = REGFREE) and
			(regTable[VAL(Reg, ord(r)+1)].state = REGFREE)
		then
		    found := true;
		else
		    inc(r);
		end;
	    end;
	    if not found then
		Error('AllocReg: virtual reg not available');
	    elsif state in RegStateSet{REGREG, REGDISP} then
		(* need a real reg, find one to steal *)
		if tworegs then
		    Error('AllocReg: tworegs and REGREG');
		end;
		stealr := FIRSTREG;
		found := false;
		while (stealr <= LASTEESREG) and not found do
		    if regTable[stealr].state = REGEES then
			found := true;
		    else
			inc(stealr);
		    end;
		end;
		if not found then
		    Error('AllocReg: could not steal reg');
		else
		    StealReg(stealr,r);
		    r := stealr;
		end;
	    end;
	end;
    end;
    regTable[r].state := state;
    regTable[r].eesElement := e;
    if regTable[r].maskbit <> 0 then
	if not odd(regmask div regTable[r].maskbit) then
	    regmask := regmask + regTable[r].maskbit;
	end;
    end;
    if tworegs then
	regTable[VAL(Reg, ord(r)+1)].state := state;
	regTable[VAL(Reg, ord(r)+1)].eesElement := e;
	if regTable[VAL(Reg, ord(r)+1)].maskbit <> 0 then
	    if not odd(regmask div regTable[VAL(Reg, ord(r)+1)].maskbit) then
		regmask := regmask + regTable[VAL(Reg, ord(r)+1)].maskbit;
	    end;
	end;
	regTable[r].regGroup := RGPAIR1;
	regTable[VAL(Reg, ord(r)+1)].regGroup := RGPAIR2;
    end;
    return r;
end AllocReg;

procedure FreeReg(var r : Reg);
begin
    if not ActiveReg(r) then
	r := NULLREG;
    elsif regTable[r].state = REGDISP then
	Error('FreeReg REGDISP');
    elsif regTable[r].regGroup = RGSINGLE then
	regTable[r].state := REGFREE;
	regTable[r].eesElement := 0;
	r := NULLREG;
    else
	if regTable[r].regGroup <> RGPAIR1 then
	    Error('FreeReg: error in register pairing');
	end;
	regTable[r].state := REGFREE;
	regTable[r].regGroup := RGSINGLE;
	regTable[r].eesElement := 0;
	regTable[VAL(Reg, ord(r)+1)].state := REGFREE;
	regTable[VAL(Reg, ord(r)+1)].regGroup := RGSINGLE;
	regTable[VAL(Reg, ord(r)+1)].eesElement := 0;
	r := NULLREG;
    end;
end FreeReg;

procedure DumpEES;
var i : integer;
begin
    Comment; S(' EES: top='); I(top); L;
    for i:=top to NUMTEMPS by -1 do
	if (i >= 1) or (ees[i].inUse) then
	    Comment; S(' ees['); I(i); S('], type=');
		S(typeString[ees[i].ptype]);
		S(', size='); I(ees[i].size); S(', kind=');
		S(kindString[ees[i].kind]); L;
	    Comment; S(' constInt='); I(ees[i].constInt);
		S(', dreg='); R(ees[i].dreg); S(', breg='); R(ees[i].breg);
		S(', sreg='); R(ees[i].sreg);
		S(', sunits='); I(ees[i].sunits);
		L;
	    Comment; S('  addrLevel='); I(ees[i].addrLevel);
		S(', addrMemType='); C(ees[i].addrMemType);
		S(', addrOffset='); I(ees[i].addrOffset);
		S(', addrBlock='); I(ees[i].addrBlock);
		L;
	    Comment; S('  indirect='); S(boolString[ees[i].indirect]);
		S(', smemoffset='); I(ees[i].smemoffset);
		S(', smemsize='); I(ees[i].smemsize);
		S(', inUse='); S(boolString[ees[i].inUse]);
		L;
	end;
    end;
end DumpEES;            (* end of dumpeesstack *)

procedure DumpReg;
var
    r : Reg;
begin
    Comment; S(' Registers'); L;
    for r := RETURNREG to LASTREG do
	Comment; C(' '); R(r); C(' '); S(stateString[regTable[r].state]);
	if regTable[r].state = REGDISP then
	    S(' l='); I(regTable[r].level);
	    S(', iu='); S(boolString[regTable[r].inUse]);
	    S(', p='); S(boolString[regTable[r].param]);
	elsif regTable[r].state = REGEES then
	    C(' '); I(regTable[r].eesElement);
	    S(', regGroup='); S(groupString[regTable[r].regGroup]);
	end;
	L;
    end;
end DumpReg;

procedure AllocDisplay(level : integer; mt : char);
var
    r : Reg;
begin
    r := FIRSTREG;
    while r <= LASTEESREG do
	if (regTable[r].state = REGDISP) and (regTable[r].level = level) and 
		    (regTable[r].param = (mt = 'p')) then
	    regTable[r].inUse := true;
	    return;
	end;
	inc(r);
    end (* while *);
    (* Couldn't find existing entry, allocate one *)
    r := AllocReg(REGDISP,0,taddress);
    regTable[r].level := level;
    regTable[r].param := mt = 'p';
    regTable[r].inUse := true;
    Op('movl'); GN; S('runtime__display+'); I(level*4); X; R(r); L;
    if mt = 'p' then
	Op('movl'); I(APOFF); C('('); R(r); C(')'); X; R(r); L;
    end;
end AllocDisplay;

procedure DispReg(level : integer; mt : char) : Reg;
var
    r : Reg;
    found : boolean;
begin
    if level < 1 then
	Error('DispReg: level < 1');
    end;
    found := false;
    r := FIRSTREG;
    if level = curlev then
	found := true;
	if mt = 'p' then
	    r := ap;
	else
	    r := fp;
	end;
    end;
    while not found and (r <= LASTEESREG) do
	if regTable[r].state = REGDISP then
	    found := (regTable[r].level = level)
			and (regTable[r].param = (mt = 'p'));
	end;
	if not found then
	    inc(r);
	end;
    end;
    if not found then
	Error('DispReg: reg not found');
    end;
    return r;
end DispReg;

procedure NeedDisp(level : integer; mt : char);
begin
    if (level > 0) and (level < curlev) then
	AllocDisplay(level,mt);
    elsif level <= 0 then
	Error('NeedDisp: level <= 0');
    else
	Error('NeedDisp: level >= curlev');
    end;
end NeedDisp;

procedure FreeDisp;
var
    r : Reg;
begin
    for r := FIRSTREG to LASTEESREG do
	if regTable[r].state = REGDISP then
	    regTable[r].inUse := false;
	end;
    end;
end FreeDisp;

procedure ClearDisp;
var
    r : Reg;
begin
    for r := FIRSTREG to LASTEESREG do
	if regTable[r].state = REGDISP then
	    regTable[r].state := REGFREE;
	end;
    end;
end ClearDisp;

procedure Registerize(var r : Reg);
var
    realr : Reg;
begin
    if r in RegSet{NULLREG, rt0..rt9} then
	(* nothing to do *)
    else
	if r in RegSet{RETURNREG..LASTREG, SPREG} then
	    regTable[r].state := REGREG;
	    if regTable[r].regGroup = RGPAIR1 then
		if regTable[VAL(Reg, ord(r)+1)].regGroup <> RGPAIR2 then
		    Error('Registerize: bad register pairing');
		end;
		regTable[VAL(Reg, ord(r)+1)].state := REGREG;
	    end;
	elsif regTable[r].regGroup <> RGSINGLE then
	    realr := AllocReg(REGREG,0,tlongreal);
	    Op('movq'); R(r); X; R(realr); L;
	    FreeReg(r);
	    r := realr;
	else
	    realr := AllocReg(REGREG,0,taddress);
	    Op('movl'); R(r); X; R(realr); L;
	    FreeReg(r);
	    r := realr;
	end;
	if regTable[r].state <> REGREG then
	    Error('Registerize: failed');
	end;
    end;
end Registerize;

procedure DeRegisterize(r : Reg; e : EESElement);
begin
    (* allow register to be stolen if necessary *)
    regTable[r].state := REGEES;
    regTable[r].eesElement := e;
end DeRegisterize;

procedure SaveReg(var r : Reg; e : EESElement);
var
    saver : Reg;
begin
    if regTable[r].regGroup = RGSINGLE then
	saver := SR0;
	while (saver <= LASTSREG) and (regTable[saver].state <> REGFREE) do
	    inc(saver);
	end;
	if saver > LASTSREG then
	    Error('SaveReg: not enough save regs');
	else
	    regTable[saver].state := REGEES;
	    regTable[saver].eesElement := e;
	    if (ord(saver)-ord(SAVEDREG)) > rmemoff then
		rmemoff := ord(saver) - ord(SAVEDREG);
	    end;
	    Op('movl'); R(r); X; C('r'); I(curblockid); C('+');
		I((ord(saver)-ord(SR0))*BYTESPERWORD); S('(fp)'); L;
	    FreeReg(r);
	    r := saver;
	end;
    else
	saver := SD0;
	while (saver <= LASTSDREG) and (regTable[saver].state <> REGFREE) do
	    inc(saver);
	end;
	if saver > LASTSDREG then
	    Error('SaveReg: not enough save regs');
	else
	    regTable[saver].state := REGEES;
	    regTable[saver].eesElement := e;
	    if (ord(saver)-ord(SAVEDDREG)) > dmemoff then
		dmemoff := ord(saver) - ord(SAVEDDREG);
	    end;
	    Op('movq'); R(r); X; C('d'); I(curblockid); C('+');
		I((ord(saver)-ord(SD0))*2*BYTESPERWORD); S('(fp)'); L;
	    FreeReg(r);
	    r := saver;
	end;
    end;
end SaveReg;

procedure SaveRegs(below : EESElement);
var
    e : EESElement;
begin
    e := below;
    while (e > 0) and (ees[e].callNest = callNest) do
	if ActiveReg(ees[e].sreg) then
	    SaveReg(ees[e].sreg,e);
	end;
	if ActiveReg(ees[e].breg) then
	    SaveReg(ees[e].breg,e);
	end;
	if ActiveReg(ees[e].dreg) then
	    SaveReg(ees[e].dreg,e);
	end;
	dec(e);
    end;
    for e := -1 to NUMTEMPS by -1 do
	if ees[e].inUse and (ees[e].callNest = callNest) then
	    if ActiveReg(ees[e].sreg) then
		SaveReg(ees[e].sreg,e);
	    end;
	    if ActiveReg(ees[e].breg) then
		SaveReg(ees[e].breg,e);
	    end;
	    if ActiveReg(ees[e].dreg) then
		SaveReg(ees[e].dreg,e);
	    end;
	end;
    end;
    inc(callNest);
end SaveRegs;

procedure RestoreReg(var r : Reg; e : EESElement);
var
    newr : Reg;
begin
    if r < SAVEDDREG then
	if (regTable[r].state <> REGEES) or (regTable[r].eesElement <> e) then
	    Error('RestoreReg: not in use?');
	else
	    newr := AllocReg(REGEES,e,tinteger);
	    Op('movl'); C('r'); I(curblockid); C('+');
		I((ord(r)-ord(SR0))*BYTESPERWORD); S('(fp)'); X; R(newr); L;
	    regTable[r].state := REGFREE;
	    r := newr;
	end;
    else
	if (regTable[r].state <> REGEES) or (regTable[r].eesElement <> e) then
	    Error('RestoreReg: not in use?');
	else
	    newr := AllocReg(REGEES,e,tlongreal);
	    Op('movq'); C('d'); I(curblockid); C('+');
		I((ord(r)-ord(SD0))*2*BYTESPERWORD); S('(fp)'); X; R(newr); L;
	    regTable[r].state := REGFREE;
	    r := newr;
	end;
    end;
end RestoreReg;

procedure RestoreRegs(below : EESElement);
var
    e : EESElement;
begin
    for e := NUMTEMPS to -1 do
	if ees[e].inUse and (ees[e].callNest = callNest) then
	    if ees[e].dreg > SAVEDREG then
		RestoreReg(ees[e].dreg,e);
	    end;
	    if ees[e].breg > SAVEDREG then
		RestoreReg(ees[e].breg,e);
	    end;
	    if ees[e].sreg > SAVEDREG then
		RestoreReg(ees[e].sreg,e);
	    end;
	end;
    end;
    for e := 1 to below do
	if ees[e].callNest = callNest then
	    if ees[e].dreg > SAVEDREG then
		RestoreReg(ees[e].dreg,e);
	    end;
	    if ees[e].breg > SAVEDREG then
		RestoreReg(ees[e].breg,e);
	    end;
	    if ees[e].sreg > SAVEDREG then
		RestoreReg(ees[e].sreg,e);
	    end;
	end;
    end;
end RestoreRegs;

(* MoveReg: reassign reg r to element e *)
procedure MoveReg(e : EESElement; r : Reg);
begin
    if ActiveReg(r) then
	regTable[r].eesElement := e;
	if regTable[r].regGroup <> RGSINGLE then
	    if regTable[r].regGroup <> RGPAIR1 then
		Error('MoveReg: error in register pairing');
	    end;
	    regTable[VAL(Reg, ord(r)+1)].eesElement := e;
	end;
    end;
end MoveReg;

procedure SwapEES(a,b : EESElement);
var
    tmp : EESRecord;
begin
    (* reassign regs *)
    MoveReg(a,ees[b].dreg);
    MoveReg(a,ees[b].breg);
    MoveReg(a,ees[b].sreg);
    MoveReg(b,ees[a].dreg);
    MoveReg(b,ees[a].breg);
    MoveReg(b,ees[a].sreg);
    tmp := ees[a];
    ees[a] := ees[b];
    ees[b] := tmp;
end SwapEES;


var
    e : EESElement;

begin
    typeString[tundefined] := 'tundefined';
    typeString[taddress] := 'taddress';
    typeString[tboolean] := 'tboolean';
    typeString[tchar] := 'tchar';
    typeString[tinteger] := 'tinteger';
    typeString[tcardinal] := 'tcardinal';
    typeString[tproc] := 'tproc';
    typeString[trecord] := 'trecord';
    typeString[treal] := 'treal';
    typeString[tlongreal] := 'tlongreal';
    typeString[tset] := 'tset';
    typeString[tstring] := 'tstring';

    boolString[true] := 'true';
    boolString[false] := 'false';

    groupString[RGSINGLE] := 'RGSINGLE';
    groupString[RGPAIR1] := 'RGPAIR1';
    groupString[RGPAIR2] := 'RGPAIR2';

    kindString[EESDATA] := 'EESDATA';
    kindString[EESADDR] := 'EESADDR';
    kindString[EESVAR] := 'EESVAR';

    stateString[REGALLOC] := 'REGALLOC';
    stateString[REGFREE] := 'REGFREE';
    stateString[REGRETURN] := 'REGRETURN';
    stateString[REGREG] := 'REGREG';
    stateString[REGTEMP] := 'REGTEMP';
    stateString[REGDISP] := 'REGDISP';
    stateString[REGEES] := 'REGEES';

    regString[NULLREG] := 'NUL';
    regString[r0] := 'r0';
    regString[r1] := 'r1';
    regString[r2] := 'r2';
    regString[r3] := 'r3';
    regString[r4] := 'r4';
    regString[r5] := 'r5';
    regString[r6] := 'r6';
    regString[r7] := 'r7';
    regString[r8] := 'r8';
    regString[r9] := 'r9';
    regString[r10] := 'r10';
    regString[r11] := 'r11';
    regString[ap] := 'ap';
    regString[fp] := 'fp';
    regString[sp] := 'sp';
    regString[pc] := 'pc';
    regString[r20] := 'r20';
    regString[r21] := 'r21';
    regString[r22] := 'r22';
    regString[r23] := 'r23';
    regString[r24] := 'r24';
    regString[r25] := 'r25';
    regString[r26] := 'r26';
    regString[r27] := 'r27';
    regString[r28] := 'r28';
    regString[r29] := 'r29';
    regString[r30] := 'r30';
    regString[r31] := 'r31';
    regString[r32] := 'r32';
    regString[r33] := 'r33';
    regString[r34] := 'r34';
    regString[r35] := 'r35';
    regString[r36] := 'r36';
    regString[r37] := 'r37';
    regString[r38] := 'r38';
    regString[r39] := 'r39';
    regString[r40] := 'r40';
    regString[rt0] := 'rt0';
    regString[rt1] := 'rt1';
    regString[rt2] := 'rt2';
    regString[rt3] := 'rt3';
    regString[rt4] := 'rt4';
    regString[rt5] := 'rt5';
    regString[rt6] := 'rt6';
    regString[rt7] := 'rt7';
    regString[rt8] := 'rt8';
    regString[rt9] := 'rt9';
    regString[SAVEDREG] := 'SavedDReg';
    regString[SR0] := 'SR0';
    regString[SR1] := 'SR1';
    regString[SR2] := 'SR2';
    regString[SR3] := 'SR3';
    regString[SR4] := 'SR4';
    regString[SR5] := 'SR5';
    regString[SR6] := 'SR6';
    regString[SR7] := 'SR7';
    regString[SR8] := 'SR8';
    regString[SR9] := 'SR9';
    regString[SD0] := 'SD0';
    regString[SD1] := 'SD1';
    regString[SD2] := 'SD2';
    regString[SD3] := 'SD3';
    regString[SD4] := 'SD4';
    regString[SD5] := 'SD5';
    regString[SD6] := 'SD6';
    regString[SD7] := 'SD7';
    regString[SD8] := 'SD8';
    regString[SD9] := 'SD9';

    ClearReg;

    for e := NUMTEMPS to EESSTACKSIZE do
	ClearEES(e);
    end;
    top := 0;
    callNest := 0;
end EES.
