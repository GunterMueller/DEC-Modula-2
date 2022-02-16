implementation module Tokens;

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


from Strings import
    NonHashText;

begin (* Tokens *)
    stringToken[TKENDOFFILE]    := NonHashText('end of file');
    stringToken[TKPLUS]		:= NonHashText('+');
    stringToken[TKMINUS]	:= NonHashText('-');
    stringToken[TKASTERISK]     := NonHashText('*');
    stringToken[TKSLASH]	:= NonHashText('/');
    stringToken[TKASSIGN]	:= NonHashText(':=');
    stringToken[TKAMPERSAND]    := NonHashText('&');
    stringToken[TKDOT]		:= NonHashText('.');
    stringToken[TKCOMMA]	:= NonHashText(',');
    stringToken[TKSEMICOLON]    := NonHashText(';');
    stringToken[TKLPAREN]	:= NonHashText('(');
    stringToken[TKLBRACKET]     := NonHashText('[');
    stringToken[TKLBRACE]	:= NonHashText('{');
    stringToken[TKUPARROW]      := NonHashText('^');
    stringToken[TKEQUALS]	:= NonHashText('=');
    stringToken[TKSHARP]	:= NonHashText('#');
    stringToken[TKLESS]		:= NonHashText('<');
    stringToken[TKGREATER]      := NonHashText('>');
    stringToken[TKNOTEQUAL]     := NonHashText('<>');
    stringToken[TKLSEQUAL]      := NonHashText('<=');
    stringToken[TKGREQUAL]      := NonHashText('>=');
    stringToken[TKDOTDOT]	:= NonHashText('..');
    stringToken[TKCOLON]	:= NonHashText(':');
    stringToken[TKRPAREN]	:= NonHashText(')');
    stringToken[TKRBRACKET]     := NonHashText(']');
    stringToken[TKRBRACE]	:= NonHashText('}');
    stringToken[TKBAR]		:= NonHashText('|');
    stringToken[TKIDENT]	:= NonHashText('identifier');
    stringToken[TKCARDCONST]    := NonHashText('card const');
    stringToken[TKREALCONST]    := NonHashText('real const');
    stringToken[TKCHARCONST]    := NonHashText('char const');
    stringToken[TKSTRCONST]     := NonHashText('string const');
    stringToken[TKAND]		:= NonHashText('and');
    stringToken[TKARRAY]	:= NonHashText('array');
    stringToken[TKBEGIN]	:= NonHashText('begin');
    stringToken[TKBY]		:= NonHashText('by');
    stringToken[TKCASE]		:= NonHashText('case');
    stringToken[TKCONST]	:= NonHashText('const');
    stringToken[TKDEFINITION]   := NonHashText('definition');
    stringToken[TKDIV]		:= NonHashText('div');
    stringToken[TKDO]		:= NonHashText('do');
    stringToken[TKELSE]		:= NonHashText('else');
    stringToken[TKELSIF]	:= NonHashText('elsif');
    stringToken[TKEND]		:= NonHashText('end');
    stringToken[TKEXIT]		:= NonHashText('exit');
    stringToken[TKEXPORT]	:= NonHashText('export');
    stringToken[TKFOR]		:= NonHashText('for');
    stringToken[TKFROM]		:= NonHashText('from');
    stringToken[TKIF]		:= NonHashText('if');
    stringToken[TKIMPLEMENTATION]:= NonHashText('implementation');
    stringToken[TKIMPORT]	:= NonHashText('import');
    stringToken[TKIN]		:= NonHashText('in');
    stringToken[TKLOOP]		:= NonHashText('loop');
    stringToken[TKMOD]		:= NonHashText('mod');
    stringToken[TKMODULE]	:= NonHashText('module');
    stringToken[TKNOT]		:= NonHashText('not');
    stringToken[TKOF]		:= NonHashText('of');
    stringToken[TKOR]		:= NonHashText('or');
    stringToken[TKPOINTER]      := NonHashText('pointer');
    stringToken[TKPROCEDURE]    := NonHashText('procedure');
    stringToken[TKQUALIFIED]    := NonHashText('qualified');
    stringToken[TKRECORD]	:= NonHashText('record');
    stringToken[TKREPEAT]	:= NonHashText('repeat');
    stringToken[TKRETURN]	:= NonHashText('return');
    stringToken[TKSET]		:= NonHashText('set');
    stringToken[TKTHEN]		:= NonHashText('then');
    stringToken[TKTO]		:= NonHashText('to');
    stringToken[TKTYPE]		:= NonHashText('type');
    stringToken[TKUNTIL]	:= NonHashText('until');
    stringToken[TKVAR]		:= NonHashText('var');
    stringToken[TKWHILE]	:= NonHashText('while');
    stringToken[TKWITH]		:= NonHashText('with');
    stringToken[TKATSIZE]	:= NonHashText('@size');
    stringToken[TKATALIGN]	:= NonHashText('@align');
    stringToken[TKATPASCAL]     := NonHashText('@pascal');
    stringToken[TKATC]		:= NonHashText('@c');
    stringToken[TKATNOCHECK]    := NonHashText('@nocheck');
    stringToken[TKATNILCHECK]   := NonHashText('@nilcheck');
    stringToken[TKNOCOUNT]      := NonHashText('@nocount');
    stringToken[TKEXTERNAL]     := NonHashText('@external');
    stringToken[TKGLOBAL]	:= NonHashText('@global');
    stringToken[TKATASM]          := NonHashText('@asm');
    stringToken[TKDYNARRAY]     := NonHashText('dynarray');
    stringToken[TKSUBARRAY]     := NonHashText('subarray');
    stringToken[TKATLOCAL]      := NonHashText('@local');
    stringToken[TKLEFTTORIGHT]  := NonHashText('@lefttoright');
    stringToken[TKRIGHTTOLEFT]  := NonHashText('@rightoleft');
    stringToken[TKINLINE]	:= NonHashText('@inline');
    stringToken[TKSHARED]	:= NonHashText('@shared');
    stringToken[TKNOINIT]	:= NonHashText('@noinit');
$if pascal then
    stringToken[TKDOWNTO]	:= NonHashText('downto');
    stringToken[TKFILE]		:= NonHashText('file');
    stringToken[TKFORWARD]      := NonHashText('forward');
    stringToken[TKPACKED]	:= NonHashText('packed');
    stringToken[TKPROGRAM]      := NonHashText('program');
    stringToken[TKOCT]		:= NonHashText('oct');
    stringToken[TKHEX]		:= NonHashText('hex');
$end
    stringToken[TKNULL]		:= NonHashText('no token');
end Tokens.
