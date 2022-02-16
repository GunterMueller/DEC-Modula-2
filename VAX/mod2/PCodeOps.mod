implementation module PCodeOps;

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

begin
    pCodeOpNames[PCABS] := 'abs';
    pCodeOpNames[PCAD2] := 'ad2';
    pCodeOpNames[PCADD] := 'add';
    pCodeOpNames[PCADR] := 'adr';
    pCodeOpNames[PCAND] := 'and';
    pCodeOpNames[PCBGN] := 'bgn';
    pCodeOpNames[PCBIT] := 'bit';
    pCodeOpNames[PCCAP] := 'cap';
    pCodeOpNames[PCCEP] := 'cep';
    pCodeOpNames[PCCHK] := 'chk';
    pCodeOpNames[PCCHR] := 'chr';
    pCodeOpNames[PCCIP] := 'cip';
    pCodeOpNames[PCCJP] := 'cjp';
    pCodeOpNames[PCCOM] := 'com';
    pCodeOpNames[PCCTS] := 'cts';
    pCodeOpNames[PCCUP] := 'cup';
    pCodeOpNames[PCDBG] := 'dbg';
    pCodeOpNames[PCDEC] := 'dec';
    pCodeOpNames[PCDEF] := 'def';
    pCodeOpNames[PCDIF] := 'dif';
    pCodeOpNames[PCDIV] := 'div';
    pCodeOpNames[PCDV2] := 'dv2';
    pCodeOpNames[PCEND] := 'end';
    pCodeOpNames[PCENT] := 'ent';
    pCodeOpNames[PCEQU] := 'equ';
    pCodeOpNames[PCEXI] := 'exi';
    pCodeOpNames[PCFJP] := 'fjp';
    pCodeOpNames[PCFLT] := 'flt';
    pCodeOpNames[PCFOR] := 'for';
    pCodeOpNames[PCGEQ] := 'geq';
    pCodeOpNames[PCGRT] := 'grt';
    pCodeOpNames[PCINC] := 'inc';
    pCodeOpNames[PCIND] := 'ind';
    pCodeOpNames[PCINI] := 'ini';
    pCodeOpNames[PCINN] := 'inn';
    pCodeOpNames[PCINT] := 'int';
    pCodeOpNames[PCIOR] := 'ior';
    pCodeOpNames[PCLAB] := 'lab';
    pCodeOpNames[PCLAO] := 'lao';
    pCodeOpNames[PCLCA] := 'lca';
    pCodeOpNames[PCLDA] := 'lda';
    pCodeOpNames[PCLDC] := 'ldc';
    pCodeOpNames[PCLDO] := 'ldo';
    pCodeOpNames[PCLEQ] := 'leq';
    pCodeOpNames[PCLES] := 'les';
    pCodeOpNames[PCLOD] := 'lod';
    pCodeOpNames[PCMAX] := 'max';
    pCodeOpNames[PCMIN] := 'min';
    pCodeOpNames[PCMOD] := 'mod';
    pCodeOpNames[PCMP2] := 'mp2';
    pCodeOpNames[PCMST] := 'mst';
    pCodeOpNames[PCMUP] := 'mup';
    pCodeOpNames[PCNEG] := 'neg';
    pCodeOpNames[PCNEQ] := 'neq';
    pCodeOpNames[PCNOT] := 'not';
    pCodeOpNames[PCODD] := 'odd';
    pCodeOpNames[PCORD] := 'ord';
    pCodeOpNames[PCRET] := 'ret';
    pCodeOpNames[PCSAC] := 'sac';
    pCodeOpNames[PCSAL] := 'sal';
    pCodeOpNames[PCSAV] := 'sav';
    pCodeOpNames[PCSB2] := 'sb2';
    pCodeOpNames[PCSDF] := 'sdf';
    pCodeOpNames[PCSET] := 'set';
    pCodeOpNames[PCSEX] := 'sex';
    pCodeOpNames[PCSIN] := 'sin';
    pCodeOpNames[PCSML] := 'sml';
    pCodeOpNames[PCSRO] := 'sro';
    pCodeOpNames[PCSTN] := 'stn';
    pCodeOpNames[PCSTO] := 'sto';
    pCodeOpNames[PCSTR] := 'str';
    pCodeOpNames[PCSUB] := 'sub';
    pCodeOpNames[PCSYM] := 'sym';
    pCodeOpNames[PCTJP] := 'tjp';
    pCodeOpNames[PCTRC] := 'trc';
    pCodeOpNames[PCTYP] := 'typ';
    pCodeOpNames[PCUJP] := 'ujp';
    pCodeOpNames[PCUNI] := 'uni';
    pCodeOpNames[PCUSE] := 'use';
    pCodeOpNames[PCXJP] := 'xjp';
    pCodeOpNames[PCSST] := 'sst';
    pCodeOpNames[PCLJP] := 'ljp';
    pCodeOpNames[PCRST] := 'rst';
    pCodeOpNames[PCRND] := 'rnd';
    pCodeOpNames[PCPAR] := 'par';

    pCodeOpNames[PCXFC] := 'xfc';	(* CED - 8/13/87 *)
    pCodeOpNames[PCZZZ] := 'zzz';
end PCodeOps.
