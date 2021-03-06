#*****************************************************************************
#									     *
#              Copyright 1984-1990 Digital Equipment Corporation             *
#                          All Rights Reserved				     *
# 								             *
#  Permission to use, copy, and modify this software and its documentation   *
#  is hereby granted only under the following terms and conditions.  Both    *
#  the above copyright notice and this permission notice must appear in all  *
#  copies of the software, derivative works or modified versions, and any    *
#  portions thereof, and both notices must appear in supporting              *
#  documentation.							     *
# 									     *
#  Users of this software agree to the terms and conditions set forth        *
#  herein, and hereby grant back to Digital a non-exclusive, unrestricted,   *
#  royalty-free right and license under any changes, enhancements or         *
#  extensions made to the core functions of the software, including but not  *
#  limited to those affording compatibility with other hardware or software  *
#  environments, but excluding applications which incorporate this software. *
#  Users further agree to use their best efforts to return to Digital any    *
#  such changes, enhancements or extensions that they make and inform        *
#  Digital of noteworthy uses of this software.  Correspondence should be    *
#  provided to Digital at:						     *
#  									     *
#                        Director of Licensing				     *
#                        Western Research Laboratory			     *
#                        Digital Equipment Corporation			     *
#                        100 Hamilton Avenue				     *
#                        Palo Alto, California  94301  			     *
#  									     *
#  This software may be distributed (but not offered for sale or transferred *
#  for compensation) to third parties, provided such third parties agree to  *
#  abide by the terms and conditions of this notice.  			     *
#  									     *
#  THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS    *
#  ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED        *
#  WARRANTIES OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL    *
#  EQUIPMENT CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR     *
#  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF    *
#  USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR     *
#  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR    *
#  PERFORMANCE OF THIS SOFTWARE.				    	     *
# 									     *
#*****************************************************************************

	.align	1
	.globl	_runtime__udiv
_runtime__udiv:	.word	0
	pushl	8(ap)
	pushl	4(ap)
	calls	$2,udiv
	ret
	.globl	_runtime__umod
_runtime__umod:	.word	0
	pushl	8(ap)
	pushl	4(ap)
	calls	$2,udiv
	mull2	8(ap),r0
	subl3	r0,4(ap),r0
	ret
