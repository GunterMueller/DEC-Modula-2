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

# Modula-2 coroutine implementation subroutines
# This file is named .c to avoid accidental deletion

.set	DISPLAYLEVELS, 16
.set	DISPLAYSIZE, DISPLAYLEVELS*4
# A "process" is represented by an area defined as follows:
#   Note:  stack grows downward, so process variable points to end.
#	Name	Offset	function	
.set	status,	-4	# indicate whether process has started
.set	entry,	-8	# entry point for process
.set	size,	-12	# size of stack area in bytes
.set	savesp,	-16	# saved sp of process
.set	display,savesp-DISPLAYSIZE	# saved display
.set	stack,	display	# start of stack area for new process
.set	MINSTACK,512	# at least 512 bytes for a stack

.data
# currently running process, initially the main program
.globl	_SYSTEM_currentprocess
_SYSTEM_currentprocess:
	.long	_SYSTEM_currentprocessstate
	.space	DISPLAYSIZE	# display
	.long	0	# savesp = ?
	.long	0x7fffffff	# size = large
	.long	0	# entry = none
	.long	1	# status = running
_SYSTEM_currentprocessstate:
.text
	.comm	_runtime__display,DISPLAYSIZE
.globl	_SYSTEM_newprocess
_SYSTEM_newprocess:
	.word	0
	movl	8(ap),r1	# stack area for process
	movl	12(ap),r2       # get size to allocate (in bytes)
	cmpl	r2,$MINSTACK	# make sure it isn't tiny
	jlss	toosmall
	addl2	r2,r1		# point to end of stack area
	clrl	status(r1)	# indicate process not yet started
	movl	4(ap),entry(r1)	# entry point of process
	movl	12(ap),size(r1)	# size of stack
	movl	r1,*16(ap)	# use stack record as process object
	ret
toosmall:
	pushab	toosmallmsg
	calls	$1,_printf
	calls	$0,_abort
toosmallmsg:.asciz	"NEWPROCESS: stack too small\n"

.set	ALLREGMASK,16383	# mask to save r0-fp
.globl	_SYSTEM_transfer
_SYSTEM_transfer:
	.word	0
	pushr	$ALLREGMASK
	movl	_SYSTEM_currentprocess,r6	# get current process
	movl	*8(ap),r7	# process to switch to (var parameter!)
	movl	sp,savesp(r6)	# save stack pointer for old process
	subl3	sp,r6,r0	# amount of stack used
	cmpl	r0,size(r6)	# check to be sure not too much
	jgeq	overflow	# overflowed stack
	movc3	$DISPLAYSIZE,_runtime__display,display(r6) # save display
	movl	r6,*4(ap)	# return pointer to old process
	movl	r7,_SYSTEM_currentprocess	# new process to run

	tstl	status(r7)	# has new process been run before?
	jeql	firsttime	# no, start it

	# run new process
	movl	savesp(r7),sp	# restore stack pointer
	movc3	$DISPLAYSIZE,display(r7),_runtime__display # restore display
	popr	$ALLREGMASK
	ret
firsttime:
	# start process at procedure
	movl	$1,status(r7)	# indicate running
	movl	entry(r7),r8	# get starting address
	movab	stack(r7),fp	# set frame pointer
	movl	fp,sp		#  and stack pointer
	calls	$0,(r8)		# enter procedure
#  will return here if procedure terminates.  terminate whole program
	clrl	r0
	.globl	_exit
	calls	$0,_exit

overflow:
	pushab	overflowmsg
	calls	$1,_printf
	calls	$0,_abort
overflowmsg:.asciz	"TRANSFER: stack overflowed\n"

