/*****************************************************************************
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
 *****************************************************************************/

#include <stdio.h>

/* This file contains routines used by the Modula-2 runtime */
/*  and the standard library modules */

#ifdef unix
#define globalref extern
#define globaldef
#endif

globaldef int unix_argc;
globaldef char **unix_argv, **unix_environ;
globaldef FILE *unix_stdin, *unix_stdout, *unix_stderr;
globalref int parameters_argc;
globalref char **parameters_argv, **parameters_envp;

globaldef int runtime__init_complete = 0;
globaldef int runtime__display[16];
globaldef FILE *IO_INPUT, *IO_OUTPUT, *IO_TERMINAL;
#ifdef vms
globaldef int r20,r21,r22,r23,r24,r25,r26,r27,r28,r29;
globaldef int r30,r31,r32,r33,r34,r35,r36,r37,r38,r39, r40;
#endif

/* Pascal stuff */
#ifdef unix
globalref int _argc;
globalref char **_argv;
#else
/* Just define these variables here, which are in Unix Pascal runtime,
   so that the code below doesn't complain.  We don't run Pascal on VMS
   anyway. */
globaldef int _argc;
globaldef char **_argv;
#endif
/* ------------ */




runtime__init(argc,argv,envp) int argc; char *argv[], *envp[]; {

    if (runtime__init_complete) return;
    runtime__init_complete = 1;

/* Pascal stuff */
    _argc = argc;
    _argv = argv;
/* ------------ */

    parameters_argc = argc;
    parameters_argv = argv;
    parameters_envp = envp;
    unix_argc = argc;
    unix_argv = argv;
    unix_environ = envp;
    unix_stdin = stdin;
    unix_stdout = stdout;
    unix_stderr = stderr;
    IO_INPUT = stdin;
    IO_OUTPUT = stdout;
    IO_TERMINAL = stderr;
#ifdef unix
    asm(".data");
    asm("	.globl	r20");
    asm("r20:	.long	0");
    asm("	.globl	r21");
    asm("r21:	.long	0");
    asm("	.globl	r22");
    asm("r22:	.long	0");
    asm("	.globl	r23");
    asm("r23:	.long	0");
    asm("	.globl	r24");
    asm("r24:	.long	0");
    asm("	.globl	r25");
    asm("r25:	.long	0");
    asm("	.globl	r26");
    asm("r26:	.long	0");
    asm("	.globl	r27");
    asm("r27:	.long	0");
    asm("	.globl	r28");
    asm("r28:	.long	0");
    asm("	.globl	r29");
    asm("r29:	.long	0");
    asm("	.globl	r30");
    asm("r30:	.long	0");
    asm("	.globl	r31");
    asm("r31:	.long	0");
    asm("	.globl	r32");
    asm("r32:	.long	0");
    asm("	.globl	r33");
    asm("r33:	.long	0");
    asm("	.globl	r34");
    asm("r34:	.long	0");
    asm("	.globl	r35");
    asm("r35:	.long	0");
    asm("	.globl	r36");
    asm("r36:	.long	0");
    asm("	.globl	r37");
    asm("r37:	.long	0");
    asm("	.globl	r38");
    asm("r38:	.long	0");
    asm("	.globl	r39");
    asm("r39:	.long	0");
    asm("	.globl	r40");
    asm("r40:	.long	0");
    asm(".text");
#endif
}

runtime__term(n) int n; {
	runtime__countterm();
	exit(n);
}

uexit(n) int n; {
    exit(n);
}

/* SYSTEM_cputime returns the program's user time in milliseconds */
struct tms {
	int	tms_utime;		/* user time */
	int	tms_stime;		/* system time */
	int	tms_cutime;		/* user time, children */
	int	tms_cstime;		/* system time, children */
};

#ifdef vms
#define TICKS 100
#else unix
#define TICKS 60
#endif

SYSTEM_cputime(){
    struct tms ru;
    times(&ru);
    return((ru.tms_utime*1000) / TICKS);
}

/* Error routines */

#if defined(vax) && defined(unix)
#define cleanup()	_cleanup()
#else titan || vms
#define cleanup()
#endif

runtime__errorrange(val,min,max) int val, min, max; {
    fprintf(stderr,"Value %d is not in subrange [%d..%d]\n",val,min,max);
    cleanup();
    abort();
}
runtime__errorsubscript(val,min,max) int val, min, max; {
    fprintf(stderr,"Subscript %d is out of range [%d..%d]\n",val,min,max);
    cleanup();
    abort();
}
runtime__errorsubscriptopen(val,num) int val, num; {
    fprintf(stderr,"Subscript %d is out of range [0..%d]\n",val,num-1);
    cleanup();
    abort();
}
runtime__erroraddr(p) int *p; {
    fprintf(stderr,"Invalid pointer (0x%x) was dereferenced\n", p);
    cleanup();
    abort();
}
runtime__errornoreturn(){
    fprintf(stderr,"No return statement at end of function\n");
    cleanup();
    abort();
}
runtime__errorcase(){
    fprintf(stderr,"Case selector value does not match a label\n");
    cleanup();
    abort();
}
runtime__errorsubarray(i,j,n) int n, j, i; {
    fprintf(stderr,"Subarray [%d:%d] exceeds bound (%d)\n",i-j,j,n-1);
    cleanup();
    abort();
}
runtime__errorvariant(t) int t; {
    fprintf(stderr,"Improper variant tag (%d) for field\n",t);
    cleanup();
    abort();
}
runtime__errorassert(s) char *s; {
    fprintf(stderr,"Assertion failed: %s\n",s);
    cleanup();
    abort();
}

/* Set routines */
#define WORDSIZE 32
runtime__makeset(i,j,s) int i, j, *s; {
    int bit, word, mask;
    bit = i % WORDSIZE;
    word = i / WORDSIZE;
    mask = 1 << bit;
    while (i <= j) {
	s[word] |= mask;
	bit++;
	if (bit == WORDSIZE) {
	    mask = 1;
	    word++;
	    bit = 0;
	} else {
	    mask <<= 1;
	}
	i++;
    }
}
/* no longer needed..except gotta make everyone recompile from scratch to
   get references to these out of old objects. */
IO__init(){}
SYSTEM__init(){}
BITOPERATIONS__init(){}
unix__init(){}
