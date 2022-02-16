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
static int argc;
static char **argv;
static int inputfile;
#define BUFFSIZE 1024

#ifdef unix
#define globalref extern
#else vms
globalref FILE *IO_TERMINAL;
#endif

globalref FILE *outputfile;
globalref int numchread, chptr;
globalref char buff[BUFFSIZE], endOfFile;
globalref char nextchar;
globalref char ECHOPCODE, VERBOSE, PRINTNAMES;


main(argc0,argv0,envp0) int argc0; char **argv0; char **envp0; {
    argc = argc0;
    argv = argv0;
    runtime__init(argc0,argv0,envp0);
#ifdef vms
    stderr = freopen(getenv("MOD$ERROR"),"w",stderr);
    IO_TERMINAL = stderr;
#endif
    Input__init();
    Error__init();
    EES__init();
    CodeSubs__init();
    OpSubs__init();
    xlate();
}


setoptions() {
    int i, nexti, j, k;
    int endofoptions;

    ECHOPCODE = 0;
    VERBOSE = 0;
    PRINTNAMES = 0;

    i = 1;
    endofoptions = 0;
    while (i < argc && !endofoptions) {
	if (argv[i][0] == '-' && argv[i][1] != '\0') {
	    j = 1;
	    while (argv[i][j] != '\0') {
		if (argv[i][j] == 'e') {
		    ECHOPCODE = 1;
		} else if (argv[i][j] == 'v') {
		    VERBOSE = 1;
		} else if (argv[i][j] == 'p') {
		    PRINTNAMES = 1;
		} else {
		    fprintf(stderr,"invalid option: %c\n",argv[i][j]);
		    exit(1);
		}
		j = j + 1;
	    }
	    i = i + 1;
	} else {
	    endofoptions = 1;
	}
    }
    if (argc > i) {
	/* use specified file */
	inputfile = open(argv[i],0);
	if (inputfile < 0) {
	    perror(argv[i]);
	    exit(1);
	}
	i = i + 1;
    } else {
	inputfile = 0;
    }
    RefillBuffer();
    if (endOfFile) {
	fprintf(stderr,"Input file empty\n");
	exit(1);
    }
    nextchar = buff[chptr];
    chptr = chptr + 1;
    if (argc > i) {
	outputfile = fopen(argv[i],"w");
	if (outputfile == NULL) {
	    perror(argv[i]);
	    exit(1);
	}
#ifdef vms
	/* These were stolen from GPH - CED 3/21/87. */
	/* They are macros to translate ULTRIX -> VMS assembler (jumps). */
	fprintf (outputfile, ".MACRO	JUMP	J,L\n");
	fprintf (outputfile, ".ENABLE LOCAL_BLOCK\n");
	fprintf (outputfile, "	J	1$\n");
	fprintf (outputfile, "	BRW	L\n");
	fprintf (outputfile, "1$:\n");
	fprintf (outputfile, ".DISABLE LOCAL_BLOCK\n");
	fprintf (outputfile, ".ENDM	JUMP\n");
	fprintf (outputfile, "\n");
	fprintf (outputfile, ".MACRO	JBR	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BRB	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	BRW	L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	BRW	L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JBR\n");
	fprintf (outputfile, "\n");
	fprintf (outputfile, ".MACRO	JLEQ	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BLEQ	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGTR,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGTR,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JLEQ\n");
	fprintf (outputfile, ".MACRO	JLSS	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BLSS	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGEQ,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGEQ,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JLSS\n");
	fprintf (outputfile, ".MACRO	JGEQ	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BGEQ	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLSS,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLSS,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JGEQ\n");
	fprintf (outputfile, ".MACRO	JGTR	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BGTR	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLEQ,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLEQ,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JGTR\n");
	fprintf (outputfile, ".MACRO	JEQL	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BEQL	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BNEQ,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BNEQ,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JEQL\n");
	fprintf (outputfile, ".MACRO	JNEQ	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BNEQ	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BEQL,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BEQL,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JNEQ\n");
	fprintf (outputfile, "\n");
	fprintf (outputfile, "\n");
	fprintf (outputfile, ".MACRO	JLEQU	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BLEQU	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGTRU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGTRU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JLEQU\n");
	fprintf (outputfile, ".MACRO	JLSSU	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BLSSU	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGEQU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BGEQU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JLSSU\n");
	fprintf (outputfile, ".MACRO	JGEQU	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BGEQU	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLSSU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLSSU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JGEQU\n");
	fprintf (outputfile, ".MACRO	JGTRU	L\n");
	fprintf (outputfile, ".IF	DEFINED	L\n");
	fprintf (outputfile, ".IF	LT	.-L-127\n");
	fprintf (outputfile, "	BGTRU	L\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLEQU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".IFF\n");
	fprintf (outputfile, "	JUMP	BLEQU,L\n");
	fprintf (outputfile, ".ENDC\n");
	fprintf (outputfile, ".ENDM	JGTRU\n");
	fprintf (outputfile, "\n");
	fprintf (outputfile, "\n");
	fprintf (outputfile, ".SHOW	ME,MEB,MC,CND,MD\n");
#endif
    } else {
	outputfile = stdout;
    }
}
RefillBuffer() {
    numchread = read(inputfile,buff,BUFFSIZE);
    chptr = 0;
    if (numchread <= 0) {
	buff[1] = '?';
	if (endOfFile) {
	    fprintf(stdout,"*** Tried to read past end of file\n");
	    exit(1);
	} else {
	    endOfFile = 1;
	}
    }
}


ExitProgram(n) int n; {
#ifdef unix
    exit(n);
#else vms
    if (n == 0) {
	exit(1);
    } else {
	exit(2);
    }
#endif
}
