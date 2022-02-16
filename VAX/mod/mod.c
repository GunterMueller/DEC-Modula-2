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

/* Most (if not all) of the following comments apply to the VMS version. */
/* Note : you can't invoke mod2.2 (imc) on VMS */

/*
     3/10/87	CED	Changed all symbol names that were differentiated
			only by case (i.e. Pflag -> P_flag)
     3/11/87	CED	Changed MODPATHFILE from ".modpath" -> "modpath."
     3/11/87	CED	Added the constants TMP and RM for scratch files
     3/11/87	CED	unlink -> delete, rindex -> strrchr, fork -> vfork
     3/18/87	CED	Added DELIMITER code (make MODPATH delimiter ':' or ',')
     3/24/88	CED	These comments were updated and cleaned up.
     3/24/88	CED	unlink() became a macro for VAX C -> delete()
     3/24/88	CED	rindex() replaced with strrchr() unconditionally
     3/24/88	CED	set mflag (=1) for -L & -N (imc options) to run imc |||
     3/24/88	CED	nomoreopt() isn't called consistently |||
     3/24/88	CED	Shouldn't -w (no warnings) be added to modopt? |||
     3/24/88	CED	ldoptv (linker options) are now added to vmscmdline
     3/24/88	CED	asoptv (assembler options) are now added to vmscmdline
     3/24/88	CED	-D now an option for the (mod2) pre-processor
     7/06/88	CED	DEFSDIR is now always concatenated to modpath (end)
*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef vms
#define EXITSUCCESS 1
#define EXITFAILURE 0
#define CURRENTDIR  "[]"
#define DIRSEPCH    ']'		/* file name comes after this */
#define DEVSEP	    ":"		/* directory ([...]) or file comes after this */
#define DEVSEPCH    ':'
#define TMP	    "sys$scratch:mod"
#define RM	    "delete %s;0\n"
#define PHASE0	    "mod2"
#define PHASE1	    "xlate"
#define PHASE2	    "imc"
#define MODPATHFILE "modpath."	/* name of file to source for MODPATH */
#define remove unsave		/* Because remove() is in VAX C RTL v2.3 */
#define unlink delete		/* To keep conditional compilation here */
#else
#define EXITSUCCESS 0
#define EXITFAILURE 1
#define CURRENTDIR  "."
#define DIRSEPCH    '/'		/* file name comes after this */
#define DEVSEP	    "/"		/* directory ([...]) or file comes after this */
#define DEVSEPCH    '/'
#define TMP	    "/tmp/mod"
#define RM	    "rm %s\n"
#define PHASE0	    "mod2.0"
#define PHASE1	    "mod2.1"
#define PHASE2	    "mod2.2"
#define MODPATHFILE ".modpath"	/* name of file to source for MODPATH */
#endif

/* String Constants */
#define MODPATH	"MODPATH="	/* environment variable */

/* Numeric Constants */
#define ARGSIZE		2048
#define FILENAMESIZE	256
#define MAXENVIRONNAMES	1024
#define MAXARGS		1024


/* Flags based on command line options (initialized to defaults)	*/
static int cflag = 0;		/* don't invoke linker (just Compile)   */
static int gflag = 0;		/* Dump symbol table for debugger       */
static int G_flag = 0;		/* Weird debug information for Titan    */
static int iflag = 0;		/* Ignore errors in recompiling		*/
static int mflag = 0;		/* Flags to use on recompilations of imc */
static int nflag = 0;		/* No action, just go thru the motions  */
int Oflag = 0;			/* Optimize code			*/
static int pflag = 0;		/* Generate profile info */
static int P_flag = 0;		/* stop after .Pcd created */
static int pgflag = 0;		/* Generate profile info (for gprof) */
static int rflag = 0;		/* retain .pcd and .s files */
static int Sflag = 0;		/* stop after .S created */
int Rflag = 0;			/* Mahler register allocation		*/
#ifdef vax
static int tflag = 0;		/* Titan Mahler code generation	 */
#else
static int tflag = 1;		/* Multititan Mahler code generation    */
#endif
static int tmflag = 0;
static int vflag = 0;		/* Verbose compile */

/* Other flags */
static int somefiles = 0;
static int didsomething = 0;

/* phase parameters for execve */
static char *mod2optv[MAXARGS] = {0}, **mod2opt = mod2optv;
static char *xlateoptv[MAXARGS] = {0}, **xlateopt = xlateoptv;
static char *asoptv[MAXARGS] = {0}, **asopt = asoptv;
static char *imcoptv[MAXARGS] = {0}, **imcopt = imcoptv;
static char *ldlistv[MAXARGS] = {0}, **ldlist = ldlistv;
static char *ldoptv[MAXARGS] = {0}, **ldopt = ldoptv;


/* MODPATH values */
static char modname[ARGSIZE] = "";
static char modpath[ARGSIZE] = "";
static char modpathI[ARGSIZE] = "";
static char modpathtail[ARGSIZE] = "";

/* File-name processing variables */
static char execfileb[FILENAMESIZE] = "", *execfile = execfileb;
static char filename[FILENAMESIZE];
static char filenamehead[FILENAMESIZE];
static char filenametail[FILENAMESIZE];
static char mfilename[FILENAMESIZE];
static char pfilename[FILENAMESIZE];
static char sfilename[FILENAMESIZE];
static char ofilename[FILENAMESIZE];
static char lfilename[FILENAMESIZE];
static char ifilename[FILENAMESIZE];

/* Misc. vars */
static char *commandv[MAXARGS], **command;
static char progname[FILENAMESIZE];
static char *myname;
static char moddir[FILENAMESIZE] = LIBDIR;
static char *modpassesdir = PASSESDIR;
static char *newenv[MAXENVIRONNAMES];
static enum filekind
	{pasfile, modfile, deffile, pcdfile, asmfile, objfile, libfile};
static enum filekind kind;
static struct stat lastlibstat, libstat;
static lastlibstatvalid = 0;

#if vms
static char vmscmdline[ARGSIZE];
#include <perror.h>
#else
extern char *sys_errlist[];
#endif

/* Function definitions (return value isn't int) */
char *malloc(), *mktemp();
extern char *sys_errlist[];

/* Add one string argument to the arg vector */
char **addto(l,s) char **l, *s; {
    *l++ = s;
    return(l);
}
char *newname(s) char *s; {
     char *p;
     p = malloc(FILENAMESIZE);
     strcpy(p,s);
     return(p);
}
outfile(saveit,filename,root,ext) int saveit; char *filename, *root, *ext; {
     if (saveit) {
	strcpy(filename,root);
     } else {
	strcpy(filename,TMP);
	strcat(filename,"XXXXXX");
	(void) mktemp(filename);
     }
    strcat(filename,ext);
}

/* Put program name in argv^[0], start params at argv^[1] */
initexec(progname) char *progname; {
    command = commandv;
    *command++ = progname;
}
/* Add a single string argument to command */
addarg(arg) char *arg; {
    if (arg == 0 || *arg == '\0') {
	fprintf(stderr,"%s: addarg: null string\n",myname);
    } else {
	*command++ = arg;
    }
}
/* Add a list of arguments to command */
addargv(argv) char *argv[]; {
    while (*argv != 0) {
	*command++ = *argv++;
    }
}
execute(progfile) char *progfile; {
    int pid, status, i;
    *command++ = 0;
    didsomething = 1;
    if (vflag || nflag) {
	fprintf(stderr,"%s: ",progfile);
	for (i=0;commandv[i]!=0;i++) {
	    fprintf(stderr,"%s ",commandv[i]);
	}
	fprintf(stderr,"\n");
    }
    status = EXITSUCCESS;
    if (!nflag) {
	if ((pid=vfork())==0) { /* Parent case - replace self with progfile */
	    execve(progfile,commandv,newenv);
	    fprintf(stderr,"%s: Cannot execute %s (%s)\n",myname,progfile,
		sys_errlist[errno]);
	    exit(EXITFAILURE); /* ||| Maybe _exit on Ultrix? */
	}
	/* Child case - wait for forked process */
	while(wait(&status)!=pid);
	if (vflag) {
	    fprintf(stderr,"%s: %s exited, status=%d\n",myname,progfile,status);
	}
    }
    if (!iflag && status != EXITSUCCESS) {
    /* This definition of a severe exit status is probably only true on unix. */
	if ((status & 0xff)== 0) {
	    status = status >> 8;
	} else {
	    fprintf(stderr,"%s: %s exited, status=%d\n",myname,progfile,status);
	}
    }
    return(status);
}
remove(file) char *file; {
    if (vflag || nflag) {
	fprintf(stderr,RM,file);
    }
    if (!nflag) {
	if ((unlink(file)!=0) && (errno!=ENOENT)) {
	    fprintf(stderr,"%s: Cannot remove %s (%s)\n",myname,file,
		sys_errlist[errno]);
	}
    }
}
/* Insure there isn't garbage at the end of an option */
nomoreopt(o,argv) char *o, *argv; {
    if (*o != '\0') {
	fprintf(stderr,"%s: %s not understood in option %s\n",myname,o,argv);
	exit(EXITFAILURE);
    }
}

main(argc,argv,environ) int argc; char **argv; char **environ;
{
    char *ext, **env, scanfbuff[ARGSIZE];
    int i, status;

    FILE *modpathfile;
#ifdef unix
    myname = argv[0];
#else
    /* This is a *major* kludge; but, because execve in VAX C RTL on VMS      */
    /* closes stdout and stderr (default in RMS is no sharing), this must be. */
    char const1a[FILENAMESIZE] = "MOD$OUTPUT=";
    char *const1b = "SYS$OUTPUT";
    char const2a[FILENAMESIZE] = "MOD$ERROR=";
    char *const2b = "SYS$ERROR";
    myname = "mod";
#endif

    env = environ;
    newenv[0] = modpath;
    if (argc <= 1) {
#if MODULA2
	fprintf(stderr,"usage: %s [options] files.mod\n",myname);
#else /* PASCAL */
	fprintf(stderr,"usage: %s [options] files.p\n",myname);
#endif
	exit(EXITFAILURE);
    }
#ifdef vms    
    strcat(const1a,getenv(const1b));
    newenv[1] = const1a;
    strcat(const2a,getenv(const2b));
    newenv[2] = const2a;
    i = 3;
#else
    i = 1;
#endif
    /* Copy info from MODPATH environment variable into modpathtail */
    while (*env!=NULL) {
	if (strncmp(*env,MODPATH,sizeof(MODPATH)-1)==0) {
#if MODULA2
	    strcpy(modpathtail, ",");
	    strcat(modpathtail,(*env)+sizeof(MODPATH)-1);
#endif
	} else {
	    newenv[i] = *env;
	    i++;
	}
	env++;
    }
    newenv[i] = NULL;
    
#if MODULA2
    /* Look for modpath file in the local directory */
    modpathfile = fopen(MODPATHFILE,"r");
    if (modpathfile != NULL) {
	/* Replace any MODPATH info with info from local modpath file */
	if (fscanf(modpathfile," %s ",scanfbuff)!=1) {
	    fprintf(stderr,"Bad %s file\n", MODPATHFILE);
	    exit(EXITFAILURE);
	}
	strcpy(modpathtail, ",");
	strcat(modpathtail,scanfbuff);
	while (fscanf(modpathfile," %s ",scanfbuff) == 1) {
	    strcat(modpathtail, ",");
	    strcat(modpathtail, scanfbuff);
	}
    }
#endif

    if (tflag) {
    	mod2opt = addto(mod2opt, "-tb");
	ldopt = addto(ldopt,"-b");
    }

    /* Parse arguments to compiler */
    for(i=1;i<argc;i++) {
	if (argv[i][0]=='\0') {
	    /* skip null argument */
	} else if (argv[i][0]=='-') {
	    switch (argv[i][1]) {

	    case 'a':			/* Advise unused idents, etc. */
		mod2opt = addto(mod2opt,argv[i]);
		break;

	    case 'B':			/* Directory for passes, runtime lib */
		modpassesdir = &argv[i][2];
		strcpy(moddir, modpassesdir);
		strcat(modpathI,",");
		strcat(modpathI, modpassesdir);
		break;

	    case 'c':			/* Don't invoke linker */
		cflag = 1;
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 'f':			/* generalized argument to any pass */
		if (argv[i][2]=='0') {
		    mod2opt = addto(mod2opt,&argv[i][3]);
		} else if (argv[i][2]=='1') {
		    xlateopt = addto(xlateopt,&argv[i][3]);
		} else if (argv[i][2]=='2') {
		    imcopt = addto(imcopt,&argv[i][3]);
		} else if (argv[i][2]=='a') {
		    asopt = addto(asopt,&argv[i][3]);
		} else if (argv[i][2]=='l') {
		    ldopt = addto(ldopt,&argv[i][3]);
		} else {
		    fprintf(stderr,"mod: bad compiler phase on -f\n");
		}
		break;

	    case 'G':			/* Runtime debug info on Titan */
		G_flag = 1;
		mod2opt = addto(mod2opt,"-G");
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 'g':			/* Dump symbol table for debugger */
		gflag = 1;
		mod2opt = addto(mod2opt,"-g");
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 'I':			/* Include named dir in search list */
		strcat(modpathI,",");
		if (argv[i][2]=='\0') {
		    strcat(modpathI,CURRENTDIR);
		} else {
		    strcat(modpathI,&argv[i][2]);
		}
		break;

	    case 'i':			/* Ignore errors in recompiling */
		iflag = 1;
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 'l':			/* Arguments for linker */
		ldlist = addto(ldlist,argv[i]);
		break;

#if MODULA2
	    case 'L':			/* Intermodule check libraries, too */
		imcopt = addto(imcopt,"-L");
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 'm':			/* Flags to use on recompilations
	    				   dictated by intermodule checker */
		mflag = 1;
		imcopt = addto(imcopt,argv[i]);
		break;

	    case 'M':			/* Do intermodule checking, but don't
	    				   recompile anything */
		mflag = 1;
		break;

	    case 'N':			/* Ignore refs to following module
	    				   in the intermodule checker */
		imcopt = addto(imcopt,argv[i]);
		break;
#endif

	    case 'n':			/* No action, just go thru the
					   motions */
		nflag = 1;
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 'O':			/* Optimize code */
		Oflag = 1;
		nomoreopt(&argv[i][2],argv[i]);
		mod2opt = addto(mod2opt,"-O");
		break;

	    case 'o':			/* Output file name */
		nomoreopt(&argv[i][2],argv[i]);
		if (i+1>=argc) {
		    fprintf(stderr,"%s: Missing file for -o\n",myname);
		    exit(EXITFAILURE);
		}
		ext = strrchr(argv[i+1],'.');
		if (ext!=NULL) {
		    if (strcmp(ext,".mod")==0) {
			ext = NULL;
		    } else if (strcmp(ext,".def")==0) {
			ext = NULL;
		    } else if (strcmp(ext,".pcd")==0) {
			ext = NULL;
		    } else if (strcmp(ext,".i")==0) {
			ext = NULL;
		    } else if ((ext[1]=='p'||ext[1]=='c'||ext[1]=='o'||
			    ext[1]=='s'|| ext[1]=='f') && ext[2]=='\0') {
			ext = NULL;
		    }
		    if (ext==NULL) {
			fprintf(stderr,
				"%s: -o would overwrite source file %s\n",
				myname,argv[i+1]);
			exit(EXITFAILURE);
		    }
		}
		strcpy(execfile,argv[i+1]);
		i = i+1;
		break;

	    case 'P':			/* Stop after .pcd created */
		P_flag = 1;
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 'p':			/* Generate profile info */
		if (argv[i][2] == 'g') {	/* For gprof */
		    nomoreopt(&argv[i][3],argv[i]);
		    mod2opt = addto(mod2opt,argv[i]);
		    pgflag = 1;
		} else if (argv[i][2] == 's') {	/* Just statement counts */
		    nomoreopt(&argv[i][3],argv[i]);
		    mod2opt = addto(mod2opt,argv[i]);
		} else {			/* For prof */
		    nomoreopt(&argv[i][2],argv[i]);
#if titan
		    mod2opt = addto(mod2opt, "-pg");
#endif
		    pflag = 1;
		}
		break;

	    case 'r':			/* Retain .pcd and .s (or .i) files */
		rflag = 1;
		nomoreopt(&argv[i][2],argv[i]);
		break;

#ifdef titan
	    case 'R':			/* Mahler register allocation */
		Rflag = 1;
		ldopt = addto(ldopt,argv[i]);
		break;
#endif

	    case 'S':			/* Stop after .s created */
		Sflag = 1;
		nomoreopt(&argv[i][2],argv[i]);
		break;

	    case 's':			/* "Standard" Modula-2 flags */
#if MODULA2
		if (argv[i][2]=='c'||argv[i][2]=='k'||argv[i][2]=='s') {
		    nomoreopt(&argv[i][3],argv[i]);
		} else {
		    nomoreopt(&argv[i][2],argv[i]);
		}
#else
		nomoreopt(&argv[i][2],argv[i]);
#endif
		mod2opt = addto(mod2opt,argv[i]);
		imcopt = addto(imcopt,argv[i]);
		break;

	    case 't':			/* Compile for Titan */
		if (!tflag) {
		    tflag = 1;
		}
		if (argv[i][2] == 'b') {
		    /* Same as -t, kept for compatibility */
		    nomoreopt(&argv[i][3],argv[i]);
		} else if (argv[i][2] == 'm') {
		    tmflag = 1;
		    nomoreopt(&argv[i][3],argv[i]);
		} else {
		    nomoreopt(&argv[i][2],argv[i]);
		}
		mod2opt = addto(mod2opt,argv[i]);
#ifdef vax
		if (tmflag) {
		    strcat(moddir, "m");
		} else {
		    strcat(moddir, "t");
		}
#endif
		break;

#if MODULA2
	    case 'u':			/* Map all to uppercase */
		mod2opt = addto(mod2opt,"-u");
		nomoreopt(&argv[i][2],argv[i]);
		break;
#endif

	    case 'v':			/* Verbose compile */
		vflag = 1;
		mod2opt = addto(mod2opt,argv[i]);
		imcopt = addto(imcopt,argv[i]);
		break;

	    case 'W':			/* imc: Watch module */
		imcopt = addto(imcopt,argv[i]);
		break;

	    case 'X':			/* imc: Examine module */
	    	imcopt = addto(imcopt,argv[i]);
		break;

	    case 'y':			/* mod2: compile def modules */
	    	mod2opt = addto(mod2opt,argv[i]);
		nomoreopt(&argv[i][2],argv[i]);
		break;
#if PASCAL
	    case 'w':
	    	nomoreopt(&argv[i][2],argv[i]);
		break;
#endif

	    default:			/* Otherwise just pass to mod2.0 */
		mod2opt = addto(mod2opt,argv[i]);
		break;
	    }
	} else {
	    status = processfile(argv[i]);
	    if (status != EXITSUCCESS && !iflag) exit(status);
	}
    }

    if (!P_flag && !Sflag) {
	strcpy(lfilename,moddir);
#ifdef unix
	strcat(lfilename,DEVSEP);   /* VMS names must have separator already */
#endif
	strcat(lfilename,"libmod");
	if (pgflag) {
	    strcat(lfilename,"_p");
	}
        strcat(lfilename,".a");
#ifdef vms
	strcat(lfilename,"/lib,");  /* VMS LINK object-library qualifier */
#endif
	fstat(lfilename, &libstat);
	if (lastlibstatvalid && lastlibstat.st_dev == libstat.st_dev
		&& lastlibstat.st_ino == libstat.st_ino) {
	    /* Duplicate of file already listed, so just toss it on the floor */
	    lfilename[0] = 0;
	} else {
	    somefiles = 1;
	}
	lastlibstat = libstat;
	lastlibstatvalid = 1;
	    
#if MODULA2
	if (mflag && somefiles) {
	    initexec(PHASE2);
	    addargv(imcoptv);
	    addargv(ldlistv);
	    if (lfilename[0] != 0) {
		addarg(lfilename);      /* Add in standard libmod.a file */
	    }
	    strcpy(modname, "-B");  /* Tell imc what name used to get mod */
	    strcat(modname, argv[0]);
	    addarg(modname);
	    strcpy(progname,modpassesdir);
#ifdef unix
	    strcat(progname,DEVSEP); /* VMS name must have separator already */
#endif
	    strcat(progname,PHASE2);
	    status = execute(progname);
	    if (status != EXITSUCCESS) exit(status);
	}
#endif
	if (!cflag && somefiles) {
	    if (tflag) {
		initexec("xld");
		addargv(ldoptv);
		addarg("-o");
		if (execfile[0]=='\0') {
		    addarg("a.out");
		} else {
		    addarg(execfile);
		}
#ifdef vax
		if (tmflag) {
		    addarg("/mt/lib/startup.o");
		} else if (pgflag) {
		    addarg("/tunix/libb/startup_p.o");
		} else {
		    addarg("/tunix/libb/startup.o");
		}
#else
		if (tmflag) {
		    addarg("/mt/lib/startup.o");
		} else if (pgflag) {
		    addarg("/lib/startup_p.o");
		} else {
		    addarg("/lib/startup.o");
		}
#endif
		addargv(ldlistv);
/* ||| change when get profiled versions of everything */
#ifdef vax
		if (tmflag) {
		    addarg("/mt/lib/libmod.a");
		    addarg("/mt/lib/libpc.a");
		    addarg("/mt/lib/libm.a");
		    addarg("/mt/lib/libc.a");
		} else if (pgflag) {
		    addarg("-lmod_p");
		    addarg("-lpc_p");
		    addarg("-lm_p");
		    addarg("-lc_p");
		} else if (tflag) {
		    addarg("/tunix/libb/libmod.a");
		    addarg("/tunix/libb/libpc.a");
		    addarg("/tunix/libb/libm.a");
		    addarg("/tunix/libb/libc.a");
		}
#else
		if (tmflag) {
		    addarg("/mt/lib/libmod.a");
		    addarg("/mt/lib/libpc.a");
		    addarg("/mt/lib/libm.a");
		    addarg("/mt/lib/libc.a");
		} else if (pgflag) {
		    addarg("-lmod_p");
		    addarg("-lpc_p");
		    addarg("-lm_p");
		    addarg("-lc_p");
		} else {
		    addarg("-lmod");
		    addarg("-lpc");
		    addarg("-lm");
		    addarg("-lc");
		}
#endif
		if (gflag) {
		    addarg("-lg");
		}
		status = execute("/usr/local/bin/xld");
		if (status != EXITSUCCESS) exit(status);
	    } else {
#ifdef unix
		initexec("ld");
		addargv(ldoptv);
		addarg("-X");
		addarg("-o");
		if (execfile[0]=='\0') {
		    addarg("a.out");
		} else {
		    addarg(execfile);
		}
		if (pflag) {
		    addarg("/lib/mcrt0.o");
		} else if (pgflag) {
		    addarg("/usr/lib/gcrt0.o");
		} else {
		    addarg("/lib/crt0.o");
		}
		addargv(ldlistv);
		if (lfilename[0] != 0) {
		    addarg(lfilename);
		}
		if (pgflag) {
		    addarg("-lpc_p");
		    addarg("-lm_p");
		    addarg("-lc_p");
		} else {
		    addarg("-lpc");
		    addarg("-lm");
		    addarg("-lc");
		}
		if (gflag) {
		    addarg("-lg");
		}
		status = execute("/bin/ld");
#else vms
		strcpy(vmscmdline,"link");
		ldopt = ldoptv;
		while (*ldopt != 0) {
		    strcat(vmscmdline,*ldopt++);
		    }
		strcat(vmscmdline,"/exe=");
		if (execfile[0]=='\0') {
		    strcat(vmscmdline,"a.out");
		} else {
		    strcat(vmscmdline,execfile);
		}
		strcat(vmscmdline," ");
		ldlist = ldlistv;
		while (*ldlist != 0) {
		    strcat(vmscmdline,*ldlist++);
		    strcat(vmscmdline,",");
		}
		if (lfilename[0] != 0) {
		    strcat(vmscmdline,lfilename);
		}
		strcat(vmscmdline,"sys$library:vaxcrtl/lib");
		if (vflag || nflag) fprintf(stderr,"%s\n",vmscmdline);
		status = EXITSUCCESS;
		if (!nflag) {
		    status = system(vmscmdline);
		    didsomething = 1;
		}
#endif
		if (status != EXITSUCCESS) exit(status);
	    }
	}
    }
    if (!didsomething) {
	fprintf(stderr,"mod: no action was specified so nothing was done\n");
	exit(EXITSUCCESS);
	
    }
    exit(EXITSUCCESS);
}
int processfile(file) char *file; {
    char *phase, *ext, *pcdext, *asmext, *objext, *tail, *pasext;
    int status;

    if (tflag) {
	pcdext = ".i";
	asmext = ".i";
	objext = ".o";
    } else {
	pcdext = ".pcd";
#ifdef vms
	pasext = ".pas";
	asmext = ".mar";
	objext = ".obj";
#else unix
	pasext = ".p";
	asmext = ".s";
	objext = ".o";
#endif
    }
    tail = strrchr(file,DIRSEPCH);
#ifdef vms
    if (tail==NULL) {
	tail = strrchr(file,DEVSEPCH); /* On VMS "logical$name:filename.ext" */
    }
#endif
    if (tail==NULL) {
	/* File is in current directory */
	strcpy(filenametail,file);
	strcpy(filenamehead,CURRENTDIR);
    } else {
	/* File is in another directory.  Add that directory, then current
	   directory, to search list */
	strcpy(filenametail,tail+1);
	strncpy(filenamehead,file,tail-file+1);
	filenamehead[tail-file] = '\0';
	strcpy(filenamehead, ",");
	strcpy(filenamehead, CURRENTDIR);
    }
    strcpy(filename,file);
    ext = strrchr(filename,'.');
    if (ext == NULL) kind = libfile;
    else if (strcmp(ext,objext)==0) kind = objfile;
#if MODULA2
    else if (strcmp(ext,".mod")==0) kind = modfile;
    else if (strcmp(ext,".def")==0) kind = deffile;
#else
    else if (strcmp(ext,pasext)==0) kind = pasfile;
#endif
    else if (strcmp(ext,pcdext)==0) kind = pcdfile;
    else if (strcmp(ext,asmext)==0) kind = asmfile;
    else kind = libfile;
    
    if (kind != libfile) {
	*ext = '\0';
	ext = strrchr(filenametail,'.');
	*ext = '\0';
    }
    switch(kind) {
#if MODULA2
    case modfile:
	strcpy(mfilename,filename);
	strcat(mfilename,".mod");
	outfile(P_flag||rflag||(tflag&&Sflag),pfilename,filenametail,pcdext);
	outfile(Sflag||rflag,sfilename,filenametail,asmext);
	outfile(1,ofilename,filenametail,objext);
	break;
#else
    case pasfile:
    	strcpy(mfilename,filename);
	strcat(mfilename,pasext);
	outfile(P_flag||rflag||(tflag&&Sflag),pfilename,filenametail,pcdext);
	outfile(Sflag||rflag,sfilename,filenametail,asmext);
	outfile(1,ofilename,filenametail,objext);
	break;
#endif
    case pcdfile:
	outfile(1,pfilename,filenametail,pcdext);
	outfile(Sflag||rflag,sfilename,filenametail,asmext);
	outfile(1,ofilename,filenametail,objext);
	break;
    case asmfile:
	outfile(1,sfilename,filenametail,asmext);
	outfile(1,ofilename,filenametail,objext);
	break;
    case objfile:
	outfile(1,ofilename,filename,objext);
	break;
    case deffile:
	fprintf(stderr,"%s.def: definition modules are not compiled\n",
				filename);
    }
    strcpy(modpath,MODPATH);
    strcat(modpath,filenamehead);
    strcat(modpath,modpathI);   /* Contains leading "," if non-null */
    strcat(modpath,modpathtail);/* So does this */
#if MODULA2
    /* Add the directory that defs are kept in */
    strcat(modpath,",");
    strcat(modpath,DEFSDIR);
    /* ||| And for now add the passes directory as well, until we make sure
       we've replaced all the old .def files lying around in
       /usr/local/lib/mod */
    strcat(modpath,",");
    strcat(modpath,moddir);
#endif
    switch (kind) {
    case modfile:
    case pasfile:
	if (vflag||nflag) {
	    fprintf(stderr,"%s\n",modpath);
	}
#if MODULA2
#ifdef vax
	if (tflag) {
	    phase = "mod2.0t";
	} else {
	    phase = PHASE0;
	}
#else
	phase = PHASE0;
#endif
#else
#ifdef vax
	if (tflag) {
	    phase = "pas2.0t";
	} else {
	    phase = "pas2.0";
	}
#else
	phase = "pas2.0";
#endif
#endif
	initexec(phase);
	addargv(mod2optv);
	addarg("-o");
	addarg(pfilename);
	addarg(mfilename);
	strcpy(progname,modpassesdir);
#ifdef unix
	strcat(progname,DEVSEP);
#endif
	strcat(progname,phase);
	status = execute(progname);
	if (P_flag || (tflag && Sflag)) return(status);
	if (status != EXITSUCCESS) {
	    if (!rflag) {
		remove(pfilename);
	    }
	    return(status);
	}
    /* fall through to next phase */
    case pcdfile:
	if (!tflag) {
	    initexec(PHASE1);
	    addargv(xlateoptv);
	    addarg(pfilename);
	    addarg(sfilename);
	    strcpy(progname,modpassesdir);
#ifdef unix
	    strcat(progname,DEVSEP);
#endif
	    strcat(progname,PHASE1);
	    status = execute(progname);

	    if ((!rflag) && (kind != pcdfile)) {
		remove(pfilename);
	    }
	}
	if (Sflag) return(status);
	if (status != EXITSUCCESS) {
	    if (!rflag) {
	    	remove(sfilename);
	    }
	    return(status);
	}
    /* fall through to next phase */
    case asmfile:
	if (tflag) {
	    if (tmflag) {
	    	initexec("mmc");
	    } else {
	        initexec("mc");
	    }
	    addargv(asoptv);
	    if (Oflag) {
		addarg("+O4");
	    }
	    addarg(sfilename);
	    addarg(ofilename);
	    if (tmflag) {
	    	status = execute("/usr/local/bin/mmc");
	    } else {
	    	status = execute("/usr/local/bin/mc");
	    }
	} else {
#ifdef vms
	    strcpy(vmscmdline,"macro/deb");
	    asopt = asoptv;
	    while (*asopt != 0) {
		strcat(vmscmdline,*asopt++);
		}
	    strcat(vmscmdline,"/obj=");
	    strcat(vmscmdline,ofilename);
	    strcat(vmscmdline," ");
	    strcat(vmscmdline,sfilename);
	    if (vflag || nflag) fprintf(stderr,"%s\n",vmscmdline);
	    status = EXITSUCCESS;
	    if (!nflag) {
		status = system(vmscmdline);
		didsomething = 1;
	    }
#else unix
	    initexec("as");
	    addargv(asoptv);
	    addarg("-o");
	    addarg(ofilename);
	    addarg(sfilename);
	    status = execute("/bin/as");
#endif
	}
	if ((!rflag) && (kind != asmfile)) {
	    remove(sfilename);
	}
	if (status != EXITSUCCESS) return(status);
    /* fall through to next phase */
    case objfile:
	ldlist = addto(ldlist,newname(ofilename));
	somefiles = 1;
	break;
    case libfile:
	fstat(ldlist, &libstat);
	if (lastlibstatvalid && lastlibstat.st_dev == libstat.st_dev
		&& lastlibstat.st_ino == libstat.st_ino) {
	    /* Duplicate file listed, so just toss it on the floor */
	} else {
	    ldlist = addto(ldlist,file);
	    somefiles = 1;
	}
	lastlibstat = libstat;
	lastlibstatvalid = 1;
	break;
    }

    /* Made it through a successful compile of one module */
    return(EXITSUCCESS);
}
