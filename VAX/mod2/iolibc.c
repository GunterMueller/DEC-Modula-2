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

/*
    3/10/87	CED	Include files don't need to specify the sys/ directory
    3/19/87	CED	The CURRENTDIR constant was added
    3/27/87	CED	Made vars static so they won't get there own psects
    3/??/87	CED	Changed Open() -> Mod2Open();no conflict w/ C RTL open()
*/

#include <stdio.h>
#include <string.h>
#define MAXSEARCH 10
#define PATHNAME "MODPATH"
#define MAXLINE 1000

#ifdef vms
#define CURRENTDIR "[]"
#else
#define CURRENTDIR "."
#endif

static char *searchList[MAXSEARCH];
static int numSearchEntries = 0;
static char pathstring[MAXSEARCH*100];

char *getenv();
InitFiles() {
    char *s; int i;
    s = getenv(PATHNAME);
    if (s!=NULL) {
	while (*s != '\0') {
	    searchList[numSearchEntries] = s;
	    while (*s != '\0' && *s != ',' && *s != ' ') {
		s++;
	    }
	    if (*s!='\0') {
		*s = '\0';
		s++;
	    }
	    numSearchEntries++;
	}
    }
    searchList[numSearchEntries] = CURRENTDIR;
    numSearchEntries++;
    /*
    for (i=0;i<numSearchEntries;i++) {
	fprintf(stdout,"searchList[%d]=\"%s\"\n",i,searchList[i]);
    }
    */
}
FILE *Mod2Open(fnint,fnl) int *fnint; int fnl;{
    char name[100];
    FILE *f;
    int i;
    char *fn;
    fn = (char *) fnint;
    strncpy(name,fn,fnl);
    name[fnl] = '\0';
    if (name[0]=='/' || strchr(name,':')!=NULL) {
	f = fopen(name,"r");
	return(f);
    } else {
	for (i=0;i<numSearchEntries;i++) {
	    strcpy(name,searchList[i]);
#ifdef unix
	    strcat(name,"/");
#endif
	    strncat(name,fn,fnl);
	    f = fopen(name,"r");
	    if (f != NULL) return(f);
	}
    }
    return(NULL);
}


#ifdef vms
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif
time_t GetFileTime(f) FILE *f; {
    struct stat st;
    fstat(fileno(f),&st);
    return (st.st_mtime);
}
