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
#define MAXSEARCH 10
#define PATHNAME "MODPATH"
#define MAXLINE 1024
char *searchList[MAXSEARCH];
int numSearchEntries = 0;
char pathstring[MAXSEARCH*100];
char *getenv();
InitFiles() {
    char *s; int i;
    s = getenv(PATHNAME);
    if (s!=NULL) {
	while (*s != '\0') {
	    searchList[numSearchEntries] = s;
	    while (*s != '\0' && *s != ':') s++;
	    if (*s!='\0') {
		*s = '\0';
		s++;
	    }
	    numSearchEntries++;
	}
    }
    searchList[numSearchEntries] = ".";
    numSearchEntries++;
/***fprintf(stderr,"Search list\n");
    for (i=0;i<numSearchEntries;i++) {
	fprintf(stderr,"%s\n",searchList[i]);
    }
***/
}
FILE *GetInput() {
    return(stdin);
}
FILE *Open(fn,fnl) char *fn; int fnl;{
    char name[100];
    FILE *f;
    int i;
    
    for (i=0;i<numSearchEntries;i++) {
	strcpy(name,searchList[i]);
	strcat(name,"/");
	strncat(name,fn,fnl);
	f = fopen(name,"r");
	if (f != NULL) return(f);
    }
    return(NULL);
}
Close(f) FILE *f; {
    fclose(f);
}
ReadLine(f,b) FILE *f; char *b; {
    register int i;
    if (fgets(b,MAXLINE,f)) {
	i = 0;
	while (b[i]!='\0') i++;
    } else {
	i = -1;
    }
    return(i);
}
