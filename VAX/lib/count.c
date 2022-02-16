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
#define COUNTFILE "modmon.out"
struct Counters {
    struct Counters *next;
    char *moduleName;
    int numCounters;
    int *countArray;
    int *lineArray;
};
static int countFile = -1;
static char countFileName[40];
static struct Counters *head = 0;
static int countInitialized = 0;

runtime__countlist(counters) struct Counters *counters; {
    runtime__countinit();
    counters->next = head;
    head = counters;
}

runtime__countinit(){
    if (countInitialized) return;
    countInitialized = 1;
    countFile = creat(COUNTFILE,0666);
    if (countFile < 0) {
	perror(COUNTFILE);
	sprintf(countFileName,"/tmp/modmon.out.%d",getpid());
	countFile = creat(countFileName,0666);
	if (countFile < 0) {
	    fprintf(stderr,"Cannot create file for statement counts\n");
	    return;
	} else {
	    fprintf(stderr,"Using %s for statement counts\n",countFileName);
	}
    }
}

runtime__countterm() {
    struct Counters *counters;
    int namelen;
    if (!head || countFile < 0) return;
    counters = head;
    while (counters != 0) {
	namelen = strlen(counters->moduleName) + 1;
	write(countFile,&namelen,4);
	write(countFile,&counters->numCounters,4);
	write(countFile,counters->moduleName,namelen);
	write(countFile,&counters->countArray[1],counters->numCounters*4);
	write(countFile,&counters->lineArray[1],counters->numCounters*4);
	counters = counters->next;
    }
    namelen = 0;
    write(countFile,&namelen,4);
    close(countFile);
}
