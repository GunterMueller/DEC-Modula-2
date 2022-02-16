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

/*
 *	Total hack version that does simple, fast, stack NEW.  NO DISPOSE!
 */

#define MINSBRK		32768		/* minimum sbrk in bytess */

static int	avail 		= 0;	/* # bytes in current chunk */
static char	*availaddress	= 0;	/* next available address */

/*
 *	ALLOCATE(p0,n) sets p0 to point to n bytes of storage
 *
 *	Address calculations are performed with char pointers.
 */

extern char *sbrk();

MemLib_ALLOCATE(p0,n) 
	int **p0; 			/* Address of pointer */
	int n;				/* Size in bytes */
{
	register int increment;
	register char *newaddress;
	register int align;
	register int size;

	size = (n+3) & ~3;		/* Round up size to word boundary */
	if (avail < size) {
		/* Damn.  Ask for enough space. */
		/* ||| First insure sbrk is word aligned */
		align = ((int) sbrk(0)) & 3;
		if (align != 0) {
			sbrk(4-align);
		}
		increment = (size < MINSBRK) ? MINSBRK : size;
		newaddress = sbrk(increment);
		if ((int) newaddress == -1) {
		    	perror("mod2.0: sbrk error");
		    	abort();
		}
		if (availaddress+avail == newaddress) {
			avail = avail + increment;
		} else {/* other allocators extended program break */
			availaddress = newaddress;
			avail = increment;
		}
	}

/* Allocate the space */
	*p0 = (int *) availaddress;
	availaddress += size;
	avail -= size;
}


/*
 *	DISPOSE(p0,n) doesn't put anything back - error
 */

MemLib_DEALLOCATE(p0,n) int **p0; int n; {

	perror("mod2.0: no dispose available error?");
	abort();
}
