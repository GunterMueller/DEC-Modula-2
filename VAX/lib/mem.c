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
 *	Next-fit storage allocation mechanism
 *
 *	Algorithm and variable names stolen from Knuth V1, p 437,
 *		with suggested modifications from Exercise 2.5.6.
 *
 *	Storage is manipulated in terms of UNITs, the amount needed
 *		to store a pointer and length.  Requests are always
 *		rounded up to multiples of UNITs.
 */

#ifdef vms
globaldef char *_maxptr = (char *)0;
globaldef char *_minptr = (char *)0x7fffffff;
#endif

#define ELEMENT		struct element
#define MINSBRK		4096		/* minimum sbrk = 4096 UNITs */
#define	UNIT		sizeof(ELEMENT)
#define BYTESPERWORD	4

ELEMENT {
	ELEMENT *link;
	int size;
};

/*
 *	free list is kept in address order
 *	avail is a dummy header that points to first free element
 *	rover is pointer into free list
 */

static ELEMENT avail;
static ELEMENT *rover = &avail;
/*
#define	NUMBINS	100
static ELEMENT *free[NUMBINS];
*/

/*
 *	Storage_ALLOCATE(p0,n) sets p0 to point to n WORDS of storage
 */

Storage_ALLOCATE(p0,n) ELEMENT **p0; int n; {
	register ELEMENT *p, *q;
	register int size;
	ELEMENT *r;
	int increment;
	int repeat;
	if (n <= 0) {
	    fprintf(stderr,"Storage_ALLOCATE: non-positive size\n");
	    abort();
	}
	size = (n + UNIT - 1) / UNIT;
/*
	if ((size < NUMBINS) && free[size]) {
		*p0 = free[size];
		free[size] = free[size]->link;
		return;
	}
*/
	if (rover == NULL) rover = &avail;
	q = rover;
	p = q->link;
	/* outer loop executed at most twice */
	repeat = 0;
	do {
		/* search for a block large enough */
		while ((p != NULL) && (p->size < size)) {
			/* keep looking */
			q = p;
			p = p->link;
		}
		if ((p == NULL) && !repeat) {
			/* if first time, one more chance */
			q = &avail;
			p = q->link;
			repeat = 1;
		} else {
			repeat = 0;
		}
	} while (repeat);
	if (p == NULL) {
		/* out of memory, get some more */
		increment = (size<MINSBRK) ? MINSBRK : size;
		r = (ELEMENT *)sbrk(increment*UNIT);
		if ((int)r == -1) {
		    perror("Storage_ALLOCATE: sbrk error");
		    abort();
		}
		/* release it */
		Storage_DEALLOCATE(&r,increment*UNIT);
		/* call self recursively to allocate memory */
		Storage_ALLOCATE(p0,n);
	} else {
		if (p->size == size) {
			/* found one of right size. remove it */
			q->link = p->link;
		} else if (p->size > size) {
			/* found one too big. take part of it */
			r = p + size;	/* remaining free area */
			q->link = r;
			r->link = p->link;
			r->size = p->size - size;
		}
		rover = q->link;
		*p0 = p;
	}
}

/*
 *	Storage_DEALLOCATE(p0,n) adds the block of n bits pointed to by p0
 *		 to the free list
 */

Storage_DEALLOCATE(p0,n) ELEMENT **p0; int n; {
	register ELEMENT *p, *q, *r;
	register int size;

	if (n <= 0) {
	    fprintf(stderr,"Storage_DEALLOCATE: non-positive size\n");
	    abort();
	}
	size = (n + UNIT - 1) / UNIT;
	r = *p0;
/*
	if (size < NUMBINS) {
		r->link = free[size];
		free[size] = r;
		*p0 = (ELEMENT *)0;
		return;
	}
*/
	q = &avail;
	p = q->link;
	/* search for the right place */
	while ((p != NULL) && (p < r)) {
		/* not the place, keep searching */
		q = p;
		p = p->link;
	}
	/* this is where it should go */
	/* note: since NULL = 0, if p = NULL, p != r + size */
	if (p == r + size) {
		/* new block abuts p, consolidate */
		size += p->size;
		r->link = p->link;
	} else {
		/* does not abut, just connect */
		r->link = p;
	}
	if (r == q + q->size) {
		/* new block abuts q, consolidate */
		q->size += size;
		q->link = r->link;
	} else {
		/* does not abut, just connect */
		q->link = r;
		r->size = size;
	}
	rover = q;	/* start searching here next time */
	*p0 = (ELEMENT *)0;
}
Storage__init(){}

/* interface to C allocation routine */
modmalloc(p,s) char **p; int s; {
    *p = (char *)malloc(s);
}
MEMORY__init(){}
MEMORY_ALLOCATE(p0,n) ELEMENT **p0; int n; {
	ELEMENT *p;
	Storage_ALLOCATE(&p,n+BYTESPERWORD);
	*p0 = (ELEMENT*)(((int)p)+BYTESPERWORD);
	p->link = *p0;
}
MEMORY_DEALLOCATE(p0,n) ELEMENT **p0; int n; {
	ELEMENT *p;
	p = (ELEMENT*)(((int)*p0)-BYTESPERWORD);
	if (*p0 != p->link) {
	    fprintf(stderr,"MEMORY_DEALLOCATE: Invalid pointer specified\n");
	    abort();
	}
	Storage_DEALLOCATE(&p,n+BYTESPERWORD);
	*p0 = 0;
}
