.po 1i
.nr PO 1i
.DA "28 June 1990"
.SH
COMPILER ANNOUNCEMENT 28 June 1990
.LP
The Western Research Laboratory of Digital Equipment Corporation is pleased to
announce the availability of a new version of their Modula-2 compiler for VAXes
running Ultrix or BSD.  (The compiler has also been ported to the MIPS and to
VMS; these versions should be available ``soon.'')
.LP
The compiler was designed and built by Michael L. Powell, and originally
released in 1984.  Joel McCormack has since sped the compiler up, fixed lots of
bugs, and stolen/written a User's Manual.  The compiler appears to be one of
the best compilers for the VAX in terms of efficiency of the generated code.
The error messages are quite a bit better than those from the Ultrix C compiler
and the compile time is about the same as cc and gcc.  Further, the compiler
includes an intermodule checker that uses fine-grained dependencies rather than
timestamps, so you can modify definition modules in upward-compatible ways
without recompiling the world.
.LP
The compiler includes several extensions.  The most notable are: provisions for
interfacing with procedures and data types written in other languages like C
and Pascal (a definition module for the X11 C library is included); dynamic
array variables, subarray parameters, and multi-dimensional open array
parameters; inline procedures; complete support for the LONGFLOAT type; and a
pseudo-module providing a type-checked interface to the C library I/O routines.
.LP
In addition, the compiler front end can be conditionally compiled to generate a
Pascal compiler that accepts a language almost identical to Berkeley Pascal.
(Procedure parameters are not supported, and record layout and parameter
passing conventions are somewhat different.)
.LP
The distribution also includes a program that does most of the translation of
Berkeley Pascal programs into Modula-2.
.LP
The compiler is available for anonymous ftp in gatekeeper.pa.dec.com, in the
compressed tar file /pub/DEC/m2.vax.tar.Z.  Once you have copied this file, you
can uncompress it and untar it, like:
.DS L
	uncompress m2.vax.tar.Z
	tar xf m2.vax.tar
.DE
From there, read the README file, and the doc/installation.f file.
.LP
The compiler is available to DEC sites over DECNET at
decwrl::/pub/DEC/m2.vax.tar.Z.
.LP
The compiler is also available by magnetic tape.  Send a check made
out to Digital Equipment Corporation for $100 US to:
.DS L
	Director of Licensing
	Digital Equipment Corporation
	Western Research Laboratory
	100 Hamilton Avenue
	Palo Alto, CA  94301
.DE
.LP
Unlike the previous license, the new license does not restrict use of the
compiler to educational institutions.  The license agreement is as follows:
.DS L
------------------------------------------------------------
     Copyright 1984-1990 Digital Equipment Corporation
                    All Rights Reserved
.DE
Permission to use, copy, and modify this software and its documentation
is hereby granted only under the following terms and conditions.  Both
the above copyright notice and this permission notice must appear in all
copies of the software, derivative works or modified versions, and any
portions thereof, and both notices must appear in supporting
documentation.
.LP
Users of this software agree to the terms and conditions set forth
herein, and hereby grant back to Digital a non-exclusive, unrestricted,
royalty-free right and license under any changes, enhancements or
extensions made to the core functions of the software, including but not
limited to those affording compatibility with other hardware or software
environments, but excluding applications which incorporate this software.
Users further agree to use their best efforts to return to Digital any
such changes, enhancements or extensions that they make and inform
Digital of noteworthy uses of this software.  Correspondence should be
provided to Digital at:
.DS L
	Director of Licensing
        Western Research Laboratory
        Digital Equipment Corporation
        100 Hamilton Avenue
        Palo Alto, California  94301
.DE
This software may be distributed (but not offered for sale or transferred
for compensation) to third parties, provided such third parties agree to
abide by the terms and conditions of this notice.
.LP
THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL
EQUIPMENT CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
.DS L
------------------------------------------------------------
.DE
.DS L
If you have any questions, you can send mail to
	decwrl!modula-2
or
	modula-2@decwrl.pa.dec.com
or
	decwrl::modula-2
.DE
.LP
This compiler has a number of internal and external users and we feel it is
quite reliable.  However, new users often find new bugs so we expect to hear
from some of those who try it.  No promises about bug fixes or new releases,
though.
