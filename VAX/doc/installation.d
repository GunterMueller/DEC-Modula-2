.po 1i
.nr PO 1i
.if n .ds lq ""
.if n .ds rq ""
.if t .ds lq ``
.if t .ds rq ''
.DA "21 June, 1990"
.TL
Installing the DECWRL Modula-2 Compiler
.br
\*(DY
.NH 1
OVERVIEW
.PP
The DECWRL Modula-2 compiler consists of three compilation phases, an
intermodule checker, and a command to invoke the various phases.  The first
phase compiles a single Modula-2 module into P-code, a machine-independent
intermediate language.  The second phase translates P-code into VAX assembly
language.  The third phase is the Unix assembler, which translates the assembly
code into an object file.
.PP
Before a collection of object modules are linked, you can optionally invoke the
intermodule checker to detect any inconsistencies between modules.  You can
instruct the intermodule checker automatically recompile any modules that are
out of date.  The Unix linker is then used to combine the object modules into
an executable file.
.FS
DEC and VAX are trademarks of Digital Equipment Corporation.
.br
Unix is a trademark of Bell Laboratories.
.FE
.PP
The DECWRL Modula-2 compiler is distributed on a 9-track, 1600 bpi
tape readable by the Unix
.I tar
command.  For the purposes of this document, Unix will mean 4.3 BSD or some
recent version of Ultrix.  It may or may not run on your version of Unix.  The
main things that the compiler depends on are: itself, to compile most of the
sources; the YACC parser generator, which is used to generate parse tables for
the compiler; and the Unix C compiler, which is used to compile some of the
library routines and the
.I mod
command.
The runtime library calls some C routines, but they are standard across most
implementations.
.PP
NOTE ABOUT LICENSES:
The distribution contains no software proprietary
to Bell Laboratories or the Regents of the University of California.
The compiler was written from scratch and is not derived from Wirth's Lilith
compiler.
Software and documentation is copyright 1984-1990
by Digital Equipment Corporation,
Maynard, Massachusetts, and is made available subject to a license agreement.
.NH 1
INSTALLING THE COMPILER
.PP
The compiler is organized as one top-level directory, with
several subdirectories.  Since all directory paths used to create the compiler
are self-relative, you can create the top-level directory at
any convenient place in your source hierarchy.  Sources and object files require
about 30 megabytes.  Once you have created this top-level directory, copy and
unpack the tape by typing \*(lqtar xf /dev/rmt0l\*(rq.
.PP
Each subdirectory contains one or more components.  The top-level Makefile can
be used to clean, make, or install all components at one time: type \*(lqmake
clean\*(rq, \*(lqmake\*(rq, or \*(lqmake install\*(rq.  You can also install
the compiler without all dependencies being satisfied by typing \*l(qmake
quickinstall\*(rq; this is useful for installing the distributed compiler as is
without actually recompiling all the source files.

The top-level Makefile defines the directory names DESTDIR, LIBDIR, PASSESDIR,
DEFSDIR, MANDIR, and BINDIR, and the binary executable names MODBINNAME,
PCBINNAME, P2M2BINNAME and MODPROFBINNAME; you may want to change some or all
of these names to match local naming conventions.
.PP
DESTDIR is the root of the installation tree, and is originally defined as 
empty.  DESTDIR is prefixed to each of the other directory names defined.  Some
people like to define DESTDIR as /tmp/ the first time through, just to see that
everything happens correctly.  DESTDIR is the only directory name in which you
include a trailing /.
.PP
LIBDIR is the full pathname of the directory in which the Modula-2 runtime
library 
.I libmod.a
is installed, and is originally defined as /usr/local/lib.  This directory name
is incorporated into the
.I mod
executable, but you can specify a specific library archive file using the
\*(lq-l\Ilibname\*(rq switch.
.PP
PASSESDIR is the full pathname of the directory in which the
various passes of the compiler are installed, and is originally defined as
/usr/local/lib/mod.  This directory name is incorporated into the
.I mod
executable, but can be overridden using the \*(lq-B\*(rq switch.
.PP
DEFSDIR is the full pathname of the directory in which definition modules
are installed, and is originally defined as /usr/local/defs.
This directory name is incorporated into the
.I mod
executable, but can be supplemented using the \*(lq-I\*(rq switch.
.PP
MANDIR is the full pathname of the directory in which the
.I man
pages for
.I mod
and
.I p2m2
are installed, and is originally defined as /usr/man/manl.
.PP
BINDIR is the full pathname of the directory in which the
.I mod
and
.I p2m2
executables are installed, and is originally defined as /usr/local/bin.
.PP
MODBINNAME is the name of the executable that invokes the various
phases of the Modula-2 compiler, and is originally defined as mod.
.PP
PCBINNAME is the name of the executable that invokes the various phases
of the Pascal compiler, and is originally defined as wrlpc.
.PP
P2M2BINNAME is the name of the Pascal to Modula-2 converter executable, and is
originally defined as p2m2.
.PP
MODPROFNAME is the name of the Modula-2 statement-level profiler, and is
originally defined as modprof.
.PP
The top-level Makefile passes all these definitions down to the component
Makefiles.  If you want to make components one at a time, you should make sure
that any changes to these directory names are incorporated into each component
Makefile.  There is no automatic way of doing this right now; volunteers are
solicited to convert everything to Imake or something similar.
.PP
Note that in the mod/Makefile, imod is the driver for the installed version of
the compiler.  Just plain mod is the driver for testing purposes.  This
executable specifies
.I lib
as the directory containing the definitions and the passes of the compiler.  It
contains self-relative symbolic links to the executables in the 
.I mod,
.I mod2, 
.I xlate, 
and
.I imc
directories.
You must assign the full path name of the lib source directory
to the TESTLIBDIR variable in the 
.I mod
Makefile, then type \*(lqmake mod\*(rq.
Executing 
.I mod
will use the latest versions
of the binaries and libraries.  Note that all the Makefiles use the testing
.I mod
to compile Modula-2 files.
.PP
The components of the compiler should be installed in the order
in which they are described below.  The top-level Makefile observes this
order.
.PP
NOTE ABOUT UNIX SYSTEM COMPATIBILITY:
Although the sources distributed can probably be compiled and run on any
4.x BSD or Ultrix system,
there is some chance the object modules on the tape will not work.
If you have problems linking the programs (unresolved references),
or errors that might be attributed to I/O problems (mysterious core dumps
before the compiler does anything, the compiler runs but produces no output,
etc.), these may result from changes to the C stdio library.
I/O and memory allocation have been left in C to ease
bootstrapping.  If you run into problems, try removing
any object files created by
compiling C sources by typing \*(lqmake cclean\*(rq
and try again to make the compiler.
.NH 1
MOD AND WRLPC COMMANDS
.PP
The mod command is a C program that invokes the phases of the Modula-2 compiler.
The wrlpc command is a C program that invokes the phases of the Pascal compiler.
The source is in a directory called
.I mod.
The same source file is conditionally compiled to create both the Modula-2 and
Pascal versions, as well as testing and installed versions of each.  To create
the installed versions, you should type \*(lqmake imod iwrlpc\*(rq and then
\*(lqmake install\*(rq; this will install mod as MODBINNAME and wrlpc as
PCBINNAME in the BINDIR directory.
.PP
The testing version of the Modula-2 compiler is called just plain
.I mod
and the testing verion of the Pascal compiler is called
.I wrlpc.
.NH 1
FRONT ENDS
.PP
The compiler front end is a YACC, C, and Modula-2 program that parses
individual Modula-2 and Pascal programs and generates P-code for them.  The
same source files (except for the YACC grammar) are conditionally compiled to
create both the Modula-2 and Pascal front ends.  An optimizer may be selected
to improve the speed of the generated code--this optimizer works best on code
that has runtime checks enabled.
.PP
The source for the Modula-2 front end is in the directory
.I mod2.
Typing \*(lqmake installmod2.0\*(rq will install the Modula-2 compiler front
end as
.I mod2.0
in the PASSESDIR directory.
.PP
The source for the Pascal front end is in the directory
.I pascal.
All files in this directory except the Makefile are symbolic links to the
.I mod2
directory.  Typing \*(lqmake installpas2.0\*(rq will install the Pascal
compiler front end as
.I mod2.0
in the PASSESDIR directory.
.PP
Note that even the huge definitions for the standard YACC table sizes may be
too small to compile the Modula-2 and Pascal grammars.  I use the following
constants in the YACC source file
.I dextern:
.br
#define NSTATES 1200
.br
#define LSETSIZE 1000
.NH 1
BACK END
.PP
The compiler back end is a Modula-2 program that parses P-code files and
generates VAX/Unix assembly language.
The source for the back end is in the directory
.I xlate.
Typing \*(lqmake install\*(rq will install the compiler back end as
.I mod2.1
in the PASSESDIR directory.
.NH 1
LIBRARY
.PP
The runtime library is a set of C and Modula-2 programs that provide a minimal
runtime environment for Modula-2.  Only two of the modules (strings and
parameters) actually exist as Modula-2 implementations.  Most of the others
have Modula-2 definition modules that are merely interfaces to the C
implementation.  The SYSTEM.def, Memory.def, IO.def, and BitOperations.def
modules document the pseudo-modules that are specially parsed by the compiler.
The Storage.def module is provided for programs that import the standard memory
allocation routines ALLOCATE and DEALLOCATE, although such imports are not
needed to use NEW and DISPOSE unless the \*(lqstandard Modula-2\*(rq switch -s
is used.
.PP
The unix.def module defines many common Unix system calls in a way that
Modula-2 programs can use, and x.def provides an interface to the X11 library.
Feel free to add your favorites, as long as you send back your additions.
.PP
The file runtime.c contains much of the miscellaneous runtime support.
Included is a procedure called SYSTEM_cputime that returns the accumulated
CPU time for a program in milliseconds.
It may be necessary to modify this on your system.
The file mem.c contains the memory allocation routines, coroutine.c contains
the Modula-2 process routines, and udiv.c contains an unsigned 32-bit
division routine.
.PP
Typing \*(lqmake install\*(rq will create the library and install it in 
the LIBDIR directory.
Two versions of the library are created: one for use in profiling
Modula-2 programs (-pg option) and the other for normal use.
.NH 1
INTERMODULE CHECKER
.PP
The intermodule checker is a Modula-2 program that examines the symbol table
information in a set of Modula-2 object files to be sure they are consistent.
(The intermodule checker does not check any other object files, not even those
generated by the Pascal compiler.)
It will optionally recompile any files that are out of date.
.PP
The intermodule checker uses information similar to that used by
.I dbx.
The information is placed into the object modules with type
80 decimal (= 50 hexadecimal).
It is possible that this number is in use by another language processor,
in which case an alternate should be selected
(probably 16 plus some multiple of 32).
To specify a different value, change the constant STABNMOD2 in
mod2/SymbolDump.mod and in imc/stab.def and add the constant N_MOD2 to
/usr/include/stab.h.
For cleanliness, you should also modify the Unix
.I nm
command to recognize Modula-2 entries.
If /usr/include/stab.h does not define N_MOD2, dbx will assume 80 decimal.
.PP
The source for the intermodule checker is in directory
.I imc.
Typing \*(lqmake install\*(rq will install the intermodule checker as
.I mod2.2
in the PASSESDIR directory.
.NH 1
P2M2
.PP
.I p2m2
is a conversion aid for translating Berkeley Pascal programs into Modula-2.
It consists of a YACC parser and some Pascal routines
to read .h and .p files and produce .def and .mod files.
The source is in a directory called p2m2.
.PP
Typing \*(lqmake install\*(rq will install p2m2 in the BINDIR directory.
.NH 1
DBX
.PP
We use the debugger
.I dbx
for Modula-2.
Mark Linton of Stanford University developed the debugger, and has extended
it to work for Modula-2.
Since that software is covered by a 4.x BSD license,
it is not a part of this software distribution.
However, we are distributing a set of updates to the 4.2 version
of DBX; these updates were developed by Mark Linton and are in the public
domain.
.PP
The updates to make DBX are in the directory called
.I dbx.
To generate a DBX that will work with Modula-2, copy the source files for the
4.2 DBX into the appropriate directory.
You should check that the sizes and version numbers of your source files agree
with the sizes at the beginning of the file mkupdate.
If they do not agree, then you do not have a clean 4.2 version of DBX,
and you will have to integrate the changes by hand (Good luck!).
If they agree, execute the csh command file
.I mkupdate.
.PP
The csh command file mktests will create a set of test directories for dbx.
You should then be able to create a new dbx by typing \*(lqmake install\*(rq.
Dbx includes a set of regression tests which can be run before installing an
updated version.
To run these tests, type \*(lqmake test\*(rq.
.I ManPage
contains a new manual page for dbx.
.NH 1
DOCUMENTATION
.PP
For a description of the standard Modula-2 language, see
N. Wirth,
.I "Programming in Modula-2"
Springer-Verlag, New York, 1982.
.PP
The file
.I m2.psf
in the
.I refman
directory contains a complete reference manual for the DECWRL
Modula-2 language, including all extensions.  This document is in PostScript
format.
.PP
This installation guide plus manual pages for the
.I mod,
.I p2m2,
and
.I modprof
commands are in a directory call 
.I doc.
This directory also includes an overview of the compiler,
and a document discussing interfacing Modula-2 to C and Pascal programs.
.NH 1
BENCHMARKS
.PP
The source for the benchmark program in various languages is supplied
in a directory called bench.
Compile and enjoy.  Feel free to put your favorite compiler to the test.
.PP
Experience has shown that the Modula-2 optimizer does not really do
as well on real programs as it does on benchmarks.  However, it does a
pretty good job of getting rid of redundant runtime checks.
.NH 1
TESTS
.PP
Some test programs are included in a directory called test.
The Makefile has some comments about what they should do.
.NH 1
BUGS
.PP
I'm not in the business of immediately fixing bugs, especially those require
restructuring, but I do want to know about them.  Your chances of getting
a bug fixed are inversely proportional to the size of the test program
demonstrating such bug.  Much better than bug reports, of course, are bug fixes.
.PP
Comments on the compiler and distribution will be appreciated.
.PP
Mail to:
.nf
.in +1i
Joel McCormack
Digital Equipment Corporation
Western Research Laboratory
100 Hamilton Avenue
Palo Alto, CA  94301

decwrl!modula-2
modula-2@decwrl.dec.com
decwrl::modula-2
.in -1i
.fi
