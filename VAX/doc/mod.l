.TH MOD l "21 June 1990"
.SH NAME
mod \- Modula-2 compiler
.SH SYNOPSIS
.B mod
[ options ] name ...
.SH DESCRIPTION
.I Mod
is a Modula-2 compiler.
It compiles one or more Modula-2 program or implementation modules.
Definition modules are not compiled.
In the absence of options, it will compile all specified modules and link
them together into an executable file called
.I a.out.
.PP
Each program or implementation module must be in a separate file with a name
ending with \*(lq.mod\*(rq.
Each definition module must be in a separate file called \*(lqmodule.def\*(rq,
where \*(lqmodule\*(rq is the name of the module.
Object files ending with \*(lq.o\*(rq compiled with
.I mod
or some other compiler may be specified.
.PP
File name arguments ending with \*(lq.pcd\*(rq and \*(lq.s\*(rq are assumed to
be pcode and assembly language files, respectively, and are translated and
assembled into object files.
.PP
The following options are available:
.TP 6
.B \-a
Advise about identifiers that are imported into a module but not used.
.TP 6
.B \-B\fIdir\fR
Use the directory
.I dir
to look for the passes of the compiler, the supplied definition modules, and
the library archive file libmod.a.
.TP 6
.B \-C
Generate runtime checks for pointer dereferences, subrange and index bounds,
and variant record tags.
.TP 6
.B \-c
Create object files but do not link them together.
.TP 6
.BR \-D\fIname=value\fR
Defines \fIname\fR to be \fIvalue\fR for the preprocessor, as if you
used a $CONST definition.
.TP 6
.BR \-f0\fIflag\fR
Passes \fIflag\fR on to the mod2.0 front end.
.TP 6
.BR \-f1\fIflag\fR
Passes \fIflag\fR on to the mod2.1 pcode-to-assembly translator.
.TP 6
.BR \-f2\fIflag\fR
Passes \fIflag\fR on to the mod2.2 intermodule checker.
.TP 6
.BR \-fa\fIflag\fR
Passes \fIflag\fR on to the assembler.
.TP 6
.BR \-fl\fIflag\fR
Passes \fIflag\fR on to the program loader.
.TP 6
.B \-g
Generate symbol table information for the debugger
.IR dbx (1).
.TP 6
.B \-I\fIdir\fR
Adds \fIdir\fR onto the list of directories searched to find .def and .mod
files.  You can use this option more than once.
.TP 6
.B \-i
Ignore the fact that there are errors in some of the modules
and continue compiling the rest of them.
.TP 6
.B \-l\fIx\fR
Passed along to the loader, which will look for the library file
/lib/\fIx\fR.a, then /usr/lib/\fIx\fR.a, and finally /usr/local/lib/\fIx\fR.a.
The placement of the \fB\-l\fR library option is significant because a library
is searched for all currently unresolved references when its name is
encountered.  The lowest-level libraries should be listed last to avoid getting
undefined symbol errors from the linker.
.TP 6
.B \-L
While performing intermodule checking, ignore references to modules that are
not listed.  (This is useful when checking modules to be placed in a library).
.TP 6
.B \-M
Perform intermodule checking, but do not recompile if inconsistencies
are found.
.TP 6
.BR \-m\fIflags\fR
Perform intermodule checking.  If an out-of-date module is encountered,
recompile it using the specified \fIflags\fR.  The flags are separated by
commas or spaces, and must be quoted if spaces are used.
.TP 6
.B \-N \fIname\fR
While performing intermodule checking, ignore references to the module
\fIname\fR.
(This is useful when the module \fIname\fR is not a Modula-2 module.)
This option may occur multiple times.
.TP 6
.B \-n
Write out what will happen when the same command is entered
without the \*(lq-n\*(rq option.
.TP 6
.B \-O
Perform code optimizations.
.TP 6
.B \-o \fIname\fR
Create an executable file called \fIname\fR instead of the default
a.out.
.TP 6
.B \-P
Stop after generating pcode in a file ending with \*(lq.pcd\*(rq.
.TP 6
.B \-p
Set up object files for profiling by
.IR  prof (1).
.TP 6
.B \-pg
Set up object files for profiling by
.IR  gprof (1).
.TP 6
.B \-ps
Set up object files for statement counting by
.IR  modprof (1).
.TP 6
.B \-R
Perform global register allocation (Titan only).
.TP 6
.B \-r
Retain pcode and assembly language files in the current directory after
compilation.
.TP 6
.B \-S
Stop after generating assembly language in a file ending with \*(lq.s\*(rq.
.TP 6
.B \-s
Use standard conventions for reserved word case, cardinal data type, and strings
(See Extensions, below).
.TP 6
.B \-sc
Use standard conventions for cardinal data type.
.TP 6
.B \-sk
Use standard conventions for reserved word case.
.TP 6
.B \-ss
Use standard conventions for string constants.
.TP 6
.B \-u
Convert all identifiers and reserved words to upper case (i.e., ignore
the case of identifiers and reserved words on input).
.TP 6
.B \-t
Cross-compile for the Titan.
.TP 6
.B \-v
Print out messages saying what is happening during compilation.
.SH "LIBRARY MODULES"
By default, an import of a global module will cause the compiler to look for
the definition module first
in the working directory and then in the standard library directory.
The standard library modules are automatically linked with the program.
.PP
The default may be overridden to specify other directories of definition modules
using the MODPATH environment variable.
MODPATH is set to a sequence of directory names separated by colons.
Those directories will be searched in the order specified to find any definition
module.
The corresponding object files or libraries are specified when linking.
The MODPATH environment variable may be set by the user
in .login or in .modpath in the working directory.
If the file \*(lq.modpath\*(rq exists in the working directory, the 
.B mod
command will use its first line as the value of the MODPATH variable.
.PP
The following modules are provided by this implementation of Modula-2 in the
directory /usr/local/defs.  Note that SYSTEM, Memory, IO, and BitOperations are
pseudo-modules; definition modules for them are provided for documentation
purposes only.  Only the strings and parameters are actually implemented in
Modula-2; the other modules are interfaces to C routines.
.TP
SYSTEM
.br
Pseudo-module that contains types like WORD, ADDRESS, etc., and PROCESS
routines.
.TP
Memory
.br
Pseudo-module for managing storage.  Sets up pointers properly for runtime
checks.  Contains ALLOCATE and DEALLOCATE.
.TP
IO
.br
Pseudo-module for I/O, providing formatted read and write similar to
.IR scanf (3)
and
.IR printf (3).
.TP
BitOperations
.br
Pseudo-module for bit manipulations.
Performs operations like shift and exclusive or on integer operands.
.TP
ASCII
.br
All ASCII control characters, plus the type CharSet.
.TP
math
.br
Mathematical functions.
Interface to the C math library.
.TP
parameters
.br
Accesses command line parameters and environment variables.
.TP
Storage
.br
Standard storage module, for compatibility with standard Modula-2.
Contains ALLOCATE and DEALLOCATE.
.TP
strings
.br
Compares, assigns, and concatenates strings.
.TP
unix
.br
Defines some Unix system calls and C library routines.
.TP
x
.br
Defines an interface to the X11 library.
.SH "DIFFERENCES AND EXTENSIONS"
This implementation of Modula-2 has compiled and run Wirth's Modula-2 compiler
(as modified by Cambridge University for the VAX) with only minor changes to
make that compiler more portable.  However, the definition of the language has
been relaxed in some areas.  For the most part, these changes are upward
compatible.
.PP
The following is an incomplete list of differences between this compiler and
Wirth's compiler (for a complete list, see the reference manual):
.PP
Reserved words and standard identifiers are recognized in any case,
so case variations of reserved words may not be used for identifiers.
This feature is disabled by the
.B -sk
option.
.PP
Cardinal and non-negative subranges that do not exceed MAXINT are considered
to be subranges of integer and are compatible with integers.
Subranges that exceed MAXINT are compatible with cardinal and non-negative
subranges.
The data type
.I unsigned
is available for unsigned values up to the capacity of the machine word.
This feature is disabled by the
.B -sc
option.
.PP
A builtin module called
.I IO
provides formatted input and output.
The
.I ReadF
and
.I WriteF
routines can accept any number of parameters, so long as their types
correspond properly with the format string.
Supported formats include: for integer and cardinal,
.BR d ,
.BR x ,
and
.BR o ;
for real,
.B g
(output only),
.BR f ,
and
.BR e ;
for longreal,
.B G
(output only),
.BR F ,
and
.BR E ;
for char,
.BR c ;
and
for string (array of char),
.B s
and
[] (input only).
.PP
No import of
.I ALLOCATE
or
.I DEALLOCATE
is required to use NEW and DISPOSE if the standard memory allocation routines
are desired.
Programs that use ALLOCATE and DEALLOCATE and desire
checking should import ALLOCATE and DEALLOCATE
from Memory, rather than Storage.
.PP
The sizes returned by
.I SIZE
and
.I TSIZE
and expected by
.IR ALLOCATE ,
.I DEALLOCATE
and
.I NEWPROCESS
are in units of bytes.
.PP
The
.I SYSTEM
module includes the type
.IR BYTE ,
which is analogous to
.IR WORD ,
as well as appropriate related constants.
There is also a function CPUTime, which returns the accumulated program
CPU time in milliseconds.
.PP
There is a standard type called
.I LONGREAL
that stores a double precision real value.
A standard function LONGFLOAT converts cardinals, integers, or reals 
to longreal.
.PP
Additional standard procedures include:
.TP
MIN(a,b)
Returns the smaller of two cardinal, integer, real, or longreal values.
.TP
MAX(a,b)
Returns the larger of two cardinal, integer, real, or longreal values.
.TP
ASSERT(condition[,message])
Aborts the program (with the optional message)
if the condition is false.
.TP
NUMBER(a)
Returns the number of elements in the specified array.
.TP
FIRST(type)
Returns the smallest legal value of the specified type.
.TP
LAST(type)
Returns the largest legal value of the specified type.
.PP
Definition modules are not compiled.
.PP
Escape sequences may be placed in strings to specify non-printing characters.
E.g., \en, \et, \er, \ef, \eb, \e\e, \e', and \e" mean
linefeed, tab, carriage return, form feed, backspace, backslash,
single quote, and double quote, respectively.
In addition a \e followed by up to three octal digits specifies
the character whose ASCII code is the octal value.
A single (double) quote also may be put in a string delimited with
single (double) quotes by specifying two single (double) quotes.
This feature is disabled by the
.B -ss
option.
.PP
The interface to Unix is through a module called
.I unix
rather than the
.I SYSTEM
module.
.PP
Additional keywords are recognized in certain contexts.
These keywords are prefixed by @ to avoid conflicting with valid
identifiers.
.TP
Pointer attributes
Attributes may be specified between the keywords
.I POINTER
and
.I TO
or after the keyword
.I DYNARRAY
in order to change the default assumptions of Modula-2 pointer with checking.
Recognized attributes are:
.br
.ta 2i
@NOCHECK	Modula-2 pointer, no checking
.br
@C	C/malloc pointer, no checking
.br
@PASCAL	Pascal pointer, Pascal checking
.TP
Size and alignment
The size and alignment of data types may be specified preceding any
type specification.
The size and alignment multiples are in bits.
For example,
.br
    TYPE Register = @ALIGN 2 @SIZE 4 [-8..7];
.br
defines a type that occupies 4 bits aligned on a multiple of two bits.
.TP
Exports
Exports from a definition module are assumed qualified whether the export
statement says qualified or not.
.TP
External variables and procedures
A procedure or variable may be accessed by C and Pascal routines using
its unqualified name if the @EXTERNAL attribute
occurs between the keyword procedure and the name of the procedure or
precedes the variable declaration, respectively.
.TP
Uncounted open arrays
Open array parameters appear as two parameters, the address of the array and
the number of element, to non-Modula-2 programs.
If necessary, the count may be omitted by placing the attribute @NOCOUNT
between the keywords
.I ARRAY
and
.I OF
in the open array declaration.
.SH FILES
.ta 2.6i
file.mod			Program or implementation module
.br
file.def			Definition module
.br
file.pcd			Pcode (-P or -r)
.br
file.s				Assembly code (-S or -r)
.br
/usr/local/lib/mod/mod2.0	Modula-2 compiler front-end
.br
/usr/local/lib/mod/mod2.1	Modula-2 compiler back-end
.br
/usr/local/lib/mod/mod2.2	Intermodule checker
.br
/usr/local/defs/*.def		Standard definition modules
.br
/usr/local/lib/libmod.a		Default library
.br
/tmp/modNNNNNN.pcd		Temporary Pcode file
.br
/tmp/modNNNNNN.s		Temporary assembly code file
.SH "SEE ALSO"
N. Wirth,
.IR "Programming in Modula-2" ,
Springer-Verlag, New York, 1982.
.PP
.IR "The WRL Modula-2 User's Manual"
.SH DIAGNOSTICS
All error messages suppress subsequent compilation phases.
Error messages ending with a question mark are internal errors, and
probably represent compiler bugs.
When pointer checking is enabled in a running Modula-2 program,
segmentation faults may be generated by the pointer validation test.
These are intentional and should be considered as invalid pointer messages.  
The compiler runs with runtime checks enabled, and may produce core dumps.
Report problems to the author.
.SH AUTHORS
Original compiler by Michael L. Powell, while working at DEC's Western Research
Laboratory.  Improvements and bug fixes by Joel McCormack.  Port to VMS by
Chuck Doucette.  Port to MIPS by Len Lattanzi.
.PP
Software and documentation is
Copyright (c) 1984-1990, Digital Equipment Corporation,
Maynard, Massachusetts.
All rights reserved.
This software is provided under a license agreement.
.SH LIMITATIONS
No warranties are expressed or implied about the compiler's conformance to the
definition of the Modula-2 language or about its proper functioning.  We will
endeavor to report and fix bugs, but users should be aware that this compiler
is not a supported product.
