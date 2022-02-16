
NAME
     mod - Modula-2 compiler

SYNOPSIS
     mod [ options ] name ...

DESCRIPTION
     _M_o_d is a Modula-2 compiler.  It compiles one or more
     Modula-2 program or implementation modules.  Definition
     modules are not compiled.  In the absence of options, it
     will compile all specified modules and link them together
     into an executable file called _a._o_u_t.

     Each program or implementation module must be in a separate
     file with a name ending with ".mod".  Each definition module
     must be in a separate file called "module.def", where
     "module" is the name of the module.  Object files ending
     with ".o" compiled with _m_o_d or some other compiler may be
     specified.

     File name arguments ending with ".pcd" and ".s" are assumed
     to be pcode and assembly language files, respectively, and
     are translated and assembled into object files.

     The following options are available:

     -a    Advise about identifiers that are imported into a
           module but not used.

     -B_d_i_r Use the directory _d_i_r to look for the passes of the
           compiler, the supplied definition modules, and the
           library archive file libmod.a.

     -C    Generate runtime checks for pointer dereferences,
           subrange and index bounds, and variant record tags.

     -c    Create object files but do not link them together.

     -D_n_a_m_e=_v_a_l_u_e
           Defines _n_a_m_e to be _v_a_l_u_e for the preprocessor, as if
           you used a $CONST definition.

     -f0_f_l_a_g
           Passes _f_l_a_g on to the mod2.0 front end.

     -f1_f_l_a_g
           Passes _f_l_a_g on to the mod2.1 pcode-to-assembly trans-
           lator.

     -f2_f_l_a_g
           Passes _f_l_a_g on to the mod2.2 intermodule checker.

     -fa_f_l_a_g
           Passes _f_l_a_g on to the assembler.

     -fl_f_l_a_g
           Passes _f_l_a_g on to the program loader.

     -g    Generate symbol table information for the debugger
           _d_b_x(1).

     -I_d_i_r Adds _d_i_r onto the list of directories searched to find
           .def and .mod files.  You can use this option more
           than once.

     -i    Ignore the fact that there are errors in some of the
           modules and continue compiling the rest of them.

     -l_x   Passed along to the loader, which will look for the
           library file /lib/_x.a, then /usr/lib/_x.a, and finally
           /usr/local/lib/_x.a.  The placement of the -l library
           option is significant because a library is searched
           for all currently unresolved references when its name
           is encountered.  The lowest-level libraries should be
           listed last to avoid getting undefined symbol errors
           from the linker.

     -L    While performing intermodule checking, ignore refer-
           ences to modules that are not listed.  (This is useful
           when checking modules to be placed in a library).

     -M    Perform intermodule checking, but do not recompile if
           inconsistencies are found.

     -m_f_l_a_g_s
           Perform intermodule checking.  If an out-of-date
           module is encountered, recompile it using the speci-
           fied _f_l_a_g_s.  The flags are separated by commas or
           spaces, and must be quoted if spaces are used.

     -N _n_a_m_e
           While performing intermodule checking, ignore refer-
           ences to the module _n_a_m_e.  (This is useful when the
           module _n_a_m_e is not a Modula-2 module.) This option may
           occur multiple times.

     -n    Write out what will happen when the same command is
           entered without the "-n" option.

     -O    Perform code optimizations.

     -o _n_a_m_e
           Create an executable file called _n_a_m_e instead of the
           default a.out.

     -P    Stop after generating pcode in a file ending with
           ".pcd".

     -p    Set up object files for profiling by _p_r_o_f(1).

     -pg   Set up object files for profiling by _g_p_r_o_f(1).

     -ps   Set up object files for statement counting by _m_o_d_-
           _p_r_o_f(1).

     -R    Perform global register allocation (Titan only).

     -r    Retain pcode and assembly language files in the
           current directory after compilation.

     -S    Stop after generating assembly language in a file end-
           ing with ".s".

     -s    Use standard conventions for reserved word case,
           cardinal data type, and strings (See Extensions,
           below).

     -sc   Use standard conventions for cardinal data type.

     -sk   Use standard conventions for reserved word case.

     -ss   Use standard conventions for string constants.

     -u    Convert all identifiers and reserved words to upper
           case (i.e., ignore the case of identifiers and
           reserved words on input).

     -t    Cross-compile for the Titan.

     -v    Print out messages saying what is happening during
           compilation.

LIBRARY MODULES
     By default, an import of a global module will cause the com-
     piler to look for the definition module first in the working
     directory and then in the standard library directory.  The
     standard library modules are automatically linked with the
     program.

     The default may be overridden to specify other directories
     of definition modules using the MODPATH environment vari-
     able.  MODPATH is set to a sequence of directory names
     separated by colons.  Those directories will be searched in
     the order specified to find any definition module.  The
     corresponding object files or libraries are specified when
     linking.  The MODPATH environment variable may be set by the
     user in .login or in .modpath in the working directory.  If
     the file ".modpath" exists in the working directory, the mod
     command will use its first line as the value of the MODPATH
     variable.

     The following modules are provided by this implementation of
     Modula-2 in the directory /usr/local/defs.  Note that SYS-
     TEM, Memory, IO, and BitOperations are pseudo-modules;
     definition modules for them are provided for documentation
     purposes only.  Only the strings and parameters are actually
     implemented in Modula-2; the other modules are interfaces to
     C routines.

     SYSTEM
          Pseudo-module that contains types like WORD, ADDRESS,
          etc., and PROCESS routines.

     Memory
          Pseudo-module for managing storage.  Sets up pointers
          properly for runtime checks.  Contains ALLOCATE and
          DEALLOCATE.

     IO
          Pseudo-module for I/O, providing formatted read and
          write similar to _s_c_a_n_f(3) and _p_r_i_n_t_f(3).

     BitOperations
          Pseudo-module for bit manipulations.  Performs opera-
          tions like shift and exclusive or on integer operands.

     ASCII
          All ASCII control characters, plus the type CharSet.

     math
          Mathematical functions.  Interface to the C math
          library.

     parameters
          Accesses command line parameters and environment vari-
          ables.

     Storage
          Standard storage module, for compatibility with stan-
          dard Modula-2.  Contains ALLOCATE and DEALLOCATE.

     strings
          Compares, assigns, and concatenates strings.

     unix
          Defines some Unix system calls and C library routines.

     x
          Defines an interface to the X11 library.

DIFFERENCES AND EXTENSIONS
     This implementation of Modula-2 has compiled and run Wirth's
     Modula-2 compiler (as modified by Cambridge University for
     the VAX) with only minor changes to make that compiler more
     portable.  However, the definition of the language has been
     relaxed in some areas.  For the most part, these changes are
     upward compatible.

     The following is an incomplete list of differences between
     this compiler and Wirth's compiler (for a complete list, see
     the reference manual):

     Reserved words and standard identifiers are recognized in
     any case, so case variations of reserved words may not be
     used for identifiers.  This feature is disabled by the -sk
     option.

     Cardinal and non-negative subranges that do not exceed MAX-
     INT are considered to be subranges of integer and are compa-
     tible with integers.  Subranges that exceed MAXINT are com-
     patible with cardinal and non-negative subranges.  The data
     type _u_n_s_i_g_n_e_d is available for unsigned values up to the
     capacity of the machine word.  This feature is disabled by
     the -sc option.

     A builtin module called _I_O provides formatted input and out-
     put.  The _R_e_a_d_F and _W_r_i_t_e_F routines can accept any number of
     parameters, so long as their types correspond properly with
     the format string.  Supported formats include: for integer
     and cardinal, d, x, and o; for real, g (output only), f, and
     e; for longreal, G (output only), F, and E; for char, c; and
     for string (array of char), s and [] (input only).

     No import of _A_L_L_O_C_A_T_E or _D_E_A_L_L_O_C_A_T_E is required to use NEW
     and DISPOSE if the standard memory allocation routines are
     desired.  Programs that use ALLOCATE and DEALLOCATE and
     desire checking should import ALLOCATE and DEALLOCATE from
     Memory, rather than Storage.

     The sizes returned by _S_I_Z_E and _T_S_I_Z_E and expected by _A_L_L_O_-
     _C_A_T_E, _D_E_A_L_L_O_C_A_T_E and _N_E_W_P_R_O_C_E_S_S are in units of bytes.

     The _S_Y_S_T_E_M module includes the type _B_Y_T_E, which is analogous
     to _W_O_R_D, as well as appropriate related constants.  There is
     also a function CPUTime, which returns the accumulated pro-
     gram CPU time in milliseconds.

     There is a standard type called _L_O_N_G_R_E_A_L that stores a dou-
     ble precision real value.  A standard function LONGFLOAT
     converts cardinals, integers, or reals to longreal.

     Additional standard procedures include:

     MIN(a,b)
          Returns the smaller of two cardinal, integer, real, or
          longreal values.

     MAX(a,b)
          Returns the larger of two cardinal, integer, real, or
          longreal values.

     ASSERT(condition[,message])
          Aborts the program (with the optional message) if the
          condition is false.

     NUMBER(a)
          Returns the number of elements in the specified array.

     FIRST(type)
          Returns the smallest legal value of the specified type.

     LAST(type)
          Returns the largest legal value of the specified type.

     Definition modules are not compiled.

     Escape sequences may be placed in strings to specify non-
     printing characters.  E.g., \n, \t, \r, \f, \b, \\, \', and
     \" mean linefeed, tab, carriage return, form feed, back-
     space, backslash, single quote, and double quote, respec-
     tively.  In addition a \ followed by up to three octal
     digits specifies the character whose ASCII code is the octal
     value.  A single (double) quote also may be put in a string
     delimited with single (double) quotes by specifying two sin-
     gle (double) quotes.  This feature is disabled by the -ss
     option.

     The interface to Unix is through a module called _u_n_i_x rather
     than the _S_Y_S_T_E_M module.

     Additional keywords are recognized in certain contexts.
     These keywords are prefixed by @ to avoid conflicting with
     valid identifiers.

     Pointer attributes
          Attributes may be specified between the keywords
          _P_O_I_N_T_E_R and _T_O or after the keyword _D_Y_N_A_R_R_A_Y in order
          to change the default assumptions of Modula-2 pointer
          with checking.  Recognized attributes are:
          @NOCHECK            Modula-2 pointer, no checking
          @C                  C/malloc pointer, no checking
          @PASCAL             Pascal pointer, Pascal checking

     Size and alignment
          The size and alignment of data types may be specified
          preceding any type specification.  The size and align-
          ment multiples are in bits.  For example,
              TYPE Register = @ALIGN 2 @SIZE 4 [-8..7];
          defines a type that occupies 4 bits aligned on a multi-
          ple of two bits.

     Exports
          Exports from a definition module are assumed qualified
          whether the export statement says qualified or not.

     External variables and procedures
          A procedure or variable may be accessed by C and Pascal
          routines using its unqualified name if the @EXTERNAL
          attribute occurs between the keyword procedure and the
          name of the procedure or precedes the variable declara-
          tion, respectively.

     Uncounted open arrays
          Open array parameters appear as two parameters, the
          address of the array and the number of element, to
          non-Modula-2 programs.  If necessary, the count may be
          omitted by placing the attribute @NOCOUNT between the
          keywords _A_R_R_A_Y and _O_F in the open array declaration.

FILES
     file.mod                  Program or implementation module
     file.def                  Definition module
     file.pcd                  Pcode (-P or -r)
     file.s                    Assembly code (-S or -r)
     /usr/local/lib/mod/mod2.0 Modula-2 compiler front-end
     /usr/local/lib/mod/mod2.1 Modula-2 compiler back-end
     /usr/local/lib/mod/mod2.2 Intermodule checker
     /usr/local/defs/*.def     Standard definition modules
     /usr/local/lib/libmod.a   Default library
     /tmp/modNNNNNN.pcd        Temporary Pcode file
     /tmp/modNNNNNN.s          Temporary assembly code file

SEE ALSO
     N. Wirth, _P_r_o_g_r_a_m_m_i_n_g _i_n _M_o_d_u_l_a-_2, Springer-Verlag, New
     York, 1982.

     _T_h_e _W_R_L _M_o_d_u_l_a-_2 _U_s_e_r'_s _M_a_n_u_a_l

DIAGNOSTICS
     All error messages suppress subsequent compilation phases.
     Error messages ending with a question mark are internal
     errors, and probably represent compiler bugs.  When pointer
     checking is enabled in a running Modula-2 program, segmenta-
     tion faults may be generated by the pointer validation test.
     These are intentional and should be considered as invalid
     pointer messages. The compiler runs with runtime checks
     enabled, and may produce core dumps.  Report problems to the
     author.

AUTHORS
     Original compiler by Michael L. Powell, while working at
     DEC's Western Research Laboratory.  Improvements and bug
     fixes by Joel McCormack.  Port to VMS by Chuck Doucette.
     Port to MIPS by Len Lattanzi.

     Software and documentation is Copyright (c) 1984-1990, Digi-
     tal Equipment Corporation, Maynard, Massachusetts.  All
     rights reserved.  This software is provided under a license
     agreement.

LIMITATIONS
     No warranties are expressed or implied about the compiler's
     conformance to the definition of the Modula-2 language or
     about its proper functioning.  We will endeavor to report
     and fix bugs, but users should be aware that this compiler
     is not a supported product.









