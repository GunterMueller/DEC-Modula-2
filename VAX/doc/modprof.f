
NAME
     modprof - Modula-2 program statement count profiler

SYNOPSIS
     modprof [ name ]

DESCRIPTION
     _M_o_d_p_r_o_f formats the results of a running of a Modula-2 pro-
     gram with statement counting.  The file parameter is the
     name of the profile data, normally modmon.out.  The profile
     data is generated by running a Modula-2 program that was
     compiled with the -ps option.  Statement count data is gen-
     erated for any modules compiled with the option.  The main
     program module must always be compiled with the option, and
     the program must terminate normally (i.e., by returning from
     the main program or by calling the _H_A_L_T procedure).

     A program listing with the statement counts is written to
     the standard output.  A number appearing to the left a line
     indicates the number of times that statement was executed.
     Only the first statement of a set of statements that must
     all be executed the same number of times is labeled.  If a
     line begins with the symbol >>, that line contains addi-
     tional counts for the preceding line.  This can occur when
     there are multiple statements on a line.

FILES
     file.mod                  Profiled program or implementation
     modules
     modmon.out                Profile data

SEE ALSO
     mod(l)
     N. Wirth, _P_r_o_g_r_a_m_m_i_n_g _i_n _M_o_d_u_l_a-_2, Springer-Verlag, New
     York, 1982.

AUTHOR
     Michael L. Powell, while at DEC's Western Research Labora-
     tory

     Software and documentation is Copyright (c) 1984-1990, Digi-
     tal Equipment Corporation, Maynard, Massachusetts.  All
     rights reserved.  This software is provided under a license
     agreement.

LIMITATIONS
     No warranties are expressed or implied about proper func-
     tioning.  We will endeavor to report and fix bugs, but users
     should be aware that this program is not a supported pro-
     duct.














