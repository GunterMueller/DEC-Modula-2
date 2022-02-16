
NAME
     p2m2 - Pascal to Modula-2 conversion tool

SYNOPSIS
     p2m2 [ -h ] [ -s ] name

DESCRIPTION
     _p_2_m_2 is a tool for assisting with the conversion of programs
     from Berkeley Pascal to Modula-2.  It does not perform a
     complete translation, since there are some language features
     in Pascal that either do not exist in Modula-2 (e.g., goto
     statements) or are too difficult to convert (e.g., write
     statements).  It also does not necessarily produce a "good"
     Modula-2 program, since it does not restructure a program
     into modules.

     In spite of these disclaimers, it is possible to use _p_2_m_2 to
     rapidly convert a substantial amount of Pascal software to
     Modula-2.

     Files are converted one at a time.  A file name must end
     with ".p" for a Pascal program or separate compilation unit,
     or with ".h" for a set of definitions or external specifica-
     tions for a separate compilation unit.  A file called
     "name.p" will produce a program or implementation module
     called "name" in a file called "name.mod".  A program module
     will be generated if the file contains a Pascal program.  A
     file called "name.h" will produce a definition module called
     name in a file called "name.def".

     The -h flag is specified when a separate compilation unit is
     converted.  The option causes _p_2_m_2 to scan the corresponding
     ".h" file for procedure parameter definitions and insert
     those in the generated ".mod" file.  This flag is useful
     because Berkeley Pascal prohibits parameters from appearing
     in the procedure definition if there is an external declara-
     tion of the procedure.

     The -s flag outputs reserved words in upper case, in accor-
     dance with the Modula-2 report.  By default, _p_2_m_2 generates
     reserved words in lower case.

METHOD
     As _p_2_m_2 parses the Pascal program, it copies white space
     (comments, spaces, new lines) to the output.  It outputs
     tokens and identifiers, rearranging them as necessary.  The
     result is a program that is formatted approximately the same
     as the original.

     The names of procedures, functions, variables, types, and
     constants defined in ".h" files are exported from the defin-
     ition module.  An include directive is changed to a comment,
     but causes the named module to be imported.

UNHANDLED DIFFERENCES
     The following is a partial list of differences between Pas-
     cal and Modula-2 that are not handled:

     _F_o_r_w_a_r_d _d_e_c_l_a_r_a_t_i_o_n_s are not necessary, since Modula-2
     allows procedures to be defined after they are used.

     _G_o_t_o _s_t_a_t_e_m_e_n_t_s _a_n_d _l_a_b_e_l _d_e_c_l_a_r_a_t_i_o_n_s are not supported.
     Many goto statements may be avoided by using the loop and
     exit or return statements.

     _I/_O _a_n_d _f_i_l_e_s are different, including write statements.
     See _m_o_d(l) and the standard module _i_o._d_e_f for details.

     _P_r_o_c_e_d_u_r_e _p_a_r_a_m_e_t_e_r_s are supported differently.  Since
     Modula-2 supports procedure variables (which may be passed
     as parameters), the syntax for formal procedure parameters
     is similar to other formal parameters.

     _F_u_n_c_t_i_o_n _r_e_t_u_r_n _v_a_l_u_e_s are done through the return state-
     ment, not by assignment to the function name.  Assignments
     to functions are marked with the comment (*!return!*) to
     allow easy editting in most cases.

     _A_u_t_o_m_a_t_i_c _c_o_n_v_e_r_s_i_o_n _o_f _i_n_t_e_g_e_r _t_o _r_e_a_l is not translated to
     explicit conversions calls.

FILES
     file.p     Pascal main program or separate compilation unit
     file.h     Pascal header file
     file.mod   Program or implementation module
     file.def   Definition module
     p2m2.temp  Output of first pass of p2m2

SEE ALSO
     N. Wirth, _P_r_o_g_r_a_m_m_i_n_g _i_n _M_o_d_u_l_a-_2, Springer-Verlag, New
     York, 1982.

DIAGNOSTICS
     Error messages are written to standard output.  In addition,
     comments are inserted in the generated modules to mark
     places where the translation failed.  Such comments are of
     the form (*! ... !*).

AUTHORS
     Benjamin C. Pierce
     Michael L. Powell
     while at DEC's Western Research Laboratory

     Software and documentation is Copyright (c) 1984-1990, Digi-
     tal Equipment Corporation, Maynard, Massachusetts.  All
     rights reserved.  This software is provided under a license
     agreement.

LIMITATIONS
     No warranties are expressed or implied about proper func-
     tioning.




















