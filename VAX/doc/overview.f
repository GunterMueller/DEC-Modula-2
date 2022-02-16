





          _O_V_E_R_V_I_E_W _O_F _C_O_M_P_I_L_E_R

          The  DECWRL  Modula-2  compiler  distribution  consists   of
          several programs.  The compiler itself is divided into three
          phases: the front end, the back  end,  and  the  intermodule
          checker.   There  is  also  a  program to invoke the various
          phases of  the  compiler,  a  small  runtime  library,  some
          updates to DBX, and a tool for helping convert programs from
          Pascal to Modula-2.

          The  first  phase  of  the  compiler  parses  an  individual
          Modula-2 module and compiles it into P-code.  It consists of
          about 43,000 lines of Modula-2 and 1,400 lines of YACC gram-
          mar.   This  source can be conditionally compiled and linked
          with a different YACC grammar of about 1,000 lines to create
          a  Pascal  parser.  The front end includes an optimizer that
          eliminates common subexpressions, moves  invariants  out  of
          loops,  allocates  registers,  and  performs other optimiza-
          tions.

          The second phase of the compiler translates P-code into  VAX
          instructions.  It consists of about 8,700 lines of Modula-2.
          It folds constants and  selects  instructions  in  order  to
          reduce execution time.

          The intermodule checker  examines  a  collection  of  object
          modules  to  be  sure  they  are consistent.  It consists of
          about 4,000 lines of Modula-2 program.  Optionally, it  will
          recompile any out-of-date modules.

          The mod command examines its argument list and  invokes  the
          various phases of the compiler to compile programs.  It con-
          sists of about 1000 lines of C program.

          There is a small runtime library provided  to  perform  I/O,
          allocate  and  release memory, do some standard string func-
          tions, and interface to the Unix libraries and system calls.
          It consists of about 5,400 lines of C and Modula-2 program.

          The DBX debugger is used to debug Modula-2 programs.  A  set
          of  updates to the 4.2 version of DBX is provided (Since DBX
          is part of the 4.2BSD distribution, we cannot distribute the
          whole debugger).

          A program to help convert Pascal programs into  Modula-2  is
          provided.   It  consists  of about 2,500 lines of Pascal and
          1,000 lines of YACC grammar.

          The benchmark program used to compare different compilers is
          also supplied.

          _S_E_L_E_C_T_E_D _F_E_A_T_U_R_E_S _O_F _T_H_E _C_O_M_P_I_L_E_R

          Compiles comparable programs faster than the Berkeley Pascal



                                21 June 1990





                                     - 2 -


          compiler.   Benchmarks  indicate  code  is comparable to the
          fastest VAX compilers, including gcc.

          Built-in I/O library similar to Unix printf/scanf.

          Automatic recompilation based on  actual  dependencies,  not
          timestamps, in order to minimize recompilations.

          Generated code is compatible with Berkeley Pascal  and  Unix
          C.  Easy to call Pascal or C from Modula-2 and vice-versa.

          Accepts the language  mostly  as  defined  in  the  Modula-2
          report.  Uses compile-time switches to disable extensions in
          the case of reserved words and identifiers.











































                                21 June 1990


