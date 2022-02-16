








                    Installing the DECWRL Modula-2 Compiler
                                 21 June, 1990





          _1.  _O_V_E_R_V_I_E_W

               The DECWRL Modula-2 compiler consists of three compila-
          tion phases, an intermodule checker, and a command to invoke
          the various phases.   The  first  phase  compiles  a  single
          Modula-2  module  into  P-code, a machine-independent inter-
          mediate language.  The second phase translates  P-code  into
          VAX  assembly  language.  The third phase is the Unix assem-
          bler, which translates the  assembly  code  into  an  object
          file.

               Before a collection of object modules are  linked,  you
          can  optionally invoke the intermodule checker to detect any
          inconsistencies  between  modules.   You  can  instruct  the
          intermodule checker automatically recompile any modules that
          are out of date.  The Unix linker is then  used  to  combine
          the object modules into an executable file.

               The DECWRL Modula-2 compiler is  distributed  on  a  9-
          track,  1600 bpi tape readable by the Unix _t_a_r command.  For
          the purposes of this document, Unix will  mean  4.3  BSD  or
          some  recent  version  of  Ultrix.  It may or may not run on
          your version of Unix.  The main  things  that  the  compiler
          depends  on are: itself, to compile most of the sources; the
          YACC parser generator,  which  is  used  to  generate  parse
          tables  for  the compiler; and the Unix C compiler, which is
          used to compile some of the library  routines  and  the  _m_o_d
          command.   The  runtime  library  calls some C routines, but
          they are standard across most implementations.

               NOTE  ABOUT  LICENSES:  The  distribution  contains  no
          software  proprietary to Bell Laboratories or the Regents of
          the University of California.  The compiler was written from
          scratch  and  is  not  derived from Wirth's Lilith compiler.
          Software and documentation is copyright 1984-1990 by Digital
          Equipment  Corporation,  Maynard, Massachusetts, and is made
          available subject to a license agreement.


          _________________________
          DEC and VAX are trademarks of  Digital  Equipment  Cor-
          poration.
          Unix is a trademark of Bell Laboratories.




                                21 June, 1990





                                     - 2 -


          _2.  _I_N_S_T_A_L_L_I_N_G _T_H_E _C_O_M_P_I_L_E_R

               The compiler is organized as one  top-level  directory,
          with several subdirectories.  Since all directory paths used
          to create the compiler are self-relative, you can create the
          top-level  directory  at any convenient place in your source
          hierarchy.  Sources and object files require about 30  mega-
          bytes.  Once you have created this top-level directory, copy
          and unpack the tape by typing "tar xf /dev/rmt0l".

               Each subdirectory contains one or more components.  The
          top-level  Makefile  can  be used to clean, make, or install
          all components at one time: type "make  clean",  "make",  or
          "make  install".   You can also install the compiler without
          all dependencies being satisfied by typing  (qmake  quickin-
          stall";  this  is useful for installing the distributed com-
          piler as is without  actually  recompiling  all  the  source
          files.

          The top-level Makefile defines the directory names  DESTDIR,
          LIBDIR,  PASSESDIR,  DEFSDIR,  MANDIR,  and  BINDIR, and the
          binary executable names MODBINNAME,  PCBINNAME,  P2M2BINNAME
          and  MODPROFBINNAME;  you  may want to change some or all of
          these names to match local naming conventions.

               DESTDIR is the root of the installation  tree,  and  is
          originally defined as empty.  DESTDIR is prefixed to each of
          the other directory names  defined.   Some  people  like  to
          define  DESTDIR as /tmp/ the first time through, just to see
          that everything happens  correctly.   DESTDIR  is  the  only
          directory name in which you include a trailing /.

               LIBDIR is the full pathname of the directory  in  which
          the  Modula-2  runtime library _l_i_b_m_o_d._a is installed, and is
          originally defined as /usr/local/lib.  This  directory  name
          is incorporated into the _m_o_d executable, but you can specify
          a specific  library  archive  file  using  the  "-lIlibname"
          switch.

               PASSESDIR is the full  pathname  of  the  directory  in
          which  the various passes of the compiler are installed, and
          is originally defined as /usr/local/lib/mod.  This directory
          name  is  incorporated  into  the _m_o_d executable, but can be
          overridden using the "-B" switch.

               DEFSDIR is the full pathname of the directory in  which
          definition  modules are installed, and is originally defined
          as /usr/local/defs.  This  directory  name  is  incorporated
          into  the  _m_o_d executable, but can be supplemented using the
          "-I" switch.

               MANDIR is the full pathname of the directory  in  which
          the  _m_a_n pages for _m_o_d and _p_2_m_2 are installed, and is origi-
          nally defined as /usr/man/manl.



                                21 June, 1990





                                     - 3 -


               BINDIR is the full pathname of the directory  in  which
          the  _m_o_d  and  _p_2_m_2 executables are installed, and is origi-
          nally defined as /usr/local/bin.

               MODBINNAME is the name of the executable  that  invokes
          the  various  phases of the Modula-2 compiler, and is origi-
          nally defined as mod.

               PCBINNAME is the name of the  executable  that  invokes
          the various phases of the Pascal compiler, and is originally
          defined as wrlpc.

               P2M2BINNAME is the name of the Pascal to Modula-2  con-
          verter executable, and is originally defined as p2m2.

               MODPROFNAME is the name of the Modula-2 statement-level
          profiler, and is originally defined as modprof.

               The top-level Makefile  passes  all  these  definitions
          down  to  the component Makefiles.  If you want to make com-
          ponents one at a time, you should make sure that any changes
          to  these  directory  names  are incorporated into each com-
          ponent Makefile.  There is no automatic way  of  doing  this
          right now; volunteers are solicited to convert everything to
          Imake or something similar.

               Note that in the mod/Makefile, imod is the  driver  for
          the  installed  version  of the compiler.  Just plain mod is
          the driver for testing purposes.  This executable  specifies
          _l_i_b  as  the  directory  containing  the definitions and the
          passes of the compiler.  It contains self-relative  symbolic
          links  to  the  executables in the _m_o_d, _m_o_d_2, _x_l_a_t_e, and _i_m_c
          directories.  You must assign the full path name of the  lib
          source  directory  to  the  TESTLIBDIR  variable  in the _m_o_d
          Makefile, then type "make mod".  Executing _m_o_d will use  the
          latest  versions  of  the binaries and libraries.  Note that
          all the Makefiles use the testing _m_o_d  to  compile  Modula-2
          files.

               The components of the compiler should be  installed  in
          the  order in which they are described below.  The top-level
          Makefile observes this order.

               NOTE ABOUT  UNIX  SYSTEM  COMPATIBILITY:  Although  the
          sources  distributed can probably be compiled and run on any
          4.x BSD or Ultrix system, there is some  chance  the  object
          modules  on  the  tape  will not work.  If you have problems
          linking the programs (unresolved references), or errors that
          might  be  attributed to I/O problems (mysterious core dumps
          before the compiler does anything,  the  compiler  runs  but
          produces  no output, etc.), these may result from changes to
          the C stdio library.  I/O and memory  allocation  have  been
          left  in C to ease bootstrapping.  If you run into problems,
          try removing any object files created by compiling C sources



                                21 June, 1990





                                     - 4 -


          by typing "make cclean" and try again to make the compiler.

          _3.  _M_O_D _A_N_D _W_R_L_P_C _C_O_M_M_A_N_D_S

               The mod command is a C program that invokes the  phases
          of  the Modula-2 compiler.  The wrlpc command is a C program
          that invokes the phases of the Pascal compiler.  The  source
          is  in a directory called _m_o_d.  The same source file is con-
          ditionally compiled to create both the Modula-2  and  Pascal
          versions, as well as testing and installed versions of each.
          To create the installed versions, you should type "make imod
          iwrlpc"  and  then  "make install"; this will install mod as
          MODBINNAME and wrlpc as PCBINNAME in the BINDIR directory.

               The testing version of the Modula-2 compiler is  called
          just plain _m_o_d and the testing verion of the Pascal compiler
          is called _w_r_l_p_c.

          _4.  _F_R_O_N_T _E_N_D_S

               The compiler front end is a YACC, C, and Modula-2  pro-
          gram that parses individual Modula-2 and Pascal programs and
          generates P-code for them.  The same  source  files  (except
          for  the  YACC grammar) are conditionally compiled to create
          both the Modula-2 and Pascal front ends.  An  optimizer  may
          be selected to improve the speed of the generated code--this
          optimizer  works  best  on  code  that  has  runtime  checks
          enabled.

               The source for the Modula-2 front end is in the  direc-
          tory  _m_o_d_2.   Typing  "make  installmod2.0" will install the
          Modula-2 compiler front  end  as  _m_o_d_2._0  in  the  PASSESDIR
          directory.

               The source for the Pascal front end is in the directory
          _p_a_s_c_a_l.  All files in this directory except the Makefile are
          symbolic links to the _m_o_d_2 directory.  Typing "make install-
          pas2.0" will install the Pascal compiler front end as _m_o_d_2._0
          in the PASSESDIR directory.

               Note that even the huge definitions  for  the  standard
          YACC  table  sizes  may be too small to compile the Modula-2
          and Pascal grammars.  I use the following constants  in  the
          YACC source file _d_e_x_t_e_r_n:
          #define NSTATES 1200
          #define LSETSIZE 1000

          _5.  _B_A_C_K _E_N_D

               The compiler back end is a Modula-2 program that parses
          P-code  files and generates VAX/Unix assembly language.  The
          source for the back end is in the directory  _x_l_a_t_e.   Typing
          "make  install" will install the compiler back end as _m_o_d_2._1
          in the PASSESDIR directory.



                                21 June, 1990





                                     - 5 -


          _6.  _L_I_B_R_A_R_Y

               The runtime library is a set of C and Modula-2 programs
          that  provide  a  minimal  runtime environment for Modula-2.
          Only two of the modules (strings  and  parameters)  actually
          exist  as Modula-2 implementations.  Most of the others have
          Modula-2 definition modules that are  merely  interfaces  to
          the  C  implementation.  The SYSTEM.def, Memory.def, IO.def,
          and BitOperations.def modules  document  the  pseudo-modules
          that  are specially parsed by the compiler.  The Storage.def
          module is provided for programs  that  import  the  standard
          memory allocation routines ALLOCATE and DEALLOCATE, although
          such imports are not needed to use NEW  and  DISPOSE  unless
          the "standard Modula-2" switch -s is used.

               The unix.def module defines  many  common  Unix  system
          calls  in  a  way  that Modula-2 programs can use, and x.def
          provides an interface to the X11 library.  Feel free to  add
          your favorites, as long as you send back your additions.

               The file runtime.c contains much of  the  miscellaneous
          runtime   support.    Included   is   a   procedure   called
          SYSTEM_cputime that returns the accumulated CPU time  for  a
          program in milliseconds.  It may be necessary to modify this
          on your system.  The file mem.c contains the memory  alloca-
          tion  routines,  coroutine.c  contains  the Modula-2 process
          routines, and udiv.c contains an  unsigned  32-bit  division
          routine.

               Typing "make  install"  will  create  the  library  and
          install  it  in  the  LIBDIR directory.  Two versions of the
          library are created: one for use in profiling Modula-2  pro-
          grams (-pg option) and the other for normal use.

          _7.  _I_N_T_E_R_M_O_D_U_L_E _C_H_E_C_K_E_R

               The intermodule checker  is  a  Modula-2  program  that
          examines  the  symbol table information in a set of Modula-2
          object files to be sure they are consistent.  (The  intermo-
          dule checker does not check any other object files, not even
          those generated by the Pascal compiler.) It will  optionally
          recompile any files that are out of date.

               The intermodule checker  uses  information  similar  to
          that used by _d_b_x.  The information is placed into the object
          modules with type 80 decimal (= 50 hexadecimal).  It is pos-
          sible that this number is in use by another language proces-
          sor, in which case an alternate should be selected (probably
          16 plus some multiple of 32).  To specify a different value,
          change the constant STABNMOD2 in mod2/SymbolDump.mod and  in
          imc/stab.def    and    add    the    constant    N_MOD2   to
          /usr/include/stab.h.   For  cleanliness,  you  should   also
          modify  the  Unix  _n_m command to recognize Modula-2 entries.
          If /usr/include/stab.h does  not  define  N_MOD2,  dbx  will



                                21 June, 1990





                                     - 6 -


          assume 80 decimal.

               The source for the intermodule checker is in  directory
          _i_m_c.   Typing  "make  install"  will install the intermodule
          checker as _m_o_d_2._2 in the PASSESDIR directory.

          _8.  _P_2_M_2

               _p_2_m_2 is a conversion aid for translating Berkeley  Pas-
          cal  programs  into  Modula-2.  It consists of a YACC parser
          and some Pascal routines to read .h and .p files and produce
          .def  and  .mod  files.  The source is in a directory called
          p2m2.

               Typing "make install" will install p2m2 in  the  BINDIR
          directory.

          _9.  _D_B_X

               We use the debugger _d_b_x for Modula-2.  Mark  Linton  of
          Stanford University developed the debugger, and has extended
          it to work for Modula-2.  Since that software is covered  by
          a 4.x BSD license, it is not a part of this software distri-
          bution.  However, we are distributing a set  of  updates  to
          the 4.2 version of DBX; these updates were developed by Mark
          Linton and are in the public domain.

               The updates to make DBX are  in  the  directory  called
          _d_b_x.   To  generate a DBX that will work with Modula-2, copy
          the source files for the 4.2 DBX into the appropriate direc-
          tory.   You  should check that the sizes and version numbers
          of your source files agree with the sizes at  the  beginning
          of the file mkupdate.  If they do not agree, then you do not
          have a clean 4.2 version  of  DBX,  and  you  will  have  to
          integrate  the changes by hand (Good luck!).  If they agree,
          execute the csh command file _m_k_u_p_d_a_t_e.

               The csh command file mktests will create a set of  test
          directories  for  dbx.   You should then be able to create a
          new dbx by typing "make install".  Dbx  includes  a  set  of
          regression  tests  which  can  be  run  before installing an
          updated version.  To run  these  tests,  type  "make  test".
          _M_a_n_P_a_g_e contains a new manual page for dbx.

          _1_0.  _D_O_C_U_M_E_N_T_A_T_I_O_N

               For a description of the  standard  Modula-2  language,
          see  N.  Wirth, _P_r_o_g_r_a_m_m_i_n_g _i_n _M_o_d_u_l_a-_2 Springer-Verlag, New
          York, 1982.

               The file _m_2._p_s_f in the _r_e_f_m_a_n directory contains a com-
          plete  reference  manual  for  the DECWRL Modula-2 language,
          including all extensions.  This document  is  in  PostScript
          format.



                                21 June, 1990





                                     - 7 -


               This installation guide plus manual pages for the  _m_o_d,
          _p_2_m_2,  and  _m_o_d_p_r_o_f  commands  are  in a directory call _d_o_c.
          This directory also includes an overview  of  the  compiler,
          and a document discussing interfacing Modula-2 to C and Pas-
          cal programs.

          _1_1.  _B_E_N_C_H_M_A_R_K_S

               The  source  for  the  benchmark  program  in   various
          languages  is supplied in a directory called bench.  Compile
          and enjoy.  Feel free to put your favorite compiler  to  the
          test.

               Experience has shown that the Modula-2  optimizer  does
          not  really do as well on real programs as it does on bench-
          marks.  However, it does a pretty good job of getting rid of
          redundant runtime checks.

          _1_2.  _T_E_S_T_S

               Some test programs are included in a  directory  called
          test.  The Makefile has some comments about what they should
          do.

          _1_3.  _B_U_G_S

               I'm not in the business  of  immediately  fixing  bugs,
          especially  those  require  restructuring,  but I do want to
          know about them.  Your chances of getting a  bug  fixed  are
          inversely  proportional  to  the  size  of  the test program
          demonstrating such bug.  Much better than  bug  reports,  of
          course, are bug fixes.

               Comments on  the  compiler  and  distribution  will  be
          appreciated.

               Mail to:
                    Joel McCormack
                    Digital Equipment Corporation
                    Western Research Laboratory
                    100 Hamilton Avenue
                    Palo Alto, CA  94301

                    decwrl!modula-2
                    modula-2@decwrl.dec.com
                    decwrl::modula-2











                                21 June, 1990


