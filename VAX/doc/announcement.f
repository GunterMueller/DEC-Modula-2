





          _C_O_M_P_I_L_E_R _A_N_N_O_U_N_C_E_M_E_N_T _2_8 _J_u_n_e _1_9_9_0

          The Western Research Laboratory of  Digital  Equipment  Cor-
          poration  is  pleased  to announce the availability of a new
          version of their Modula-2 compiler for VAXes running  Ultrix
          or  BSD.  (The compiler has also been ported to the MIPS and
          to VMS; these versions should be available ``soon.'')

          The compiler was designed and built by  Michael  L.  Powell,
          and  originally  released in 1984.  Joel McCormack has since
          sped the compiler up, fixed lots of bugs, and stolen/written
          a User's Manual.  The compiler appears to be one of the best
          compilers for the VAX in terms of  efficiency  of  the  gen-
          erated code.  The error messages are quite a bit better than
          those from the Ultrix C compiler and  the  compile  time  is
          about  the  same  as  cc  and  gcc.   Further,  the compiler
          includes  an  intermodule  checker  that  uses  fine-grained
          dependencies  rather  than  timestamps,  so  you  can modify
          definition modules in upward-compatible ways without  recom-
          piling the world.

          The compiler includes several extensions.  The most  notable
          are:  provisions  for  interfacing  with procedures and data
          types written in other languages like C and Pascal (a defin-
          ition  module  for  the  X11 C library is included); dynamic
          array variables, subarray parameters, and  multi-dimensional
          open  array  parameters; inline procedures; complete support
          for the LONGFLOAT type;  and  a  pseudo-module  providing  a
          type-checked interface to the C library I/O routines.

          In addition, the compiler front  end  can  be  conditionally
          compiled  to  generate  a  Pascal  compiler  that  accepts a
          language almost identical to  Berkeley  Pascal.   (Procedure
          parameters  are not supported, and record layout and parame-
          ter passing conventions are somewhat different.)

          The distribution also includes a program that does  most  of
          the translation of Berkeley Pascal programs into Modula-2.

          The   compiler   is   available   for   anonymous   ftp   in
          gatekeeper.pa.dec.com,    in   the   compressed   tar   file
          /pub/DEC/m2.vax.tar.Z.  Once you have copied this file,  you
          can uncompress it and untar it, like:

                  uncompress m2.vax.tar.Z
                  tar xf m2.vax.tar

          From there, read the README file, and the doc/installation.f
          file.

          The compiler is  available  to  DEC  sites  over  DECNET  at
          decwrl::/pub/DEC/m2.vax.tar.Z.

          The compiler is also available by  magnetic  tape.   Send  a



                                28 June 1990





                                     - 2 -


          check  made out to Digital Equipment Corporation for $100 US
          to:

                  Director of Licensing
                  Digital Equipment Corporation
                  Western Research Laboratory
                  100 Hamilton Avenue
                  Palo Alto, CA  94301


          Unlike the previous license, the new license does  not  res-
          trict  use of the compiler to educational institutions.  The
          license agreement is as follows:

          ------------------------------------------------------------
               Copyright 1984-1990 Digital Equipment Corporation
                              All Rights Reserved

          Permission to use, copy, and modify this  software  and  its
          documentation  is  hereby  granted  only under the following
          terms and conditions.  Both the above copyright  notice  and
          this  permission  notice  must  appear  in all copies of the
          software, derivative works or  modified  versions,  and  any
          portions thereof, and both notices must appear in supporting
          documentation.

          Users of this software agree to the terms and conditions set
          forth  herein,  and  hereby  grant  back  to  Digital a non-
          exclusive,  unrestricted,  royalty-free  right  and  license
          under  any  changes,  enhancements or extensions made to the
          core functions of the software, including but not limited to
          those   affording   compatibility  with  other  hardware  or
          software  environments,  but  excluding  applications  which
          incorporate this software.  Users further agree to use their
          best efforts to return to Digital any such changes, enhance-
          ments  or  extensions  that  they make and inform Digital of
          noteworthy uses of this software.  Correspondence should  be
          provided to Digital at:

                  Director of Licensing
                  Western Research Laboratory
                  Digital Equipment Corporation
                  100 Hamilton Avenue
                  Palo Alto, California  94301

          This software may be distributed (but not offered  for  sale
          or  transferred for compensation) to third parties, provided
          such third parties agree to abide by the  terms  and  condi-
          tions of this notice.

          THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP.
          DISCLAIMS  ALL  WARRANTIES  WITH  REGARD  TO  THIS SOFTWARE,
          INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
          NESS.    IN  NO EVENT SHALL DIGITAL EQUIPMENT CORPORATION BE



                                28 June 1990





                                     - 3 -


          LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT,  OR  CONSEQUENTIAL
          DAMAGES  OR  ANY  DAMAGES  WHATSOEVER RESULTING FROM LOSS OF
          USE, DATA OR PROFITS, WHETHER  IN  AN  ACTION  OF  CONTRACT,
          NEGLIGENCE  OR  OTHER  TORTIOUS ACTION, ARISING OUT OF OR IN
          CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

          ------------------------------------------------------------


          If you have any questions, you can send mail to
                  decwrl!modula-2
          or
                  modula-2@decwrl.pa.dec.com
          or
                  decwrl::modula-2


          This compiler has a number of internal  and  external  users
          and  we feel it is quite reliable.  However, new users often
          find new bugs so we expect to hear from some  of  those  who
          try  it.   No  promises  about  bug  fixes  or new releases,
          though.



































                                28 June 1990


