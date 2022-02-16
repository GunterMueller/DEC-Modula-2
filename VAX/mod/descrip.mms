#
# $Header: Makefile,v 1.9 86/11/17 17:25:58 joel WSL/Titan/TapeRelease $
# Changed by CED on 2/18/87 (most of this file was commented out)
# Initial VMS pass on 3/10/87
#
TESTLIBDIR = dmod$testlib:
INSTALLDIR = dmod$lib:
MODLIBDIR = $(INSTALLDIR)
LINKFLAGS = 
CFLAGS = /OPTIMIZE /DEBUG

all : $(TESTLIBDIR)mod.exe 
	continue

$(TESTLIBDIR)mod.exe : mod.obj
	LINK/EXE=$*$(LINKFLAGS) $?,\
$(TESTLIBDIR)MODLIB/LIB,SYS$LIBRARY:VAXCRTL/LIB

mod.obj : mod.c
	CC$(CFLAGS)/DEFINE=\
(MODLIBDIR="""$(TESTLIBDIR)""",\
MODPASSESDIR="""$(TESTLIBDIR)""",\
MODDEFSDIR="""$(TESTLIBDIR)""",\
PASCAL=0,MODULA2=1)\
 mod.c

install : $(INSTALLDIR)mod.exe $(TESTLIBDIR)mod.exe
	CONTINUE

$(INSTALLDIR)mod.exe : imod.obj
	LINK/EXE=$*$(LINKFLAGS) $?,\
$(TESTLIBDIR)MODLIB/LIB,SYS$LIBRARY:VAXCRTL/LIB

#
# NOTE:
# imod.exe isn't made until install because that is when the $(INSTALLDIR) macro
# is supplied by the top-level makefile. If we built it by default (target=all),
# when the top-level makefile supplied the $(INSTALLDIR) it might contradict
# what imod was built with, i.e. MODLIBDIR === $(INSTALLDIR).
#
imod.obj : mod.c
	CC$(CFLAGS)/DEFINE=\
(MODLIBDIR="""$(MODLIBDIR)""",\
MODPASSESDIR="""$(MODLIBDIR)""",\
MODDEFSDIR="""$(MODLIBDIR)""",\
PASCAL=0,MODULA2=1)\
/OBJECT=imod.obj\
 mod.c

clean :
	- delete *.obj;*
	purge/log
