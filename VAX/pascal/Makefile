############################# Definitions ###################################

# NOTE: if you change any of these directory or binary names, you must also
# make the same changes in all Makefiles.  You should be able to do that 
# automatically, but you can't.  Too bad.

# Top-level directory for installation.  Define as /tmp/ to test installation.
# Note that if you define DESTDIR, it must have a trailing /.
DESTDIR     =
# Directory for installing libmod.a
LIBDIR      = /usr/local/lib
# Directory for installing the passes of the compiler
PASSESDIR   = /usr/local/lib/mod
# Directory for installing the .def files we provide
DEFSDIR     = /usr/local/defs
# Directory for installing the man pages for mod and p2m2
MANDIR      = /usr/man/manl
# Directory for installing the binaries mod and p2m2
BINDIR      = /usr/local/bin

# Name of Modula-2 driver program
MODBINNAME  = mod
# Name of Pascal driver program
PCBINNAME   = wrlpc
# Name of Pascal to Modula-2 converter program
P2M2BINNAME = p2m2
# Name of Modula-2 statement-level profiler
MODPROFBINNAME = modprof

# Name of directory in which lib sources reside
TESTLIBDIR  = /udir/joel/m2/lib

# Names of VMS directories (not yet supported)
REMOTEDIR = ophion::'[.m2.mod2]'
SUBJECT    = "Here are the results of the remote make (MMS on VMS)."
REMOTENODE = ophion::
SUBMITFILE = makemod.com

########################## End of Definitions ###############################


#############################################################################
# This file looks very much like the Makefile in ../pascal.  If you change  #
# one, you probably want to change the other in a similar way.              #
#############################################################################

all:	pas2.0 pas2.0t

.SUFFIXES:
.SUFFIXES: .o .mod .def .c

YACC = ../yacc/yacc

PG =
CFLAGS = $(PG)
modflags = -O -a -g $(PG) -Dpascal=TRUE -Dmodula2=FALSE -B../lib
MODFLAGS = -C $(modflags)
MOD = ../mod/mod

.def.mod:
	touch $*.mod
.mod.o:
	$(MOD) -c $(MODFLAGS) $*.mod

# Objects common to Modula-2 and Pascal compilers
COMMONOBJS = \
	main.o Mod2.o Former.o Inline.o Tokens.o Strings.o Keywords.o\
	Alloc.o Globals.o Decls.o TypeInfo.o Compatible.o MemLib.o\
	TypeDefs.o Consts.o BuildExpr.o BuildStmt.o CheckStmt.o CheckExpr.o\
	InitBuiltin.o CheckBuiltin.o OptBuiltin.o Errors.o Scanner.o Symbols.o\
	IOLib.o Machine.o Optim.o OCount.o OTree.o iolibc.o memlibc.o

# Objects specific to Titan
TOBJS =	$(COMMONOBJS) \
	GenCodeT.o  GenT.o  Mahler.o SymbolDumpT.o  BuiltinT.o

# Objects specific to VAX
VOBJS =	$(COMMONOBJS) \
	GenCodePC.o GenPC.o PCode.o  SymbolDumpPC.o BuiltinPC.o PCodeOps.o

# Objects specific to Pascal
TPOBJS = $(TOBJS) pas_gram.o
VPOBJS = $(VOBJS) pas_gram.o

pas:	$(VOBJS) pas_gram.o
	$(MOD) -o pas $(MODFLAGS) -m"$(MODFLAGS)" $(VPOBJS)

pas2.0:	$(VOBJS) pas_gram.o
	$(MOD) -o pas2.0 $(MODFLAGS) -m"$(MODFLAGS)" $(VPOBJS)

past:	$(TOBJS) pas_gram.o
	$(MOD) -o past $(MODFLAGS) -m"$(MODFLAGS)" $(TPOBJS)

opast:	$(TOBJS) pas_gram.o
	$(MOD) -o opast -R $(MODFLAGS) -m"$(MODFLAGS)" $(TPOBJS)

pas2.0t:$(TOBJS) pas_gram.o
	$(MOD) -o pas2.0t -R $(MODFLAGS) -m"$(MODFLAGS)" $(TPOBJS)


Scanner.o:	Scanner.def Scanner.mod
	$(MOD) -c $(modflags) Scanner.mod

quickinstallpas2.0: ${DESTDIR}$(PASSESDIR)
	install -c -s pas2.0 ${DESTDIR}$(PASSESDIR)/pas2.0
quickinstallpas2.0t: ${DESTDIR}$(PASSESDIR)
	install -c -s pas2.0t ${DESTDIR}$(PASSESDIR)/pas2.0t

installpas2.0:	${DESTDIR}$(PASSESDIR) ${DESTDIR}$(PASSESDIR)/pas2.0
installpas2.0t:	${DESTDIR}$(PASSESDIR) ${DESTDIR}$(PASSESDIR)/pas2.0t

${DESTDIR}$(PASSESDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(PASSESDIR)/pas2.0: pas2.0
	install -c -s pas2.0 ${DESTDIR}$(PASSESDIR)/pas2.0
${DESTDIR}$(PASSESDIR)/pas2.0t: pas2.0t
	install -c -s pas2.0t ${DESTDIR}$(PASSESDIR)/pas2.0t

clean:
	-rm *.o y.output .,*

checkin:
	ci -l -f -m"For first release on gatekeeper" \
	-s"Release" [A-Z]*.def [A-Z]*.mod *.c Bugs \
	compiler.doc *.gram Makefile descrip.mms; \
	touch [A-Z]*.def iolibc.c [A-Z]*.mod descrip.mms


# @inline procedure depencies
BuiltinPC.o:	PCode.mod PCode.def
GenPC.o:	PCode.mod PCode.def
SymbolDumpPC.o:	PCode.mod PCode.def

iolibc.c:	IOLib.def
	@echo "Please change iolibc.c to match IOLib.def"

pas_gram.c: pas.gram
	@echo Should be 3 shift/reduce and no reduce/reduce conflicts
	$(YACC) -vd pas.gram; mv y.tab.c pas_gram.c; rm y.tab.h

