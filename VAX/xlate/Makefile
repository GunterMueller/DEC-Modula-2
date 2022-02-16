# $Header: Makefile,v 1.21 89/06/16 15:31:29 joel Experimental $

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
TESTLIBDIR  = /udir/joel/m2 lib

# Names of VMS directories (not yet supported)
REMOTEDIR = ophion::'[.m2.mod2]'
SUBJECT    = "Here are the results of the remote make (MMS on VMS)."
REMOTENODE = ophion::
SUBMITFILE = makemod.com

########################## End of Definitions ###############################

.SUFFIXES:
.SUFFIXES: .o .mod .def .c

vms = FALSE

CFLAGS = -O
MODFLAGS = -a -g -O -Dvms=$(vms) -B../lib
MOD = ../mod/mod

.def.mod:
	touch $*.mod
.mod.o:
	$(MOD) -c $(MODFLAGS) $*.mod

all: mod2.1 descrip.mms

OBJS =	CodeSubs.o Consts.o EES.o Error.o Input.o Machine.o \
	MemRef.o OpSubs.o Types.o Output.o Util.o Vars.o Xlate.o \
	PCodeOps.o Hash.o machinec.o

xlate:	$(OBJS)
	$(MOD) -o xlate $(MODFLAGS) -m"$(MODFLAGS)" $(OBJS)

mod2.1:	$(OBJS)
	$(MOD) -o mod2.1 $(MODFLAGS) -m"$(MODFLAGS)" $(OBJS)

testr:	$(OBJS)
	$(MOD) -c $(MODFLAGS) -m"$(MODFLAGS)" $(OBJS)

# hash table function generation
makehash:makehash.o PCodeOps.def PCodeOps.mod PCodeOps.o
	$(MOD) -o makehash $(MODFLAGS) -m"$(MODFLAGS)" PCodeOps.o makehash.o
Input.mod:	Hash.def
Hash.def:	PCodeOps.def PCodeOps.mod makehash
	./makehash
Hash.mod:	PCodeOps.def PCodeOps.mod

# inline dependencies
CodeSubs.o:	Output.o Util.o
EES.o:		Output.o
MemRef.o:	Output.o Util.o
OpSubs.o:	Output.o Util.o


quickinstall: ${DESTDIR}$(PASSESDIR)
	install -c -s mod2.1 ${DESTDIR}$(PASSESDIR)/mod2.1

install: ${DESTDIR}$(PASSESDIR) ${DESTDIR}$(PASSESDIR)/mod2.1

${DESTDIR}$(PASSESDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(PASSESDIR)/mod2.1:	mod2.1
	install -c -s mod2.1 ${DESTDIR}$(PASSESDIR)/mod2.1

clean:
	-rm *.o

descrip.mms: Makefile
	echo Update descrip.mms to match Makefile

vmscopy:
	dcp -v [A-Z]*.def [A-Z]*.mod makehash.mod machinec.c descrip.mms \
	$(REMOTEDIR)

checkin:
	ci -l -f -m"Checkin before VMS changes" \
	-s"Experimental" \
	[A-Z]*.def [A-Z]*.mod makehash.mod machinec.c Makefile descrip.mms
