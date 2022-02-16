# $Header: Makefile,v 1.3 85/03/12 13:58:44 powell Exp $

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


.SUFFIXES:
.SUFFIXES: .o .mod .def .c
MODFLAGS = -O -a -g -C -B../lib
MOD = ../mod/mod

.def.mod:
	touch $*.mod
.mod.o:
	$(MOD) -c ${MODFLAGS} $*.mod

all: modprof

modprof:	modprof.o
	$(MOD) -o modprof $(MODFLAGS) modprof.o

quickinstall: ${DESTDIR}$(BINDIR)
	install -c -s modprof ${DESTDIR}$(BINDIR)/$(MODPROFBINNAME)

install: ${DESTDIR}$(BINDIR) ${DESTDIR}$(BINDIR)/$(MODPROFBINNAME)

${DESTDIR}$(BINDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(BINDIR)/$(MODPROFBINNAME): modprof
	install -c -s modprof ${DESTDIR}$(BINDIR)/$(MODPROFBINNAME)

clean:
	-rm *.o

ci:
