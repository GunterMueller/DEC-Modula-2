# $Header: Makefile,v 1.9 89/09/15 14:33:47 joel Locked $

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

CC = cc
CFLAGS = -0

MOD = ../mod/mod
TITANFLAG = FALSE
MODFLAGS = -a -g -O -Dtitan=$(TITANFLAG) -I/usr/def -B../lib

.def.mod:
	touch $*.mod
.mod.o:
	$(MOD) -c $(MODFLAGS) $*.mod
.c.o:
	$(CC) -c $(CFLAGS) $*.c

OBJS =	imc.o execute.o list.o numtab.o porttab.o symtab.o Strings.o \
	FileInfo.o MemLib.o memlibc.o fileinfo.o aOut.o stab.o

all:	mod2.2

test:	$(OBJS)
	$(MOD) -v -c $(MODFLAGS) -M $(OBJS)
imc:	$(OBJS)
	$(MOD) -o imc $(MODFLAGS) -m"$(MODFLAGS)" $(OBJS)
mod2.2:	$(OBJS)
	$(MOD) -o mod2.2 $(MODFLAGS) -m"$(MODFLAGS)" $(OBJS)
oops:	$(OBJS)
	$(MOD) -o imc $(MODFLAGS) $(OBJS)

checkin:
	ci -l -f -m"Checkin before VMS? changes"\
	-s"Experimental" \
	FileInfo.def FileInfo.mod aOut.def aOut.mod fileinfo.c \
	execute.def execute.mod imc.mod \
	list.def list.mod numtab.def numtab.mod porttab.def porttab.mod \
	stab.def stab.mod symtab.def symtab.mod Makefile

	
quickinstall: ${DESTDIR}$(PASSESDIR)
	install -c -s mod2.2 ${DESTDIR}$(PASSESDIR)/mod2.2

install: ${DESTDIR}$(PASSESDIR) ${DESTDIR}$(PASSESDIR)/mod2.2

${DESTDIR}$(PASSESDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(PASSESDIR)/mod2.2: mod2.2
	install -c -s mod2.2 ${DESTDIR}$(PASSESDIR)/mod2.2
clean:
	-rm *.o
