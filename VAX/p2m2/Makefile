# $Header: Makefile,v 1.7 86/07/23 13:15:18 joel Exp $
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

all:	p2m2

CFLAGS = -g
PFLAGS = -w -C -g
OBJS =	p2m2.o \
	y.tab.o stringtab.o keywords.o scanner.o \
	error.o io.o util.o

p2m2:	$(OBJS)
	pc -w -o p2m2 $(OBJS)

error.o: error.p stringtab.h tokens.h globals.h error.h scanner.h
keywords.o: keywords.p stringtab.h tokens.h globals.h keywords.h
p2m2.o: p2m2.p tokens.h stringtab.h globals.h error.h scanner.h io.h\
	keywords.h util.h
scanner.o: scanner.p tokens.h stringtab.h globals.h error.h scanner.h io.h\
	keywords.h util.h
stringtab.o: stringtab.p stringtab.h tokens.h error.h globals.h util.h
util.o: util.p tokens.h stringtab.h error.h globals.h scanner.h util.h
io.o:	io.c io.h

y.tab.c: pascal.gram
	@echo should produce 3 shift/reduce conflicts
	yacc -vd pascal.gram

y.tab.o: y.tab.c
	cc $(CFLAGS) -c -DYYMAXDEPTH=250 y.tab.c

quickinstall: ${DESTDIR}$(BINDIR)
	install -c -s p2m2 ${DESTDIR}$(BINDIR)/$(P2M2BINNAME)

install: ${DESTDIR}$(BINDIR) ${DESTDIR}$(BINDIR)/$(P2M2BINNAME)

${DESTDIR}$(BINDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(BINDIR)/$(P2M2BINNAME): p2m2
	install -c -s p2m2 ${DESTDIR}$(BINDIR)/$(P2M2BINNAME)

clean:
	-rm *.o y.tab.c y.tab.h y.output
