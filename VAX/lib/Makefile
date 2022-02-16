# $Header: Makefile,v 1.17 89/09/12 17:05:10 joel Locked $

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

REMOTEDIR = ophion::'[.m2.lib]'

.SUFFIXES:
.SUFFIXES: .o .mod .def .c .a .m

CC = cc
CFLAGS = -g

MOD = ../mod/mod
MODFLAGS = -a -g -O -B../lib

DEFS = Storage.def math.def longmath.def parameters.def strings.def unix.def \
	ASCII.def x.def
PDEFS = SYSTEM.def Memory.def IO.def BitOperations.def
OBJS = runtime.o mem.o udiv.o coroutine.o count.o NAM.o
MOBJS =	math.o longmath.o parameters.o strings.o unix.o ASCII.o x.o
IOOBJS = iocvtl.o ioptab.o iomult.o iowc.o iowid.o iowle.o iowlf.o iows.o\
	iowud.o iowuio.o iowuix.o iocvtr.o iowrf.o iowre.o
IOMOBJS = ioimp.o

#
#	To automatically update your local VMS node, just remove all
#	of the following comment characters ("^#") and comment out
#	the current definition of the target all.
#

.def.mod:
	touch $*.mod

.mod.o:
	$(MOD) -c -pg ${MODFLAGS} $*.mod
	mv $*.o prof
	$(MOD) -c ${MODFLAGS} $*.mod

.c.o:
	$(CC) -c -pg ${CFLAGS} $*.c
	mv $*.o prof
	$(CC) -c ${CFLAGS} $*.c

.a.o:
	as -o $*.o $*.a

.m.o:
	mc $*.m

all:	libmod.a libmod_p.a udiv.mar coroutine.mar descrip.mms

testf: testf.o iomult.o ioimp.o iocvtf.o ioftab.o
	mod -g -o testf testf.o iomult.o ioimp.o iocvtf.o ioftab.o
truntime.o:	truntime.c
	tcc -c truntime.c
testf.o:	testf.mod
	mod -c -r -g -C testf.mod
testmult64: testmult64.o iomult64.o ioimp.o
	mod -g -o testmult64 testmult64.o iomult64.o ioimp.o
testmult64.o:	testmult64.mod
	mod -c -r -g -C testmult64.mod
testmult: testmult.o iomult.o ioimp.o
	mod -g -o testmult testmult.o iomult.o ioimp.o
testmult.o:	testmult.mod
	mod -c -r -g -C testmult.mod
ioptab.a:	genptab ioptab.bc
	genptab <ioptab.bc >ioptab.a
genptab:	genptab.mod
	mod -o genptab -r -g -C genptab.mod
genptabin:	genptabin.mod
	mod -o genptabin -r -g -C genptabin.mod
ioptab.bc:	ioptab.bcin
	bc < ioptab.bcin >ioptab.bc
ioptab.bcin:	genptabin
	genptabin > ioptab.bcin
testio:	testio.o iolib
	mod -g -o testio testio.o iolib
testio.o:	testio.mod
	mod -c -r -g -C testio.mod
timerf.o:	timerf.mod
	mod -c -r -g -C timerf.mod
timerf:	timerf.o iolib
	mod -g -o timerf timerf.o iolib
timei.o:	timei.mod
	mod -c -r -g -C timei.mod
timei:	timei.o iolib
	mod -g -o timei timei.o iolib
timef.o:	timef.mod
	mod -c -r -g -C timef.mod
timef:	timef.o iolib
	mod -g -o timef timef.o iolib
rande.o:	rande.mod
	mod -c -r -g -C rande.mod
rande:	rande.o iolib
	mod -g -o rande rande.o iolib
randrf.o:	randrf.mod
	mod -c -r -g -C randrf.mod
randrf:	randrf.o iolib
	mod -g -o randrf randrf.o iolib
randf.o:	randf.mod
	mod -c -r -g -C randf.mod
randf:	randf.o iolib
	mod -g -o randf randf.o iolib
iolib:	$(IOOBJS) $(IOMOBJS)
	-rm iolib
	ar cr iolib $(IOOBJS) $(IOMOBJS)
	ranlib iolib

libmod.a:	$(OBJS) $(MOBJS)
	-rm libmod.a
	ar cr libmod.a $(OBJS) $(MOBJS)
	ranlib libmod.a
libmod_p.a:	$(OBJS) $(MOBJS)
	-rm prof/libmod_p.a libmod_p.a
	cd prof; ar cr libmod_p.a $(OBJS) $(MOBJS)
	cp prof/libmod_p.a libmod_p.a
	ranlib libmod_p.a
udiv.o:	udiv.c
	as -o udiv.o udiv.c
	cp udiv.o prof
udiv.mar: udiv.c
	@echo "Make sure you update $@ and copy it to $(REMOTEDIR)."
coroutine.mar: coroutine.c
	@echo "Make sure you update $@ and copy it to $(REMOTEDIR)."
descrip.mms: Makefile
	@echo "Make sure you update $@ and copy it to $(REMOTEDIR)."
coroutine.o:	coroutine.c
	as -o coroutine.o coroutine.c
	cp coroutine.o prof
clean:
	-rm *.s *.pcd *.o prof/*.o

quickinstall: ${DESTDIR}$(LIBDIR) COPYDEFS
	rdist -c libmod.a libmod_p.a localhost:$(DESTDIR)$(LIBDIR)

install: ${DESTDIR}$(LIBDIR) ${DESTDIR}$(LIBDIR)/libmod.a \
		${DESTDIR}$(LIBDIR)/libmod_p.a COPYDEFS

${DESTDIR}$(LIBDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(LIBDIR)/libmod.a: libmod.a
	rdist -c libmod.a localhost:$(DESTDIR)$(LIBDIR)/libmod.a

${DESTDIR}$(LIBDIR)/libmod_p.a: libmod_p.a
	rdist -c libmod_p.a localhost:$(DESTDIR)$(LIBDIR)/libmod_p.a

COPYDEFS:
	-test -d $(DESTDIR)$(DEFSDIR) || mkdir $(DESTDIR)$(DEFSDIR)
	rdist -c $(DEFS) $(PDEFS) localhost:$(DESTDIR)$(DEFSDIR)

testdir:
	ln -s ../mod/mod mod
	ln -s ../mod2/mod2.0 mod2.0
	ln -s ../mod2/mod2.0t mod2.0t
	ln -s ../pascal/pas2.0 pas2.0
	ln -s ../pascal/pas2.0t pas2.0t
	ln -s ../xlate/mod2.1 mod2.1
	ln -s /usr/local/bin/mc mod2.1t
	ln -s ../imc/mod2.2 mod2.2

checkin:
	ci -l -f -m"WSL/WRL Checkin before VMS changes"\
	-s"WSL/WRL/Release" *.def *.c *.mod Makefile descrip.mms *.mar

vmscopy:
	dcp -v *.def *.c *.mod Makefile descrip.mms *.mar $(REMOTEDIR)
