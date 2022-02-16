# $Header: Makefile,v 1.11 88/04/27 18:30:27 joel WSL/WRL/Release $

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

CFLAGS=	-g
CC= cc

all:	mod imod wrlpc iwrlpc

mod:	mod.c
	${CC} -o mod -DLIBDIR='"${TESTLIBDIR}"' \
		-DPASSESDIR='"${TESTLIBDIR}"' \
		-DDEFSDIR='"${TESTLIBDIR}"' \
		-DPASCAL=0 -DMODULA2=1 $(CFLAGS) mod.c
imod:	mod.c
	${CC} -o imod -DLIBDIR='"${DESTDIR}${LIBDIR}"' \
		-DPASSESDIR='"${DESTDIR}${PASSESDIR}"' \
		-DDEFSDIR='"${DESTDIR}${DEFSDIR}"' \
		-DPASCAL=0 -DMODULA2=1 $(CFLAGS) mod.c

wrlpc:	mod.c
	${CC} -o wrlpc -DLIBDIR='"${TESTLIBDIR}"' \
		-DPASSESDIR='"${TESTLIBDIR}"' \
		-DPASCAL=1 -DMODULA2=0 $(CFLAGS) mod.c

iwrlpc:	mod.c
	${CC} -o iwrlpc -DLIBDIR='"${DESTDIR}${LIBDIR}"' \
		-DPASSESDIR='"${DESTDIR}${PASSESDIR}"' \
		-DPASCAL=1 -DMODULA2=0 $(CFLAGS) mod.c

quickinstall: ${DESTDIR}$(BINDIR) ${DESTDIR}$(PASSESDIR)
	install -c -s imod ${DESTDIR}$(BINDIR)/$(MODBINNAME)
	rm -f ${DESTDIR}$(PASSESDIR)/mod
	ln -s ${DESTDIR}$(BINDIR)/$(MODBINNAME) ${DESTDIR}$(PASSESDIR)/mod
	install -c -s iwrlpc ${DESTDIR}$(BINDIR)/$(PCBINNAME)

install: ${DESTDIR}$(BINDIR) ${DESTDIR}$(PASSESDIR) \
		${DESTDIR}$(BINDIR)/$(MODBINNAME) \
		${DESTDIR}$(PASSEDIR)/mod \
		${DESTDIR}$(BINDIR)/$(PCBINNAME)

${DESTDIR}$(BINDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(PASSESDIR):
	-test -d $@ || mkdir $@

${DESTDIR}$(BINDIR)/$(MODBINNAME): imod
	install -c -s imod ${DESTDIR}$(BINDIR)/$(MODBINNAME)

${DESTDIR}$(PASSEDIR)/mod:
	rm -f ${DESTDIR}$(PASSESDIR)/mod
	ln -s ${DESTDIR}$(BINDIR)/$(MODBINNAME) ${DESTDIR}$(PASSESDIR)/mod

${DESTDIR}$(BINDIR)/$(PCBINNAME): iwrlpc
	install -c -s iwrlpc ${DESTDIR}$(BINDIR)/$(PCBINNAME)

vmscopy:
	dcp -v mod.c descrip.mms $(REMOTEDIR)

descrip.mms : Makefile
	@echo "Make sure you update $@ and copy it to $(REMOTEDIR)."

clean:
	-rm *.o mod imod wrlpc iwrlpc

checkin:
	ci -l -f  -m"WSL/WRL Release 28 Apr 1988"\
	-s"WSL/WRL/Release" mod.c Makefile  descrip.mms
