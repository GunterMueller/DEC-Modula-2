# $Header: Makefile,v 1.7 88/05/16 17:29:25 joel Experimental $

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


DOCS= installation.f overview.f mod.f p2m2.f announcement.f pcode.doc.f \
	modprof.f sigplan84.f
OLDDOCS = array.f inline.f m+c+p.f 
VMSDOCS = vms_mod.txt vms_mod.f overview.txt refman.psf

REMOTENODE = ophion
REMOTEDIR = '[.m2.doc]'

all: docs
#all: docs vmsdocs

docs:		$(DOCS)
olddocs:	$(OLDDOCS)
vmsdocs:	$(VMSDOCS)

.SUFFIXES:
.SUFFIXES: .txt .f .l .d
.l.f:
	nroff -man $*.l > $*.f
.d.f:
	nroff -ms $*.d > $*.f

pcode.doc.f:
	tbl pcode.doc | nroff -ms >pcode.doc.f
sigplan84.f: sigplan84
	tbl sigplan84 | nroff -ms > sigplan84.f

quickinstall: $(DESTDIR)$(MANDIR)
	rdist -c mod.l     localhost:$(DESTDIR)$(MANDIR)/$(MODBINNAME).l
	rdist -c p2m2.l    localhost:$(DESTDIR)$(MANDIR)/$(P2M2BINNAME).l
	rdist -c modprof.l localhost:$(DESTDIR)$(MANDIR)/$(MODPROFBINNAME).l

install: $(DESTDIR)$(MANDIR) \
	 $(DESTDIR)$(MANDIR)/$(MODBINNAME).l \
	 $(DESTDIR)$(MANDIR)/$(P2M2BINNAME).l \
	 $(DESTDIR)$(MANDIR)/$(MODPROFBINNAME).l

$(DESTDIR)$(MANDIR):
	-test -d $@ || mkdir $@

$(DESTDIR)$(MANDIR)/$(MODBINNAME).l: mod.l
	rdist -c mod.l     localhost:$(DESTDIR)$(MANDIR)/$(MODBINNAME).l

$(DESTDIR)$(MANDIR)/$(P2M2BINNAME).l: p2m2.l
	rdist -c p2m2.l    localhost:$(DESTDIR)$(MANDIR)/$(P2M2BINNAME).l

$(DESTDIR)$(MANDIR)/$(MODPROFBINNAME).l: modprof.l
	rdist -c modprof.l localhost:$(DESTDIR)$(MANDIR)/$(MODPROFBINNAME).l

# Would be nice to remove _ <bs> sequences

overview.txt: overview.f
	cp overview.f overview.txt
vms_mod.l: mod.l
	@echo Be sure to update vms_mod.l to match mod.l
vms_mod.txt: vms_mod.f
	cp vms_mod.f vms_mod.txt
	
vmscopy: $(VMSDOCS)
	dcp -v $(VMSDOCS) $(REMOTENODE)::$(REMOTEDIR)

clean:
	-rm *.f *.txt

checkin:
	ci -l -q -m"Gatekeeper release 22 June 1990" \
	-s"Release" *.d *.doc *.l Makefile
