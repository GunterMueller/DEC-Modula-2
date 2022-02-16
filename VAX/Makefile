#
#	This is the global makefile adopted from makeall & install.
#	This was written by CED on 3/6/87. Hopefully I will
#	never have to change a Makefile (in one of the subdirectories) again!
#
#	NOTE:	If you want this to automatically update your local VMS node,
#		see the comments that follow in this Makefile, and those
#		in mod, mod2, xlate & lib.
#

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

DEFINES = DESTDIR=$(DESTDIR) LIBDIR=$(LIBDIR) PASSESDIR=$(PASSESDIR) \
	  DEFSDIR=$(DEFSDIR) MANDIR=$(MANDIR) BINDIR=$(BINDIR) \
	  MODBINNAME=$(MODBINNAME) PCBINNAME=$(PCBINNAME) \
	  P2M2BINNAME=$(P2M2BINNAME) MODPROFBINNAME=$(MODPROFBINNAME) \
	  TESTLIBDIR=$(TESTLIBDIR)

#
#	To automatically update your local VMS node, uncomment the following
#	section and comment out the section after that.
#
#all : $(SUBMITFILE) descrip.mms
#	(cd mod	    ; make all)
#	(cd lib	    ; make all)
#	(cd mod2    ; make mod2.0)
#	(cd imc	    ; make mod2.2)
#	(cd xlate   ; make mod2.1)
#	(cd test    ; make test)
#	@echo "Submitting $(SUBMITFILE) to $(REMOTENODE) for MMS on VMS."
#	@dcp -S $(SUBMITFILE) $(REMOTENODE)

all :
	(cd doc     ; make $(DEFINES) all)
	(cd mod	    ; make $(DEFINES) all)
	(cd lib	    ; make $(DEFINES) all)
	(cd mod2    ; make $(DEFINES) mod2.0)
#	(cd mod2    ; make $(DEFINES) mod2.0 mod2.0t)
	(cd pascal  ; make $(DEFINES) pas2.0)
#	(cd pascal  ; make $(DEFINES) pas2.0 pas2.0t)
	(cd imc	    ; make $(DEFINES) all)
	(cd xlate   ; make $(DEFINES) all)
	(cd p2m2    ; make $(DEFINES) all)
	(cd modprof ; make $(DEFINES) all)
	(cd bench   ; make $(DEFINES) -i all)
#	(cd test    ; make $(DEFINES) all)

quickinstall:
	(cd doc	    ; make $(DEFINES) quickinstall)
	(cd mod	    ; make $(DEFINES) quickinstall)
	(cd lib	    ; make $(DEFINES) quickinstall)
	(cd mod2    ; make $(DEFINES) quickinstallmod2.0)
#	(cd mod2    ; make $(DEFINES) quickinstallmod2.0 quickinstallmod2.0t)
	(cd pascal  ; make $(DEFINES) quickinstallpas2.0)
#	(cd pascal  ; make $(DEFINES) quickinstallpas2.0 quickinstallpas2.0t)
	(cd imc	    ; make $(DEFINES) quickinstall)
	(cd xlate   ; make $(DEFINES) quickinstall)
	(cd p2m2    ; make $(DEFINES) quickinstall)
	(cd modprof ; make $(DEFINES) quickinstall)

install :
	(cd doc	    ; make $(DEFINES) install)
	(cd mod	    ; make $(DEFINES) install)
	(cd lib	    ; make $(DEFINES) install)
	(cd mod2    ; make $(DEFINES) installmod2.0)
#	(cd mod2    ; make $(DEFINES) installmod2.0 installmod2.0t)
	(cd pascal  ; make $(DEFINES) installpas2.0)
#	(cd pascal  ; make $(DEFINES) installpas2.0 install pas2.0t)
	(cd imc	    ; make $(DEFINES) install)
	(cd xlate   ; make $(DEFINES) install)
	(cd p2m2    ; make $(DEFINES) install)
	(cd modprof ; make $(DEFINES) install)

release:
	maketar m2.tar
	-rm m2.tar.Z
	compress m2.tar

clean :
	(cd doc	    ; make $(DEFINES) clean)
	(cd mod	    ; make $(DEFINES) clean)
	(cd lib	    ; make $(DEFINES) clean)
	(cd mod2    ; make $(DEFINES) clean)
	(cd pascal  ; make $(DEFINES) clean)
	(cd imc	    ; make $(DEFINES) clean)
	(cd xlate   ; make $(DEFINES) clean)
	(cd p2m2    ; make $(DEFINES) clean)
	(cd modprof ; make $(DEFINES) clean)
	(cd bench   ; make $(DEFINES) clean)

checkin :
	(cd doc     ; make $(DEFINES) checkin)
	(cd mod	    ; make $(DEFINES) checkin)
	(cd lib	    ; make $(DEFINES) checkin)
	(cd mod2    ; make $(DEFINES) checkin)
# don't checkin pascal, uses same files as mod2
	(cd imc	    ; make $(DEFINES) checkin)
	(cd xlate   ; make $(DEFINES) checkin)
	(cd p2m2    ; make $(DEFINES) checkin)
	(cd modprof ; make $(DEFINES) checkin)

$(SUBMITFILE) : Makefile
	@echo "Re-creating the submit file: $@"
	@echo \$$ SET VERIFY > $@
	@echo \$$ SET DEFAULT $(REMOTEDIR) >> $@
	@echo \$$ "@NEWMOD" >> $@
	@echo \$$ MMS/OUTPUT=MAKE.LOG >> $@
	@echo \$$ MAIL MAKE.LOG $${HOST}::$${USER} /SUBJECT='$(SUBJECT)' >>$@
	@echo \$$ SET NOVERIFY >> $@

descrip.mms : Makefile
	@echo "Make sure you update $@ and copy it to $(REMOTEDIR)."
