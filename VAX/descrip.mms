#
#	Updated for VMS by C. E. Doucette on 4/16/87
#
# Note : the logical name was changed from NEWMOD -> DMOD on 4/28/87
#	by Greg Keller who wrote the KITINSTAL.COM procedure.
#
#	This file may depend on logicals defined in M2.COM and M2_AUX.COM
#	I could do a .FIRST etc., but it is only needed once.
#
#	On 8/17/87 I decided to change the directory, COM file,
#	and source-tree save-set from NEWMOD -> M2. I won't touch
#	the kit name (DMOD).
#
#	On 3/24/88, I made a) the tests (real & scratch) be done before
#	installation; and, b) I keep the previous version (purge/keep=2)
#	in $(TESTLIBDIR) of the compiler etc. just in case.
#
#	On 4/5/88, I changed all so that xlate was made before mod2.
#	See Makefile for an attempted explanation.
#
LOGICAL = dmod
CURRENT_VER = 011
TESTLIBDIR = $(LOGICAL)$testlib:
INSTALLDIR = $(LOGICAL)$lib:
KITFILEDIR = $(LOGICAL)$kit:
CURRENT_KITFILE = $(LOGICAL)$(CURRENT_VER).a
BACKUPFILE = [-]m2.bak

.FIRST
#	mod := $$(INSTALLDIR)mod
	pushd := set def
	popd := set def [-]
	make := mms
	cp := copy/log
	mv := rename/log
	echo := write sys$output

all :
	@ pushd [.mod]
	@ pwd
	make/MACRO="TESTLIBDIR=$(TESTLIBDIR)"
	@ popd
	@ pushd [.xlate]
	@ pwd
	make
	@ popd
	@ pushd [.mod2]
	@ pwd
	make
	@ popd
	@ pushd [.lib]
	@ pwd
	make
	@ popd

$(INSTALLDIR)kitinstal.com : kitinstal.com
	cp kitinstal.com $(INSTALLDIR)

install : all purge $(INSTALLDIR)kitinstal.com
	@ echo "Before installation - lets run through the *real* tests..."
	@ pushd [.test]
	@ pwd
	make
	@ popd
#	@ echo "Before installation - lets run through the *scratch* tests..."
#	@ pushd [.scratch]
#	@ pwd
#	make
#	@ popd
	@ pushd [.doc]
	@ pwd
	make/MACRO="INSTALLDIR=$(INSTALLDIR)" install
	@ popd
	@ pushd [.mod]
	@ pwd
	make/MACRO="INSTALLDIR=$(INSTALLDIR)" install
	@ popd
	@ pushd [.mod2]
	@ pwd
	make/MACRO="INSTALLDIR=$(INSTALLDIR)" install
	@ popd
	@ pushd [.xlate]
	@ pwd
	make/MACRO="INSTALLDIR=$(INSTALLDIR)" install
	@ popd
	@ pushd [.lib]
	@ pwd
	make/MACRO="INSTALLDIR=$(INSTALLDIR)" install
	@ popd
#	@ pushd [.vaxc]
#	@ pwd
#	make/MACRO="INSTALLDIR=$(INSTALLDIR)" install
#	@ popd

kit :
	@ echo "Making the current kit file..."
	@ pushd $(INSTALLDIR)
	@ pwd
	purge/log
	@ backup *.*;*/exclude=(*.dir;*) -
		$(KITFILEDIR)$(CURRENT_KITFILE)/save_set
	@ echo "The current kit file has been made."
	@ popd
	@ pushd $(KITFILEDIR)
	purge/log
	@ echo "A new save set for the DEC WRL Modula-2 library has been made."
	@ popd

clean :
	@ pushd [.test]
	@ pwd
	make clean
	@ popd
	@ pushd [.scratch]
	@ pwd
	make clean
	@ popd
	@ pushd [.mod]
	@ pwd
	make clean
	@ popd
	@ pushd [.mod2]
	@ pwd
	make clean
	@ popd
	@ pushd [.xlate]
	@ pwd
	make clean
	@ popd
	@ pushd [.lib]
	@ pwd
	make clean
	@ popd

purge :
	purge/log
	@ pushd [.test]
	@ pwd
	purge/log
	@ popd
	@ pushd [.scratch]
	@ pwd
	purge/log
	@ popd
	@ pushd [.doc]
	@ pwd
	purge/log
	@ popd
	@ pushd [.mod]
	@ pwd
	purge/log
	@ popd
	@ pushd [.mod2]
	@ pwd
	purge/log
	@ popd
	@ pushd [.xlate]
	@ pwd
	purge/log
	@ popd
	@ pushd [.lib]
	@ pwd
	purge/log/keep=2 *.exe
	purge/log/exclude=*.exe *.*
	@ popd
#	@ pushd [.vaxc]
#	@ pwd
#	purge/log
#	@ popd

srckit :
	@ echo "Starting to make $(BACKUPFILE) (of source tree)..."
	@ BACKUP [...] $(BACKUPFILE)/SAVE_SET
	@ echo "All done making $(BACKUPFILE)."
