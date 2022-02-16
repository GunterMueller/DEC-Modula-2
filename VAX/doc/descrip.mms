!
!	This was written by CED on 4/22/87 to make sure that dmod$lib
!	had the latest copies of 3 basic documents.
!
INSTALLDIR = dmod$lib:

install : $(INSTALLDIR)vms_mod.txt $(INSTALLDIR)overview.txt\
	$(INSTALLDIR)preprocess.txt $(INSTALLDIR)refman.ps
	CONTINUE

$(INSTALLDIR)vms_mod.txt : vms_mod.txt
	copy vms_mod.txt $(INSTALLDIR)

$(INSTALLDIR)overview.txt : overview.txt
	copy overview.txt $(INSTALLDIR)

$(INSTALLDIR)preprocess.txt : preprocess.txt
	copy preprocess.txt $(INSTALLDIR)

$(INSTALLDIR)refman.psf : refman.psf
	copy refman.psf $(INSTALLDIR)
