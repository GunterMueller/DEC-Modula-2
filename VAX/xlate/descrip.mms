#
# $Header: Makefile,v 1.17 86/10/21 21:46:47 joel WSL/Rice $
# Updated by CED on 3/6/87. Updated for VMS on 3/24/87.
#
# Because VMS file names are all caps:
# NOTE: machine.c   -> machinec.c   so as not to be confused with Machine.mod
# NOTE: hash.mod    -> makehash.mod    so as not to be confused with Hash.mod
#
TESTLIBDIR = dmod$testlib:
INSTALLDIR = dmod$lib:

.SUFFIXES :
.SUFFIXES : .exe .olb .obj .mod .def .c

pascal = FALSE
modula2 = TRUE
CFLAGS = /OPTIMIZE /DEBUG
MODFLAGS = -v -r -g "-O"

.def.mod :
	touch $*.mod

.mod.obj :
	mod $(MODFLAGS) -c $*.mod

all : $(TESTLIBDIR)xlate.exe
	continue

OBJS =	xlate(CodeSubs) xlate(Consts) xlate(EES) xlate(Error) xlate(Input) \
	xlate(Machine) xlate(MemRef) xlate(OpSubs) xlate(Types) xlate(Output) \
	xlate(Util) xlate(Vars) xlate(Xlate) \
	xlate(ASCII) xlate(PCodeOps) xlate(Hash) xlate(machinec) xlate(Symbol)

$(TESTLIBDIR)xlate.exe : $(OBJS)
	mod -o $@ []xlate.olb/include=machinec []xlate.olb/lib

clean :
	- delete *.obj;*
	purge/log

# hash table function generation
makehash.exe :	makehash.mod PCodeOps.def PCodeOps.mod PCodeOps.obj
	mod $(MODFLAGS) -o $*.exe $*.mod PCodeOps.obj
Input.mod :	Hash.def
Hash.def :	PCodeOps.def PCodeOps.mod makehash.exe
	run makehash.exe
Hash.mod :	PCodeOps.def PCodeOps.mod

# inline dependencies
CodeSubs.obj :	Output.obj Util.obj
EES.obj	: Output.obj
MemRef.obj :	Output.obj Util.obj
OpSubs.obj :	Output.obj Util.obj

install : $(INSTALLDIR)xlate.exe
	CONTINUE

$(INSTALLDIR)xlate.exe : $(TESTLIBDIR)xlate.exe
	cp $(TESTLIBDIR)xlate.exe $(INSTALLDIR)xlate.exe
