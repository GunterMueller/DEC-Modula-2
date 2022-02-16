#
# $Header: Makefile,v 1.17 86/10/21 21:46:47 joel WSL/Rice $
#

TESTLIBDIR = dmod$testlib:
INSTALLDIR = dmod$lib:

.FIRST
	echo := write sys$output

pascal = FALSE
modula2 = TRUE
CFLAGS = /debug
mflags = -r -v -g "-O" "-Dpascal=$(pascal)" "-Dmodula2=$(modula2)"
#mflags = -r -v -g "-Dpascal=$(pascal)" "-Dmodula2=$(modula2)"
MODFLAGS = "-C" $(mflags)

.SUFFIXES :
.SUFFIXES : .exe .olb .obj .mod .def .c
#.SUFFIXES : .exe .olb .obj .mar .pcd .mod .def .c

.def.mod :
	touch $*.mod
.mod.obj :
	mod $(MODFLAGS) -c $*.mod
#.mar.obj :
#	mod $(MODFLAGS) -c $*.mar
#.pcd.obj :
#	mod $(MODFLAGS) -c $*.pcd

PREP_OBJS = prep.obj ASCII.obj Preprocess.obj MemLib.obj memlibc.obj Strings.obj

MOD2_OBJS = mod2(main) mod2(Mod2) mod2(Former) mod2(Inline) mod2(Tokens)\
	mod2(Strings) mod2(Keywords) mod2(Alloc) mod2(Globals) mod2(Decls)\
	mod2(TypeInfo) mod2(Compatible) mod2(TypeDefs) mod2(Consts)\
	mod2(BuildExpr) mod2(BuildStmt) mod2(CheckStmt) mod2(CheckExpr)\
	mod2(InitBuiltin) mod2(CheckBuiltin) mod2(OptBuiltin) mod2(Errors)\
	mod2(Scanner) mod2(Symbols) mod2(iolibc) mod2(IOLib) mod2(Machine)\
	mod2(Optim) mod2(OCount) mod2(OTree) mod2(PCodeOps) mod2(memlibc)\
	mod2(Preprocess) mod2(mod2_gram) mod2(MemLib)\
	mod2(GenCode=GenCodePC.obj) mod2(GenPC) mod2(PCode)\
	mod2(SymbolDump=SymbolDumpPC) mod2(BuiltinPC)

PAS_OBJS = pas(main) pas(Mod2) pas(Former) pas(Inline) pas(Tokens)\
	pas(Strings) pas(Keywords) pas(Alloc) pas(Globals) pas(Decls)\
	pas(TypeInfo) pas(Compatible) pas(TypeDefs) pas(Consts)\
	pas(BuildExpr) pas(BuildStmt) pas(CheckStmt) pas(CheckExpr)\
	pas(InitBuiltin) pas(CheckBuiltin) pas(OptBuiltin) pas(Errors)\
	pas(Scanner) pas(Symbols) pas(iolibc) pas(IOLib) pas(Machine)\
	pas(Optim) pas(OCount) pas(OTree) pas(PCodeOps) pas(memlibc)\
	pas(pas_gram) pas(MemLib)\
	pas(GenCode=GenCodePC.obj) pas(GenPC) pas(PCode)\
	pas(SymbolDump=SymbolDumpPC) pas(BuiltinPC)

all : $(TESTLIBDIR)mod2.exe $(TESTLIBDIR)prep.exe
	CONTINUE

debug :
	mod -v -o $(TESTLIBDIR)mod2.exe/debug []mod2.olb/include=mod2 \
	[]mod2.olb/lib

Scanner.obj :	Scanner.def Scanner.mod
	mod -c $(mflags) Scanner.mod

$(TESTLIBDIR)mod2.exe : $(MOD2_OBJS)
	mod -v -o $@ []mod2.olb/include=mod2 []mod2.olb/lib

$(TESTLIBDIR)pas.exe : $(PAS_OBJS)
	mod -o $@ []pas.olb/include=mod2 []pas.olb/lib

$(TESTLIBDIR)prep.exe : $(PREP_OBJS)
	mod -o $@ $(PREP_OBJS)

# @inline procedure depencies
BuiltinPC.obj :	PCode.mod
GenPC.obj :	PCode.mod
SymbolDumpPC.obj :	PCode.mod

iolibc.c :	IOLib.def
	@ echo "Please change iolibc.c to match IOLib.def"
iolibc.obj :	iolibc.c
main.obj :	main.c
memlibc.c :	MemLib.def
	@ echo "Please change memlibc.c to match MemLib.def"
memlibc.obj :	memlibc.c

mod2_gram.obj : mod2_gram.c

mod2_gram.c : mod2.gram
	@ echo "Should be 10 shift/reduce and 18 reduce/reduce conflicts"
	yacc -vd mod2.gram
	mv y_tab.c mod2_gram.c

pas_gram.obj : pas_gram.c

pas_gram.c : pas.gram
	@ echo "Should be 3 shift/reduce and no reduce/reduce conflicts"
	yacc -vd pas.gram
	mv y_tab.c pas_gram.c
clean :
	- delete *.obj;*
	purge/log

install : $(INSTALLDIR)mod2.exe $(INSTALLDIR)prep.exe
	CONTINUE

$(INSTALLDIR)mod2.exe : $(TESTLIBDIR)mod2.exe
	cp $(TESTLIBDIR)mod2.exe $(INSTALLDIR)mod2.exe

$(INSTALLDIR)prep.exe : $(TESTLIBDIR)prep.exe
	cp $(TESTLIBDIR)prep.exe $(INSTALLDIR)prep.exe
