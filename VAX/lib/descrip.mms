#
# $Header: descrip.mms,v 1.1 89/09/12 17:05:13 joel Locked $
# Updated for VMS by CED on 3/21/87
#
INSTALLDIR = dmod$lib:

.SUFFIXES :
.SUFFIXES :	.olb .obj .mar .mod .def .c

CFLAGS = /DEBUG/OPTIMIZE
MFLAGS = /DEBUG
MODFLAGS = "-O" -g

DEFS = Storage.def math.def longmath.def parameters.def strings.def unix.def\
	errno.def ASCII.def x.def
PDEFS = SYSTEM.def Memory.def IO.def BitOperations.def

OBJS =	modlib(runtime) modlib(mem) modlib(udiv) modlib(nam) modlib(coroutine)\
	modlib(count) modlib(errno)
MOBJS =	modlib(math) modlib(longmath) modlib(parameters) modlib(strings)\
	modlib(ASCII) modlib(x)

.def.mod :
	touch $*.mod

.mod.obj :
	mod -c $(MODFLAGS) $*.mod

#
#	If you have version T2.3 of VAX C RTL (not the compiler - the RTL - it
#	comes with VMS v4.6) or later, it includes a system()
#	function call. This call must be provided in either modlib.olb or
#	vaxcrtl.olb because the unix module expects it and it must exist to
#	compile and link [-.mod]mod.c. That is what happens with no version
#	of system() in either object library. If you have multiple versions
#	(one in each library), you will get linker warnings (not errors) on
#	each attempted link operation that references both libraries.
#	This is not a problem - but should be avoided. The two versions of
#	system() should be identical.
#
#	So, in short, follow the comments below to make sure that the
#	appropriate MMS dependencies exist for your version of the VAX C RTL
#	(and will therefore make the appropriate version of MODLIB).
#
#	If you have VAX C RTL v2.2 or less, uncomment these lines:
#all : $(OBJS) $(MOBJS) modlib(system)
#	CONTINUE
#
#	If you have VAX C RTL v2.3 or greater, uncomment these lines:
all : $(OBJS) $(MOBJS)
	CONTINUE

clean :
	- delete *.obj;*
	purge/log

install : $(INSTALLDIR)modlib.olb
	CONTINUE

$(INSTALLDIR)modlib.olb : modlib.olb $(DEFS) $(PDEFS)
	cp $? $(INSTALLDIR)
