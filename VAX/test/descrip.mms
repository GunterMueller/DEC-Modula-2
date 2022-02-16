#
#	Updated for VMS by CED on 4/8/87
#
.FIRST
	mod := $dmod$testlib:mod
	echo := write sys$output

ALL = 	strings.exe bitops.exe float.exe forloop.exe lib.exe array.exe\
	nocount.exe pack.exe mark.exe process.exe mem.exe mems.exe\
	lib-s 

test :	clean all
	CONTINUE
all :	$(ALL)
	CONTINUE

strings.exe :	strings.mod
	mod -o strings.exe "-C" -g strings.mod
	@ echo "strings will complain if errors in strings module"
	run strings
bitops.exe :	bitops.mod
	mod -o bitops.exe "-C" -g bitops.mod
	@ echo "bitops will complain if errors in bitoperations implementation"
	run bitops
float.exe :	float.mod
	mod -o float.exe "-C" -g float.mod
	@ echo "float will complain if errors in floating conversion"
	run float
forloop.exe :	forloop.mod
	mod -o forloop.exe "-C" -g forloop.mod
	@ echo "forloop will complain if errors in char for loop"
	run forloop
lib.exe :	lib.mod
	@ echo "lib should generate no compile errors"
	mod -o lib.exe "-C" -g lib.mod
lib-s :	lib-s.mod
	@ echo "lib-s should generate no compile errors"
	mod -o lib-s -s "-C" -g lib-s.mod
array.exe :	array.mod
	@ echo "compiler should detect 4 errors indicating you can only use"
	@ echo "open arrays in parameters, 1 error indicating you can't pass"
	@ echo "@NOCOUNT open arrays as value parameters"
	- mod -o array.exe "-C" -g array.mod
	- touch array.exe
nocount.exe :	nocount.mod
	@ echo "nocount should generate no compile errors"
	@ echo "look at code to see if right thing is happening"
	mod "-S" -g nocount.mod
	touch nocount.exe
pack.exe :	pack.mod
	@ echo "pack should generate no compile errors"
	@ echo "look at code to see if right thing is happening"
	mod -o pack "-C" -g pack.mod
mark.exe :	mark.mod
	@ echo "mark should generate no compile errors"
	mod -o mark "-C" -g mark.mod
process.exe :	process.mod
	@ echo "process should run and generate a bunch of output"
	mod -o process.exe -g process.mod
	run process
mem.exe :	mem.mod
	@ echo "mem should run and generate a bunch of output"
	mod -o mem.exe "-C" -g mem.mod
	run mem
mems.exe :	mems.mod
	@ echo "mems should run and generate a bunch of output"
	mod -o mems.exe "-C" -g mems.mod
	run mems
clean :
	- delete *.exe;*
	- delete *.obj;*
	- delete *.mar;*
