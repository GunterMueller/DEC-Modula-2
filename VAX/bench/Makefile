#
#	Updated by C. E. Doucette on 5/26/87
#
#	NOTE : Since every source file has the name bench,
#		the file name bench.o is reused for each language.
#


REMOTEDIR = ophion::'[.m2.bench]'

PROGS = benchc benchv benchg benchm benchp benchwp

all: $(PROGS)

benchb: bench.b32
	@echo A Bliss-32 compiler is only available on VMS, not on ULTRIX.

benchc: bench.c
	cc -O -o benchc bench.c

benchm: bench.mod
	../mod/mod -B../lib -O -o benchm bench.mod

benchp: bench.p
	-pc -w -O -o benchp bench.p

benchwp: bench.p
	../mod/wrlpc -B../lib -O -o benchwp bench.p

benchv: bench.c
	-vcc -O -o benchv bench.c

benchg: bench.c
	-gcc -O -o benchg bench.c

vmscopy:
	dcp -v bench.wirth bench.c bench.mod bench.p descrip.mms bench.b32 \
	$(REMOTEDIR)

clean:
	-rm $(PROGS) *.o
