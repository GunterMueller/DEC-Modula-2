MOD = ../mod/mod -B../lib
ALL=	strings bitops float forloop lib array nocount pack mark process\
	mem mems lib-s
REMOTEDIR= ophion::'[.m2.test]'

all:	$(ALL) clean descrip.mms
test:	$(ALL)

descrip.mms : Makefile
	@echo "Make sure $@ is up to date."

strings:	strings.mod
	$(MOD) -o strings -C -g strings.mod
	@echo strings will complain if errors in string module
	./strings
bitops:	bitops.mod
	$(MOD) -o bitops -C -g bitops.mod
	@echo bitops will complain if errors in bitoperations implementation
	./bitops
float:	float.mod
	$(MOD) -o float -C -g float.mod
	@echo float will complain if errors in floating conversion
	./float
forloop:	forloop.mod
	$(MOD) -o forloop -C -g forloop.mod
	@echo forloop will complain if errors in char for loop
	./forloop
lib:	lib.mod
	@echo lib should generate no compile errors
	$(MOD) -o lib -C -g lib.mod
lib-s:	lib-s.mod
	@echo lib-s should generate no compile errors
	$(MOD) -o lib-s -s -C -g lib-s.mod
array:	array.mod
	@echo 'compiler should detect 4 errors indicating you can only use'
	@echo 'open arrays in parameters, 1 error indicating you can't pass'
	@echo '@NOCOUNT open arrays as value parameters'
	-$(MOD) -o array -C -g array.mod
	touch array
nocount:	nocount.mod
	@echo nocount should generate no compile errors
	@echo look at code to see if right thing is happening
	$(MOD) -S -g nocount.mod
	touch nocount
pack:	pack.mod
	@echo pack should generate no compile errors
	@echo look at code to see if right thing is happening
	$(MOD) -o pack -C -g pack.mod
mark:	mark.mod
	@echo mark should generate no compile errors
	$(MOD) -o mark -C -g mark.mod
process:	process.mod
	@echo process should run and generate a bunch of output
	$(MOD) -o process -C -g process.mod
	./process
mem:	mem.mod
	@echo mem should run and generate a bunch of output
	$(MOD) -o mem -C -g mem.mod
	./mem
mems:	mems.mod
	@echo mems should run and generate a bunch of output
	$(MOD) -o mems -C -g mems.mod
	./mems
clean:
	-rm $(ALL) *.o *.s

vmscopy:
	dcp -v *.mod descrip.mms $(REMOTEDIR)
