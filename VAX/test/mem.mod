module testmem;
from system import tsize, adr;
from memory import allocate, deallocate;
from io import writef, output;

const
    MAXSIZE = 1000;
    ACTUALSIZE = 100;
type
    ArrayPtr = pointer to array [0..MAXSIZE] of integer;
var
    ap1, ap2 : ArrayPtr;
    api1, api2 : pointer @nocheck to integer;
    i : cardinal;
begin
    allocate(ap1,ACTUALSIZE*tsize(integer));
    api1 := adr(ap1^) - 4;
    for i := 0 to ACTUALSIZE do
	ap1^[i] := i;
    end;
    writef(output,"ap1=%x api1^=%x ap1^[1]=%x\n",integer(ap1),api1^,ap1^[1]);

    allocate(ap2,ACTUALSIZE*tsize(integer));
    api2 := adr(ap2^) - 4;
    for i := 0 to ACTUALSIZE do
	ap2^[i] := i;
    end;
    writef(output,"ap2=%x api2^=%x ap2^[1]=%x\n",integer(ap2),api2^,ap2^[1]);
    deallocate(ap2,ACTUALSIZE*tsize(integer));
    deallocate(ap1,ACTUALSIZE*tsize(integer));
    writef(output,"ap1=%x\n",integer(ap1));
    writef(output,"ap2=%x\n",integer(ap2));

    new(ap1);
    api1 := adr(ap1^) - 4;
    for i := 0 to MAXSIZE do
	ap1^[i] := i;
    end;
    writef(output,"ap1=%x api1^=%x ap1^[1]=%x\n",integer(ap1),api1^,ap1^[1]);
    new(ap2);
    api2 := adr(ap2^) - 4;
    for i := 0 to MAXSIZE do
	ap2^[i] := i;
    end;
    writef(output,"ap2=%x api2^=%x ap2^[1]=%x\n",integer(ap2),api2^,ap2^[1]);
    dispose(ap2);
    dispose(ap1);
    writef(output,"ap1=%x\n",integer(ap1));
    writef(output,"ap2=%x\n",integer(ap2));
end testmem.
