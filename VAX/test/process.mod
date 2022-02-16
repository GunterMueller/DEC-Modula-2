module testprocess;
from system import NewProcess, Coroutine, Transfer, Word, TSize;
from io import writef, output;
const
    STACKSIZE = 1000;
    NUMPROCESSES = 3;
    NUMTIMES = 3;
type
    StackRec = array [1..STACKSIZE] of Word;
    Stack = pointer to StackRec;
    Name = array [1..10] of char;
    ProcessId = [0..NUMPROCESSES-1];
var
    (* CED 4/10/87 - p->q because it won't compile on VMS *)
    q : array ProcessId of Coroutine;
    s : array ProcessId of Stack;
    parent : Coroutine;
    paramname : Name;
    paramid : ProcessId;
procedure P();
var
    myname : Name;
    myid : ProcessId;
    count : integer;
begin
    myname := paramname;
    myid := paramid;
    Transfer(q[myid],parent);
    for count := 1 to NUMTIMES do
	writef(output,"%s %d %d\n",myname,myid,count);
	Transfer(q[myid],q[(myid+1) mod NUMPROCESSES]);
    end;
	writef(output,"%s %d done\n",myname,myid);
    Transfer(q[myid],parent);
end P;
var
    i : ProcessId;
begin
    for i := first(ProcessId) to last(ProcessId) do
	New(s[i]);
	NewProcess(P,s[i],TSize(StackRec),q[i]);
	case i of
	| 0 : paramname := "Larry";
	| 1 : paramname := "Curly";
	| 2 : paramname := "Moe";
	else
	    paramname := "?";
	end;
	paramid := i;
	Transfer(parent,q[i]);
    end;
    Transfer(parent,q[0]);
    writef(output,"That's all, folks!\n");
end testprocess.
