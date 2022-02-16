module bench;

(*	From PASCAL Source File: bench.pas       *)



(*  This is a suite of benchmarks that are relatively short, both in program
    size and execution time.  It requires no input, and prints the execution
    time for each program, using the system- dependent routine Getclock,
    below, to find out the current CPU time.  It does a rudimentary check to
    make sure each program gets the right output.  These programs were
    gathered by John Hennessy and modified by Peter Nye. *)

from system import cputime;
from io import writef, output;

const
      bubblebase = 1.61;
      dnfbase = 3.5;
      permbase = 1.75;
      queensbase = 1.83;
      towersbase = 2.39;
      quickbase = 1.92;
      intmmbase = 1.46;
      treebase =  2.5;
      mmbase = 0.73;
      fpmmbase = 2.92;
      puzzlebase = 0.5;
      fftbase = 1.11;
      fpfftbase = 4.44;

    (* Towers *)
    maxcells = 18;

    (* Intmm, Mm *)
    rowsize = 40;

    (* Puzzle *)
    size = 511;
    classmax = 3;
    typemax = 12;
    d = 8;

    (* Bubble, Quick *)
    sortelements = 5000;
    srtelements = 500;

    (* fft *)
    fftsize = 256;
    fftsize2 = 129 ;

type
    (* Perm *)
    permrange = [0 .. 10];

   (* tree *)
    nodeptr = pointer to node;
    node = record
	left,right: nodeptr;
	val:integer;
    end;

    (* Towers *)
    discsizrange = [1..maxcells];
    stackrange = [1..3];
    cellcursor = [0..maxcells];
    element = 
        record
	    discsize:discsizrange;
	    next:cellcursor;
	end;
    emsgtype = (* %PACKED *) array[1..16] of char;

    (* Intmm, Mm *)
    index = [1 .. rowsize];
    intmatrix = array index,index of integer;
    realmatrix = array index,index of real;

    (* Puzzle *)
    piececlass = [0..classmax];
    piecetype = [0..typemax];
    position = [0..size];

    (* Bubble, Quick *)
    listsize = [0..sortelements];
    sortarray = array listsize of integer;

    (* FFT *)
    complex = record  rp: real;
	      ip: real; end; 
    carray = array [1..fftsize] of complex;
    c2array = array [1..fftsize2] of complex;

var
 value: real;
    fixed,floating: real;

    (* global *)
    timer: real;
    xtimes: array[1..10] of real;
    seed: integer;
    i : integer;

    (* Perm *)
    permarray : array permrange of permrange;
    pctr: integer;

    (* tree *)
    tree: nodeptr;

    (* Towers *)
    stack: array stackrange of cellcursor;
    cellspace: array[1..maxcells] of element;
    freelist: cellcursor;
    movesdone: integer;

    (* Intmm, Mm *)
    ima, imb, imr: intmatrix;
    rma, rmb, rmr: realmatrix;

    (* Puzzle *)
    piececount: array piececlass of [0..13];
    class: array piecetype of piececlass;
    piecemax: array piecetype of position;
    puzzl: array position of boolean;
    p: array piecetype,position of boolean;
    m,n: position;
    kount: integer;

    (* Bubble, Quick *)
    sortlist: sortarray;
    biggest, littlest: integer;
    top: listsize;

    (* FFT *)
    z, w : carray;
    e    : c2array;
    zr, zi : real;

(* global procedures *)

procedure Getclock (): real;
    begin
    return (float(cputime ()));
    end Getclock;

procedure Initrand;
    begin
    seed := 74755;
    end Initrand;

procedure Rand (): integer;
    begin
    seed := (seed * 1309 + 13849) mod 65536;
    return (seed);
    end Rand;


procedure Perm;

    (* Permutation program, heavily recursive, written by Denny Brown. *)
    
    var i : permrange;

    procedure Swap(var a,b : permrange);
	var t : permrange;
	begin
	t := a;  a := b;  b := t;
	end Swap;

    procedure Initialize;
	var i : permrange;
	begin
	for i := 1 to 7 do 
	    permarray[i]:=i-1;
	     end;
	end Initialize;

    procedure Permute(n : permrange);
	var k : permrange;
	begin   (* permute *)
	pctr := pctr + 1;
	if n<>1 then  
	    Permute(n-1);
	    for k := n-1 to 1  by -1 do 
		Swap(permarray[n],permarray[k]);
		Permute(n-1);
		Swap(permarray[n],permarray[k]);
		 end;
	     end;
	end Permute;     (* permute *)

    begin   (* Perm *)
    pctr := 0;
    for i := 1 to 5 do 
	Initialize;
	Permute(7);
	 end;
    if pctr <> 43300 then
	writef (output,' Error in Perm.\n'); end;
    end Perm;     (* Perm *)



procedure Towers;

    (*  Program to Solve the Towers of Hanoi *)

    procedure Error (emsg:emsgtype);
	begin
	writef(output,' Error in Towers: %s\n',emsg);
	end Error;

    procedure Makenull (s:stackrange);
	begin
	stack[s]:=0;
	end Makenull;

    procedure Getelement ():cellcursor;
	var nextfree:cellcursor;
	begin
	if freelist>0 then
	    
	    nextfree := freelist;
	    freelist:=cellspace[freelist].next;
	    return (nextfree);
	    
	else
	    Error('out of space   '); end;
	end Getelement;

    procedure Push(i:discsizrange;s:stackrange);

	var
	    errorfound:boolean;
	    localel:cellcursor;
	begin

	errorfound:=false;
	if stack[s] > 0 then
	    if cellspace[stack[s]].discsize<=i then
		
		errorfound:=true;
		Error('disc size error');
		 end; end;
	if not errorfound then
	    
	    localel:=Getelement();
	    cellspace[localel].next:=stack[s];
	    stack[s]:=localel;
	    cellspace[localel].discsize:=i;
	    
	 end; end Push;

    procedure Init (s:stackrange;n:discsizrange);

	var
	    discctr:discsizrange;
	begin
	Makenull(s);
	for discctr:=n to 1  by -1 do
	    Push(discctr,s); end;
	end Init;

    procedure Pop (s:stackrange):discsizrange;

	var
	    popresult:discsizrange;
	    temp:cellcursor;
	begin
	if stack[s] > 0 then
	    
	    popresult:=cellspace[stack[s]].discsize;
	    temp:=cellspace[stack[s]].next;
	    cellspace[stack[s]].next:=freelist;
	    freelist:=stack[s];
	    stack[s]:=temp;
	    return (popresult);
	    
	else
	    Error('nothing to pop '); end;
	end Pop;

    procedure Move (s1,s2:stackrange);
	begin

	Push(Pop(s1),s2);
	movesdone:=movesdone+1;
	end Move;

    procedure Towers(i,j,k:integer);
	var other:integer;
	begin

	if k=1 then
	    Move(i,j);
	else
	    
	    other:=6-i-j;
	    Towers(i,other,k-1);
	    Move(i,j);
	    Towers(other,j,k-1);
	     end;
	end Towers;


    begin (* Towers *)
    for i:=1 to maxcells do
	cellspace[i].next:=i-1; end;
    freelist:=maxcells;
    Init(1,14);
    Makenull(2);
    Makenull(3);
    movesdone:=0;
    Towers(1,2,14);
    if movesdone <> 16383 then
	writef (output,' Error in Towers.\n'); end;
    end Towers; (* Towers *)



procedure Queens;
    (* The eight queens problem, solved 50 times. *)

    var  i:integer;

    procedure Doit;

	type    
	    doubleboard =   [2..16];
	    doublenorm  =   [-7..7];
	    boardrange  =   [1..8];
	    aarray      =   array boardrange of boolean;
	    barray      =   array doubleboard of boolean;
	    carray      =   array doublenorm of boolean;
	    xarray      =   array boardrange of boardrange;

	var     
	    i           :   integer;
	    q           :   boolean;
	    a           :   aarray;
	    b           :   barray;
	    c           :   carray;
	    x           :   xarray;

	procedure Try(i : integer; var q : boolean; var a : barray;
		      var b : aarray);

	    var     j           :   integer;

	    begin
	    j := 0;
	    q := false;
	    WHILE (not q) and (j <> 8) do
		 j := j + 1;
		q := false;
		if b[j] and a[i+j] and c[i-j] then
		     x[i] := j;
		    b[j] := false;
		    a[i+j] := false;
		    c[i-j] := false;
		    if i < 8 then
			 Try(i+1,q,a,b);
			if not q then
			     b[j] := true;
			    a[i+j] := true;
			    c[i-j] := true;
			    
			 end;
		    else q := true;
		     end;
		 end; end;
	    end Try;

	begin
	i := 0 - 7;
	while i <= 16 do
	     if (i >= 1) and (i <= 8) then a[i] := true; end;
	    if i >= 2 then b[i] := true; end;
	    if i <= 7 then c[i] := true; end;
	    i := i + 1;
	     end;

	Try(1, q, b, a);
	if not q then
	    writef (output,' Error in Queens.\n'); end;
	end Doit;
    begin
    for i := 1 to 50 do Doit; end;
    end Queens;



procedure Intmm;

    (* Multiplies two integer matrices. *)
    
    var i, j : index;

    procedure Initmatrix (var m: intmatrix);
	var i, j : index;
	begin
	for i := 1 to rowsize do
	    for j := 1 to rowsize do
		m[i,j] := (Rand() mod 120 - 60); end; end;
	end Initmatrix;

    procedure Innerproduct(var result: integer; var a,b: intmatrix;
			   row,column: index);
	(* computes the inner product of A[row,*] and B[*,column] *)
	var i: index;
	begin
	result := 0;
	for i := 1 to rowsize do  result := result + a[row,i]*b[i,column] end;
	end Innerproduct;

    begin
    Initrand;
    Initmatrix (ima);
    Initmatrix (imb);
    for i := 1 to rowsize do
	for j := 1 to rowsize do Innerproduct(imr[i,j],ima,imb,i,j); end; end;
    end Intmm;


procedure Mm;

    (* Multiplies two real matrices. *)
    
    var i, j : index;

    procedure Initmatrix (var m: realmatrix);
	var i, j : index;
	begin
	for i := 1 to rowsize do
	    for j := 1 to rowsize do
		m[i,j] := Float(Rand() mod 120 - 60)/3.0; end; end;
	end Initmatrix;

    procedure Innerproduct(var result: real; var a,b: realmatrix;
			   row,column: index);
	(* computes the inner product of A[row,*] and B[*,column] *)
	var i: index;
	begin
	result := 0.0;
	for i := 1 to rowsize do  result := result + a[row,i]*b[i,column]; end;
	end Innerproduct;

    begin
    Initrand;
    Initmatrix (rma);
    Initmatrix (rmb);
    for i := 1 to rowsize do
	for j := 1 to rowsize do Innerproduct(rmr[i,j],rma,rmb,i,j); end; end;
    end Mm;



procedure Puzzle;

    (* A compute-bound program from Forest Baskett. *)
    
    var i, j, k, m : integer;

    procedure Fit (i : piecetype; j : position) : boolean;

	var     k       :       position;

	begin
	for k := 0 to piecemax[i] do
	    if p[i,k] then if puzzl[j+k] then return (false); end; end; end;
	return (true);
	end Fit;

    procedure Place (i : piecetype; j : position) : position;

	var     k       :       position;

	begin
	for k := 0 to piecemax[i] do
	    if p[i,k] then puzzl[j+k] := true; end; end;
	piececount[class[i]] := piececount[class[i]] - 1;
	for k := j to size do
	    if not puzzl[k] then return (k); end; end;
	return (0);
	end Place;

    procedure Remove (i : piecetype; j : position);

	var     k       :       position;

	begin
	for k := 0 to piecemax[i] do
	    if p[i,k] then puzzl[j+k] := false; end; end;
	piececount[class[i]] := piececount[class[i]] + 1;
	end Remove;

    procedure Trial (j : position) : boolean;


	var     i       :       piecetype;
	    k       :       position;

	begin
	kount := kount + 1;
	for i := 0 to typemax do
	    if piececount[class[i]] <> 0 then
		if Fit (i, j) then 
		    k := Place (i, j);
		    if Trial(k) or (k = 0) then 
			return (true);
		    else Remove (i, j); end;
		     end; end; end;
	return (false);
	end Trial;

    begin
    for m := 0 to size do puzzl[m] := true; end;
    for i := 1 to 5 do for j := 1 to 5 do for k := 1 to 5 do
	puzzl[i+d*(j+d*k)] := false; end; end; end;
    for i := 0 to typemax do for m := 0 to size do p[i, m] := false end  end;
    for i := 0 to 3 do for j := 0 to 1 do for k := 0 to 0 do
	p[0,i+d*(j+d*k)] := true; end; end; end;
    class[0] := 0;
    piecemax[0] := 3+d*1+d*d*0;
    for i := 0 to 1 do for j := 0 to 0 do for k := 0 to 3 do
	p[1,i+d*(j+d*k)] := true; end; end; end;
    class[1] := 0;
    piecemax[1] := 1+d*0+d*d*3;
    for i := 0 to 0 do for j := 0 to 3 do for k := 0 to 1 do
	p[2,i+d*(j+d*k)] := true; end; end; end;
    class[2] := 0;
    piecemax[2] := 0+d*3+d*d*1;
    for i := 0 to 1 do for j := 0 to 3 do for k := 0 to 0 do
	p[3,i+d*(j+d*k)] := true; end; end; end;
    class[3] := 0;
    piecemax[3] := 1+d*3+d*d*0;
    for i := 0 to 3 do for j := 0 to 0 do for k := 0 to 1 do
	p[4,i+d*(j+d*k)] := true; end; end; end;
    class[4] := 0;
    piecemax[4] := 3+d*0+d*d*1;
    for i := 0 to 0 do for j := 0 to 1 do for k := 0 to 3 do
	p[5,i+d*(j+d*k)] := true; end; end; end;
    class[5] := 0;
    piecemax[5] := 0+d*1+d*d*3;
    for i := 0 to 2 do for j := 0 to 0 do for k := 0 to 0 do
	p[6,i+d*(j+d*k)] := true; end; end; end;
    class[6] := 1;
    piecemax[6] := 2+d*0+d*d*0;
    for i := 0 to 0 do for j := 0 to 2 do for k := 0 to 0 do
	p[7,i+d*(j+d*k)] := true; end; end; end;
    class[7] := 1;
    piecemax[7] := 0+d*2+d*d*0;
    for i := 0 to 0 do for j := 0 to 0 do for k := 0 to 2 do
	p[8,i+d*(j+d*k)] := true; end; end; end;
    class[8] := 1;
    piecemax[8] := 0+d*0+d*d*2;
    for i := 0 to 1 do for j := 0 to 1 do for k := 0 to 0 do
	p[9,i+d*(j+d*k)] := true; end; end; end;
    class[9] := 2;
    piecemax[9] := 1+d*1+d*d*0;
    for i := 0 to 1 do for j := 0 to 0 do for k := 0 to 1 do
	p[10,i+d*(j+d*k)] := true; end; end; end;
    class[10] := 2;
    piecemax[10] := 1+d*0+d*d*1;
    for i := 0 to 0 do for j := 0 to 1 do for k := 0 to 1 do
	p[11,i+d*(j+d*k)] := true; end; end; end;
    class[11] := 2;
    piecemax[11] := 0+d*1+d*d*1;
    for i := 0 to 1 do for j := 0 to 1 do for k := 0 to 1 do
	p[12,i+d*(j+d*k)] := true; end; end; end;
    class[12] := 3;
    piecemax[12] := 1+d*1+d*d*1;
    piececount[0] := 13;
    piececount[1] := 3;
    piececount[2] := 1;
    piececount[3] := 1;
    m := 1+d*(1+d*1);
    kount := 0;
    if Fit(0, m) then n := Place(0, m);
    else writef(output,'Error1 in Puzzle\n'); end;
    if not Trial(n) then writef (output,'Error2 in Puzzle.\n'); 
    else if kount <> 2005 then writef (output,'Error3 in Puzzle.\n'); end  end;
    end Puzzle;



procedure Quick;

    (* Sorts an array using quicksort *)

    procedure Initarr;
	var i:integer;
	begin
	Initrand;
	biggest := 0; littlest := 0;
	for i := 1 to sortelements do
	    
	    sortlist[i] := Rand() mod 100000 - 50000;
	    if sortlist[i] > biggest then biggest := sortlist[i];
	    else if sortlist[i] < littlest then littlest := sortlist[i]; end; end;
	     end;
	end Initarr;

    procedure Quicksort(var a: sortarray; l,r: listsize);
	(* quicksort the array A from start to finish *)
	var i,j: listsize;
	    x,w: integer;
	begin
	i:=l; j:=r;
	x:=a[(l+r) DIV 2];
	repeat
	    while a[i]<x do i := i+1; end;
	    while x<a[j] do j := j-1; end;
	    if i<=j then 
		w := a[i];
		a[i] := a[j];
		a[j] := w;
		i := i+1;    j:= j-1;
		end;
	 until i>j;
	if l <j then Quicksort(a,l,j); end;
	if i<r then Quicksort(a,i,r); end;
	end Quicksort;


    begin
    Initarr;
    Quicksort(sortlist,1,sortelements);
    if (sortlist[1] <> littlest) or (sortlist[sortelements] <> biggest) then
	writef (output,' Error in Quick.\n'); end;
    end Quick;


procedure Trees;

    (* Sorts an array using treesort *)
    
    var i : integer;

    procedure Initarr;
	var i:listsize;
	begin
	Initrand;
	biggest := 0; littlest := 0;
	for i := 1 to sortelements do
	    
	    sortlist[i] := Rand() mod 100000 - 50000;
	    if sortlist[i] > biggest then biggest := sortlist[i];
	    else if sortlist[i] < littlest then littlest := sortlist[i]; end; end;
	     end;
	end Initarr;

    procedure Insert(n: integer;  t: nodeptr);
	(* insert n into tree *)
	procedure CreateNode(var t: nodeptr;n: integer);
	begin
		new(t); 
		t^.left := nil; t^.right := nil;
		t^.val := n;
	end CreateNode;
    begin
	WITH t^ do 
	   if n>val then 
		if left=nil then CreateNode(left,n);
		else Insert(n,left); end 
    	   else if n<val then
		if right=nil then CreateNode(right,n);
		else Insert(n,right); end 
	 end; end; end;
    end Insert;

    procedure Checktree(p: nodeptr): boolean;
    (* check by inorder traversal *)
    var result: boolean;
    begin
        result := true;
	WITH p^ do 
		if left<>nil then 
		   if left^.val <= val then result :=false;
		   else result := Checktree(left) and result; end; end;
		if right<>nil then
		   if right^.val >= val then result := false;
		   else result := Checktree(right) and result; end; end;
	 end;
    	 return (result);
    end Checktree;  (* checktree *)

    begin
    Initarr;
    new(tree); 
    tree^.left := nil; tree^.right:=nil; tree^.val:=sortlist[1];
    for i := 2 to sortelements do Insert(sortlist[i],tree) end;
    if not Checktree(tree) then writef (output, ' Error in Tree.\n'); end;
    end Trees;


procedure Bubble;

    (* Sorts an array using bubblesort *)
    
    var i : integer;
	j : integer;

    procedure Initarr;
	var i:listsize;
	begin
	Initrand;
	biggest := 0; littlest := 0;
	for i := 1 to srtelements do
	    
	    sortlist[i] := Rand() mod 100000 - 50000;
	    if sortlist[i] > biggest then biggest := sortlist[i];
	    else if sortlist[i] < littlest then littlest := sortlist[i]; end; end;
	     end;
	end Initarr;

    begin
    Initarr;
    top:=srtelements;

    while top>1 do 

	i:=1;
	while i<integer(top) do 

	    if sortlist[i] > sortlist[i+1] then 
		j := sortlist[i];
		sortlist[i] := sortlist[i+1];
		sortlist[i+1] := j;
		 end;
	    i:=i+1
	     end;

	top:=top-1;
	 end;
    if (sortlist[1] <> littlest) or (sortlist[srtelements] <> biggest) then
	writef ( output,'Error3 in Bubble.\n'); end;
    end Bubble;

procedure Oscar;

var i: integer;

procedure Cos (x: real): real;
(* computes cos of x (x in radians) by an expansion *)
var i: [2..10];
    result,power: real;
    factor: integer;
begin
   result := 1.0; factor := 1;  power := x;
   for i := 2 to 10 do 
      factor := factor * integer(i);  power := power*x;
      if (i mod 2) = 0 then  
        if (i mod 4) = 0 then result := result + power/float(factor);
	else result := result - power/float(factor); end;
       end;
    end;
   return (result);
end Cos;

procedure Min0( arg1, arg2 : integer) : integer;
    begin
    if arg1 < arg2 then
	return (arg1);
    else
	return (arg2); end;
    end Min0;

procedure Printcomplex(  arg1, arg2 : integer;
			var zarray : carray;
			start, finish, increment : integer);

    var
	i: integer;

    begin
    writef(output,'\n');

    i := start;
    repeat
	writef(output,'  %15.0f %15.0f\n',zarray[i].rp,zarray[i].ip);
	i := i + increment;
	writef(output,'  %15.0f %15.0f\n\n',zarray[i].rp,zarray[i].ip);
	i := i + increment;
    until i > finish;

    end Printcomplex; 

procedure Uniform11( var iy: integer;
		      var yfl: real);
    begin
    iy := (4855*iy + 1731) mod 8192;
    yfl := float(iy)/8192.0;
    end Uniform11; (* uniform *) 

procedure Exptab(n: integer;
		 var e: c2array);

    var
	h: array [1..25] of real;
	i, j, k, l, m : integer;
	theta, divisor : real;

    begin (* exptab *)
    theta := 3.1415926536;
    divisor := 4.0;
    for i:=1 to 25 do
	
	h[i] := 1.0/(2.0*Cos (theta/divisor ));
	divisor := divisor + divisor;
	 end;

    m := n DIV 2;
    l := m DIV 2;
    j := 1;
    e[1].rp := 1.0;
    e[1].ip := 0.0;
    e[l+1].rp := 0.0;
    e[l+1].ip := 1.0;
    e[m+1].rp := -1.0;
    e[m+1].ip := 0.0;

    repeat
	i := l DIV 2;
	k := i;

	repeat
	    e[k+1].rp := h[j]*(e[k+i+1].rp+e[k-i+1].rp);
	    e[k+1].ip := h[j]*(e[k+i+1].ip+e[k-i+1].ip);
	    k := k+l;
	until k > m;

	j := Min0 (j+1, 25);
	l := i;
    until l <= 1;

    end Exptab; (* exptab *) 

procedure Fft( n: integer;
	      var z, w: carray;
	      var e: c2array;
	      sqrinv: real);

    var
	i, j, k, l, m, index: integer;

    begin
    m := n DIV 2;
    l := 1;

    repeat
	k := 0;
	j := l;
	i := 1;

	repeat

	    repeat
		w[i+k].rp := z[i].rp+z[m+i].rp;
		w[i+k].ip := z[i].ip+z[m+i].ip;
		w[i+j].rp := e[k+1].rp*(z[i].rp-z[i+m].rp)
		-e[k+1].ip*(z[i].ip-z[i+m].ip);
		w[i+j].ip := e[k+1].rp*(z[i].ip-z[i+m].ip)
		+e[k+1].ip*(z[i].rp-z[i+m].rp);
		i := i+1;
	    until i > j;

	    k := j;
	    j := k+l;
	until j > m;

	(*z := w;*) index := 1;
	repeat
	    z[index] := w[index];
	    index := index+1;
	until index > n;
	l := l+l;
    until l > m;

    for i := 1 to n do
	
	z[i].rp := sqrinv*z[i].rp;
	z[i].ip := -sqrinv*z[i].ip;
	 end ;

    end Fft; 

begin (* oscar *)
Exptab(fftsize,e);
seed := 5767;
for i := 1 to fftsize do
    
    Uniform11 (seed, zr );
    Uniform11 (seed, zi );
    z[i].rp := 20.0*zr - 10.0;
    z[i].ip := 20.0*zi - 10.0;
     end ;


for i := 1 to 20 do 
   Fft(fftsize,z,w,e,0.0625);
   (* Printcomplex( 6, 99, z, 1, 256, 17 ); *)
 end;
end Oscar; (* oscar *) 


begin
fixed := 0.0;	floating := 0.0;

writef(output,'    Perm'); timer := Getclock(); Perm; xtimes[1] := Getclock()-timer;
fixed := fixed + permbase*xtimes[1];
floating := floating + permbase*xtimes[1];
writef(output,'  Towers'); timer := Getclock(); Towers; xtimes[2] := Getclock()-timer;
fixed := fixed + towersbase*xtimes[2];
floating := floating + towersbase*xtimes[2];
writef(output,'  Queens'); timer := Getclock(); Queens; xtimes[3] := Getclock()-timer;
fixed := fixed + queensbase*xtimes[3];
floating := floating + queensbase*xtimes[3];
writef(output,'   Intmm'); timer := Getclock(); Intmm; xtimes[4] := Getclock()-timer;
fixed := fixed + intmmbase*xtimes[4];
floating := floating + intmmbase*xtimes[4];
writef(output,'      Mm'); timer := Getclock(); Mm; xtimes[5] := Getclock()-timer;
fixed := fixed + mmbase*xtimes[5];
floating := floating + fpmmbase*xtimes[5];
writef(output,'  Puzzle'); timer := Getclock(); Puzzle; xtimes[6] := Getclock()-timer;
fixed := fixed + puzzlebase*xtimes[6];
floating := floating + puzzlebase*xtimes[6];
writef(output,'   Quick'); timer := Getclock(); Quick; xtimes[7] := Getclock()-timer;
fixed := fixed + quickbase*xtimes[7];
floating := floating + quickbase*xtimes[7];
writef(output,'  Bubble'); timer := Getclock(); Bubble; xtimes[8] := Getclock()-timer;
fixed := fixed + bubblebase*xtimes[8];
floating := floating + bubblebase*xtimes[8];
writef(output,'    Tree'); timer := Getclock(); Trees; xtimes[9] := Getclock()-timer;
fixed := fixed + treebase*xtimes[9];
floating := floating + treebase*xtimes[9];
writef(output,'     FFT'); timer := Getclock(); Oscar; xtimes[10] := Getclock()-timer;
fixed := fixed + fftbase*xtimes[10];
floating := floating + fpfftbase*xtimes[10];
writef(output,'\n');
for i := 1 to 10 do writef(output,'%8.0f',xtimes[i]); end;
writef(output,'\n\n');
    (* compute composites *)
    writef(output,'Nonfloating point composite is %10.0f\n\n',fixed/10.0);
    writef(output,'Floating point composite is %10.0f\n',floating/10.0);


end bench.

