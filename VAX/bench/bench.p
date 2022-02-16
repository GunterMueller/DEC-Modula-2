program bench(output);
 
(*  This is a suite of benchmarks that are relatively short, both in program
    size and execution time.  It requires no input, and prints the execution
    time for each program, using the system- dependent routine Getclock,
    below, to find out the current CPU time.  It does a rudimentary check to
    make sure each program gets the right output.  These programs were
    gathered by John Hennessy and modified by Peter Nye. *)
 
 
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
    fftsize = 256 ;
    fftsize2 = 129  ;
 
type
    (* Perm *)
    permrange = 0 .. 10;
 
   (* tree *)
    nodeptr = ^node;
    node = record
	left,right: nodeptr;
	val:integer;
    end;
 
    (* Towers *)
    discsizrange = 1..maxcells;
    stackrange = 1..3;
    cellcursor = 0..maxcells;
    element = 
        record
	    discsize:discsizrange;
	    next:cellcursor
	end;
    emsgtype = packed array[1..15] of char;
 
    (* Intmm, Mm *)
    index = 1 .. rowsize;
    intmatrix = array [index,index] of integer;
    realmatrix = array [index,index] of real;
 
    (* Puzzle *)
    piececlass = 0..classmax;
    piecetype = 0..typemax;
    position = 0..size;
 
    (* Bubble, Quick *)
    listsize = 0..sortelements;
    sortarray = array [listsize] of integer;
 
    (* FFT *)
    complex = record  rp: real ;
	      ip: real  end ;
    carray = array [1..fftsize] of complex ;
    c2array = array [1..fftsize2] of complex ;
 
var
 value: real;
    fixed,float: real;
 
    (* global *)
    timer: integer;
    xtimes: array[1..10] of integer;
    seed: integer;
 
    (* Perm *)
    permarray : array [permrange] of permrange;
    pctr: integer;
 
    (* tree *)
    tree: nodeptr;
 
    (* Towers *)
    stack: array[stackrange] of cellcursor;
    cellspace: array[1..maxcells] of element;
    freelist : cellcursor;
    movesdone: integer;
 
    (* Intmm, Mm *)
    ima, imb, imr: intmatrix;
    rma, rmb, rmr: realmatrix;
 
    (* Puzzle *)
    piececount: array [piececlass] of 0..13;
    class: array [piecetype] of piececlass;
    piecemax: array [piecetype] of position;
    puzzl: array [position] of boolean;
    p: array [piecetype, position] of boolean;
    m,n: position;
    kount: integer;
 
    (* Bubble, Quick *)
    sortlist: sortarray;
    biggest, littlest: integer;
    top: listsize;
 
    (* FFT *)
    z, w : carray ;
    e    : c2array ;
    zr, zi : real;
    i : integer;
 
(* global procedures *)
 
function Getclock: integer;
    begin
    Getclock:= clock;
    end;
 
procedure Initrand;
    begin
    seed := {7774755} 74755;
    end;
 
function Rand: integer;
    begin
    seed := (seed * 1309 + 13849) mod 65536;
    Rand := seed;
    end;
 
 
procedure Perm;
 
    (* Permutation program, heavily recursive, written by Denny Brown. *)
 
    procedure Swap(var a,b : permrange);
	var t : permrange;
	begin
	t := a;  a := b;  b := t;
	end;
 
    procedure Initialize;
	var i : permrange;
	begin
	for i := 1 to 7 do begin
	    permarray[i]:=i-1;
	    end;
	end;
 
    procedure Permute(n : permrange);
	var k : permrange;
	begin   (* permute *)
	pctr := pctr + 1;
	if n<>1 then  begin
	    Permute(n-1);
	    for k := n-1 downto 1 do begin
		Swap(permarray[n],permarray[k]);
		Permute(n-1);
		Swap(permarray[n],permarray[k]);
		end;
	    end;
	end     (* permute *);
 
    var i : integer;
    begin   (* Perm *)
    pctr := 0;
    for i := 1 to 5 do begin
	Initialize;
	Permute(7);
	end;
    if pctr <> 43300 then
	writeln (output,' Error in Perm.');
    end     (* Perm *);
 
 
 
procedure Towers;
 
    (*  Program to Solve the Towers of Hanoi *)
 
    procedure Error (emsg:emsgtype);
	begin
	writeln(output,' Error in Towers: ',emsg);
	end;
 
    procedure Makenull (s:stackrange);
	begin
	stack[s]:=0;
	end;
 
    function Getelement:cellcursor;
	begin
	if freelist>0 then
	    begin
	    Getelement := freelist;
	    freelist:=cellspace[freelist].next
	    end
	else
	    Error('out of space   ')
	end;
 
    procedure Push(i:discsizrange;s:stackrange);
 
	var
	    errorfound:boolean;
	    localel:cellcursor;
	begin
 
	errorfound:=false;
	if stack[s] > 0 then
	    if cellspace[stack[s]].discsize<=i then
		begin
		errorfound:=true;
		Error('disc size error')
		end;
	if not errorfound then
	    begin
	    localel:=Getelement;
	    cellspace[localel].next:=stack[s];
	    stack[s]:=localel;
	    cellspace[localel].discsize:=i
	    end
	end;
 
    procedure Init (s:stackrange;n:discsizrange);
 
	var
	    discctr:discsizrange;
	begin
	Makenull(s);
	for discctr:=n downto 1 do
	    Push(discctr,s)
	end;
 
    function Pop (s:stackrange):discsizrange;
 
	var
	    temp:cellcursor;
	begin
	if stack[s] > 0 then
	    begin
	    Pop:=cellspace[stack[s]].discsize;
	    temp:=cellspace[stack[s]].next;
	    cellspace[stack[s]].next:=freelist;
	    freelist:=stack[s];
	    stack[s]:=temp
	    end
	else
	    Error('nothing to pop ');
	end;
 
    procedure Move (s1,s2:stackrange);
	begin
 
	Push(Pop(s1),s2);
	movesdone:=movesdone+1
	end;
 
    procedure Towers(i,j,k:integer);
	var other:integer;
	begin
 
	if k=1 then
	    Move(i,j)
	else
	    begin
	    other:=6-i-j;
	    Towers(i,other,k-1);
	    Move(i,j);
	    Towers(other,j,k-1)
	    end
	end;
 
 
    var i : integer;
    begin (* Towers *)
    for i:=1 to maxcells do
	cellspace[i].next:=i-1;
    freelist:=maxcells;
    Init(1,14);
    Makenull(2);
    Makenull(3);
    movesdone:=0;
    Towers(1,2,14);
    if movesdone <> 16383 then
	writeln (output,' Error in Towers.');
    end; (* Towers *)
 
 
 
procedure Queens;
    (* The eight queens problem, solved 50 times. *)
 
    var  i:integer;
 
    procedure Doit;
 
	type    
	    doubleboard =   2..16;
	    doublenorm  =   -7..7;
	    boardrange  =   1..8;
	    aarray      =   array [boardrange] of boolean;
	    barray      =   array [doubleboard] of boolean;
	    carray      =   array [doublenorm] of boolean;
	    xarray      =   array [boardrange] of boardrange;
 
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
	    while (not q) and (j <> 8) do
		begin j := j + 1;
		q := false;
		if b[j] and a[i+j] and c[i-j] then
		    begin x[i] := j;
		    b[j] := false;
		    a[i+j] := false;
		    c[i-j] := false;
		    if i < 8 then
			begin Try(i+1,q,a,b);
			if not q then
			    begin b[j] := true;
			    a[i+j] := true;
			    c[i-j] := true
			    end
			end
		    else q := true
		    end
		end
	    end;
 
	begin
	i := 0 - 7;
	while i <= 16 do
	    begin if (i >= 1) and (i <= 8) then a[i] := true;
	    if i >= 2 then b[i] := true;
	    if i <= 7 then c[i] := true;
	    i := i + 1
	    end;
 
	Try(1, q, b, a);
	if not q then
	    writeln (output,' Error in Queens.');
	end;
    begin
    for i := 1 to 50 do Doit;
    end;
 
 
 
procedure Intmm;
 
    (* Multiplies two integer matrices. *)
 
    procedure Initmatrix (var m: intmatrix);
	var i,j : integer;
	begin
	for i := 1 to rowsize do
	    for j := 1 to rowsize do
		m[i,j] := (Rand mod 120 - 60);
	end;
 
    procedure Innerproduct(var result: integer; var a,b: intmatrix;
			   row,column: index);
	(* computes the inner product of A[row,*] and B[*,column] *)
	var i: index;
	begin
	result := 0;
	for i := 1 to rowsize do  result := result + a[row,i]*b[i,column];
	end;
 
    var i,j : integer;
    begin
    Initrand;
    Initmatrix (ima);
    Initmatrix (imb);
    for i := 1 to rowsize do
	for j := 1 to rowsize do Innerproduct(imr[i,j],ima,imb,i,j);
    end;
 
 
procedure Mm;
 
    (* Multiplies two real matrices. *)
 
    procedure Initmatrix (var m: realmatrix);
	var i,j : integer;
	begin
	for i := 1 to rowsize do
	    for j := 1 to rowsize do
		m[i,j] := (Rand mod 120 - 60)/3;
	end;
 
    procedure Innerproduct(var result: real; var a,b: realmatrix;
			   row,column: index);
	(* computes the inner product of A[row,*] and B[*,column] *)
	var i: index;
	begin
	result := 0;
	for i := 1 to rowsize do  result := result + a[row,i]*b[i,column];
	end;
 
    var i,j : integer;
    begin
    Initrand;
    Initmatrix (rma);
    Initmatrix (rmb);
    for i := 1 to rowsize do
	for j := 1 to rowsize do Innerproduct(rmr[i,j],rma,rmb,i,j);
    end;
 
 
 
procedure Puzzle;
 
    (* A compute-bound program from Forest Baskett. *)
 
    function Fit (i : piecetype; j : position) : boolean;
 
	label   1;
	var     k       :       position;
 
	begin
	Fit := false;
	for k := 0 to piecemax[i] do
	    if p[i,k] then if puzzl[j+k] then goto 1;
	Fit := true;
	1:
	end;
 
    function Place (i : piecetype; j : position) : position;
 
	label   1;
	var     k       :       position;
 
	begin
	for k := 0 to piecemax[i] do
	    if p[i,k] then puzzl[j+k] := true;
	piececount[class[i]] := piececount[class[i]] - 1;
	for k := j to size do
	    if not puzzl[k] then begin
		Place := k;
		goto 1;
		end;
	Place := 0;
	1:
	end;
 
    procedure Remove (i : piecetype; j : position);
 
	var     k       :       position;
 
	begin
	for k := 0 to piecemax[i] do
	    if p[i,k] then puzzl[j+k] := false;
	piececount[class[i]] := piececount[class[i]] + 1;
	end;
 
    function Trial (j : position) : boolean;
 
	label   1;
 
	var     i       :       piecetype;
	    k       :       position;
 
	begin
	for i := 0 to typemax do
	    if piececount[class[i]] <> 0 then
		if Fit (i, j) then begin
		    k := Place (i, j);
		    if Trial(k) or (k = 0) then begin
			Trial := true;
			goto 1;
			end
		    else Remove (i, j);
		    end;
	Trial := false;
	1:      kount := kount + 1;
	end;
 
    var i,j,k,m : integer;
    begin
    for m := 0 to size do puzzl[m] := true;
    for i := 1 to 5 do for j := 1 to 5 do for k := 1 to 5 do
	puzzl[i+d*(j+d*k)] := false;
    for i := 0 to typemax do for m := 0 to size do p[i, m] := false;
    for i := 0 to 3 do for j := 0 to 1 do for k := 0 to 0 do
	p[0,i+d*(j+d*k)] := true;
    class[0] := 0;
    piecemax[0] := 3+d*1+d*d*0;
    for i := 0 to 1 do for j := 0 to 0 do for k := 0 to 3 do
	p[1,i+d*(j+d*k)] := true;
    class[1] := 0;
    piecemax[1] := 1+d*0+d*d*3;
    for i := 0 to 0 do for j := 0 to 3 do for k := 0 to 1 do
	p[2,i+d*(j+d*k)] := true;
    class[2] := 0;
    piecemax[2] := 0+d*3+d*d*1;
    for i := 0 to 1 do for j := 0 to 3 do for k := 0 to 0 do
	p[3,i+d*(j+d*k)] := true;
    class[3] := 0;
    piecemax[3] := 1+d*3+d*d*0;
    for i := 0 to 3 do for j := 0 to 0 do for k := 0 to 1 do
	p[4,i+d*(j+d*k)] := true;
    class[4] := 0;
    piecemax[4] := 3+d*0+d*d*1;
    for i := 0 to 0 do for j := 0 to 1 do for k := 0 to 3 do
	p[5,i+d*(j+d*k)] := true;
    class[5] := 0;
    piecemax[5] := 0+d*1+d*d*3;
    for i := 0 to 2 do for j := 0 to 0 do for k := 0 to 0 do
	p[6,i+d*(j+d*k)] := true;
    class[6] := 1;
    piecemax[6] := 2+d*0+d*d*0;
    for i := 0 to 0 do for j := 0 to 2 do for k := 0 to 0 do
	p[7,i+d*(j+d*k)] := true;
    class[7] := 1;
    piecemax[7] := 0+d*2+d*d*0;
    for i := 0 to 0 do for j := 0 to 0 do for k := 0 to 2 do
	p[8,i+d*(j+d*k)] := true;
    class[8] := 1;
    piecemax[8] := 0+d*0+d*d*2;
    for i := 0 to 1 do for j := 0 to 1 do for k := 0 to 0 do
	p[9,i+d*(j+d*k)] := true;
    class[9] := 2;
    piecemax[9] := 1+d*1+d*d*0;
    for i := 0 to 1 do for j := 0 to 0 do for k := 0 to 1 do
	p[10,i+d*(j+d*k)] := true;
    class[10] := 2;
    piecemax[10] := 1+d*0+d*d*1;
    for i := 0 to 0 do for j := 0 to 1 do for k := 0 to 1 do
	p[11,i+d*(j+d*k)] := true;
    class[11] := 2;
    piecemax[11] := 0+d*1+d*d*1;
    for i := 0 to 1 do for j := 0 to 1 do for k := 0 to 1 do
	p[12,i+d*(j+d*k)] := true;
    class[12] := 3;
    piecemax[12] := 1+d*1+d*d*1;
    piececount[0] := 13;
    piececount[1] := 3;
    piececount[2] := 1;
    piececount[3] := 1;
    m := 1+d*(1+d*1);
    kount := 0;
    if Fit(0, m) then n := Place(0, m)
    else writeln('Error1 in Puzzle');
    if not Trial(n) then writeln (output,'Error2 in Puzzle.')
    else if kount <> 2005 then writeln (output, 'Error3 in Puzzle.');
    end;
 
 
 
procedure Quick;
 
    (* Sorts an array using quicksort *)
 
    procedure Initarr;
	var i:integer;
	begin
	Initrand;
	biggest := 0; littlest := 0;
	for i := 1 to sortelements do
	    begin
	    sortlist[i] := Rand mod 100000 - 50000;
	    if sortlist[i] > biggest then biggest := sortlist[i]
	    else if sortlist[i] < littlest then littlest := sortlist[i];
	    end;
	end;
 
    procedure Quicksort(var a: sortarray; l,r: listsize);
	(* quicksort the array A from start to finish *)
	var i,j: listsize;
	    x,w: integer;
	begin
	i:=l; j:=r;
	x:=a[(l+r) div 2];
	repeat
	    while a[i]<x do i := i+1;
	    while x<a[j] do j := j-1;
	    if i<=j then begin
		w := a[i];
		a[i] := a[j];
		a[j] := w;
		i := i+1;    j:= j-1;
		end
	until i>j;
	if l <j then Quicksort(a,l,j);
	if i<r then Quicksort(a,i,r);
	end;
 
 
    begin
    Initarr;
    Quicksort(sortlist,1,sortelements);
    if (sortlist[1] <> littlest) or (sortlist[sortelements] <> biggest) then
	writeln (output, ' Error in Quick.');
    end;
 
 
procedure Trees;
 
    (* Sorts an array using treesort *)
 
    procedure Initarr;
	var i:integer;
	begin
	Initrand;
	biggest := 0; littlest := 0;
	for i := 1 to sortelements do
	    begin
	    sortlist[i] := Rand mod 100000 - 50000;
	    if sortlist[i] > biggest then biggest := sortlist[i]
	    else if sortlist[i] < littlest then littlest := sortlist[i];
	    end;
	end;
 
    procedure Insert(n: integer;  t: nodeptr);
	(* insert n into tree *)
	procedure CreateNode(var t: nodeptr;n: integer);
	begin
		new(t); 
		t^.left := nil; t^.right := nil;
		t^.val := n;
	end;
    begin
	with t^ do begin
	   if n>val then 
		if left=nil then CreateNode(left,n)
		else Insert(n,left)
    	   else if n<val then
		if right=nil then CreateNode(right,n)
		else Insert(n,right)
	end;
    end;
 
    function Checktree(p: nodeptr): boolean;
    (* check by inorder traversal *)
    var result: boolean;
    begin
        result := true;
	with p^ do begin
		if left<>nil then 
		   if left^.val <= val then result:=false
		   else result := Checktree(left) and result;
		if right<>nil then
		   if right^.val >= val then result := false
		   else result := Checktree(right) and result;
	end;
    	Checktree := result;
    end; (* checktree *)
    var i : integer;
    begin
    Initarr;
    new(tree); 
    tree^.left := nil; tree^.right:=nil; tree^.val:=sortlist[1];
    for i := 2 to sortelements do Insert(sortlist[i],tree);
    if not Checktree(tree) then writeln (output, ' Error in Tree.');
    end;
 
 
procedure Bubble;
    (* Sorts an array using bubblesort *)
 
    procedure Initarr;
	var i:integer;
	begin
	Initrand;
	biggest := 0; littlest := 0;
	for i := 1 to srtelements do
	    begin
	    sortlist[i] := Rand mod 100000 - 50000;
	    if sortlist[i] > biggest then biggest := sortlist[i]
	    else if sortlist[i] < littlest then littlest := sortlist[i];
	    end;
	end;
 
    var i,j : integer;
    begin
    Initarr;
    top:=srtelements;
 
    while top>1 do begin
 
	i:=1;
	while i<top do begin
 
	    if sortlist[i] > sortlist[i+1] then begin
		j := sortlist[i];
		sortlist[i] := sortlist[i+1];
		sortlist[i+1] := j;
		end;
	    i:=i+1
	    end;
 
	top:=top-1
	end;
    if (sortlist[1] <> littlest) or (sortlist[srtelements] <> biggest) then
	writeln (output, 'Error3 in Bubble.');
    end;
 
procedure Oscar;
 
function Cos (x: real): real;
(* computes cos of x (x in radians) by an expansion *)
var i: 2..10;
    result,power: real;
    factor: integer;
begin
   result := 1.0; factor := 1;  power := x;
   for i := 2 to 10 do begin
      factor := factor * i;  power := power*x;
      if (i mod 2) = 0 then  begin
        if (i mod 4) = 0 then result := result + power/factor
	else result := result - power/factor;
      end;
   end;
   Cos := result;
end;
 
function Min0( arg1, arg2 : integer) : integer;
    begin
    if arg1 < arg2 then
	Min0 := arg1
    else
	Min0 := arg2;
    end;
 
procedure Printcomplex(  arg1, arg2 : integer;
			var zarray : carray;
			start, finish, increment : integer);
 
    var
	i: integer ;
 
    begin
    writeln(output) ;
 
    i := start;
    repeat
	write('  ',zarray[i].rp:15,zarray[i].ip:15) ;
	i := i + increment;
	write('  ',zarray[i].rp:15,zarray[i].ip:15) ;
	writeln(output);
	i := i + increment ;
    until i > finish;
 
    end ;
 
procedure Uniform11( var iy: integer;
		      var yfl: real);
    begin
    iy := (4855*iy + 1731) mod 8192;
    yfl := iy/8192.0;
    end (* uniform *) ;
 
procedure Exptab(n: integer;
		 var e: c2array) ;
 
    var
	h: array [1..25] of real ;
	i, j, k, l, m : integer;
	theta, divisor : real;
 
    begin (* exptab *)
    theta := 3.1415926536;
    divisor := 4.0;
    for i:=1 to 25 do
	begin
	h[i] := 1/(2*Cos( theta/divisor ));
	divisor := divisor + divisor
	end;
 
    m := n div 2 ;
    l := m div 2 ;
    j := 1 ;
    e[1].rp := 1.0 ;
    e[1].ip := 0.0;
    e[l+1].rp := 0.0;
    e[l+1].ip := 1.0 ;
    e[m+1].rp := -1.0 ;
    e[m+1].ip := 0.0 ;
 
    repeat
	i := l div 2 ;
	k := i ;
 
	repeat
	    e[k+1].rp := h[j]*(e[k+i+1].rp+e[k-i+1].rp) ;
	    e[k+1].ip := h[j]*(e[k+i+1].ip+e[k-i+1].ip) ;
	    k := k+l ;
	until k > m ;
 
	j := Min0( j+1, 25);
	l := i ;
    until l <= 1 ;
 
    end (* exptab *) ;
 
procedure Fft( n: integer ;
	      var z, w: carray ;
	      var e: c2array ;
	      sqrinv: real) ;
 
    var
	i, j, k, l, m, index: integer ;
 
    begin
    m := n div 2 ;
    l := 1 ;
 
    repeat
	k := 0 ;
	j := l ;
	i := 1 ;
 
	repeat
 
	    repeat
		w[i+k].rp := z[i].rp+z[m+i].rp ;
		w[i+k].ip := z[i].ip+z[m+i].ip ;
		w[i+j].rp := e[k+1].rp*(z[i].rp-z[i+m].rp)
		-e[k+1].ip*(z[i].ip-z[i+m].ip) ;
		w[i+j].ip := e[k+1].rp*(z[i].ip-z[i+m].ip)
		+e[k+1].ip*(z[i].rp-z[i+m].rp) ;
		i := i+1 ;
	    until i > j ;
 
	    k := j ;
	    j := k+l ;
	until j > m ;
 
	(*z := w ;*) index := 1;
	repeat
	    z[index] := w[index];
	    index := index+1;
	until index > n;
	l := l+l ;
    until l > m ;
 
    for i := 1 to n do
	begin
	z[i].rp := sqrinv*z[i].rp ;
	z[i].ip := -sqrinv*z[i].ip
	end ;
 
    end ;
 
var i : integer;
begin (* oscar *)
Exptab(fftsize,e) ;
seed := 5767 ;
for i := 1 to fftsize do
    begin
    Uniform11( seed, zr );
    Uniform11( seed, zi );
    z[i].rp := 20.0*zr - 10.0;
    z[i].ip := 20.0*zi - 10.0;
    end ;
 
 
for i := 1 to 20 do begin
   Fft(fftsize,z,w,e,0.0625) ;
   (* Printcomplex( 6, 99, z, 1, 256, 17 ); *)
end;
end (* oscar *) ;
 
 
begin
fixed := 0.0;	float := 0.0;
(* rewrite (output); *)
write ('Perm'  :8);timer := Getclock; Perm;   xtimes[1] := Getclock-timer;
fixed := fixed + permbase*xtimes[1];
float := float + permbase*xtimes[1];
write ('Towers':8);timer := Getclock; Towers; xtimes[2] := Getclock-timer;
fixed := fixed + towersbase*xtimes[2];
float := float + towersbase*xtimes[2];
write ('Queens':8);timer := Getclock; Queens; xtimes[3] := Getclock-timer;
fixed := fixed + queensbase*xtimes[3];
float := float + queensbase*xtimes[3];
write ('Intmm' :8);timer := Getclock; Intmm;  xtimes[4] := Getclock-timer;
fixed := fixed + intmmbase*xtimes[4];
float := float + intmmbase*xtimes[4];
write ('Mm'    :8);timer := Getclock; Mm;     xtimes[5] := Getclock-timer;
fixed := fixed + mmbase*xtimes[5];
float := float + fpmmbase*xtimes[5];
write ('Puzzle':8);timer := Getclock; Puzzle; xtimes[6] := Getclock-timer;
fixed := fixed + puzzlebase*xtimes[6];
float := float + puzzlebase*xtimes[6];
write ('Quick' :8);timer := Getclock; Quick;  xtimes[7] := Getclock-timer;
fixed := fixed + quickbase*xtimes[7];
float := float + quickbase*xtimes[7];
write ('Bubble':8);timer := Getclock; Bubble; xtimes[8] := Getclock-timer;
fixed := fixed + bubblebase*xtimes[8];
float := float + bubblebase*xtimes[8];
write ('Tree':8);timer := Getclock; Trees; xtimes[9] := Getclock-timer;
fixed := fixed + treebase*xtimes[9];
float := float + treebase*xtimes[9];
write ('FFT':8);timer := Getclock; Oscar; xtimes[10] := Getclock-timer;
fixed := fixed + fftbase*xtimes[10];
float := float + fpfftbase*xtimes[10];
writeln;
for i := 1 to 10 do write (xtimes[i]:8);
writeln;
    (* compute composites *)
    writeln;
    writeln('Nonfloating point composite is ',fixed/10.0:10);
    writeln;
    writeln('Floating point composite is ',float/10.0:10);
end.
