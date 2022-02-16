.po 1i
.nr PO 1i
.TL
DYNAMIC ARRAYS AND SUBARRAY PARAMETERS IN MODULA-2
.AU
Michael L. Powell
.AI
Digital Equipment Corporation
Western Research Laboratory
4410 El Camino Real
Los Altos, California  94022
.AB
.PP
Modula-2 has been extended to support more flexible operations on arrays.
A new data type, called a
.I "dynamic array,"
defines an array to be allocated at run-time.
The "open array" parameter passing mechanism is extended to allow multiple
dimensions to be open, and to allow subarrays and cross-sections of arrays
to be passed.
.PP
By specifying attributes on dynamic arrays, it is possible to interact with
C programs in a convenient way.
.AE
.SH
Dynamic arrays
.LP
Conceptually, a dynamic array is a pointer to an array whose size is determined
at run time.
A dynamic array is stored as a descriptor,
a pointer followed by a count for each
dimension indicating how many elements there are in that dimension.
.LP
Dynamic arrays are declared as follows:
.DS L
type
    OneD = dynarray of integer;
    TwoD = dynarray of array of integer;
    ThreeD = dynarray of array of array of integer;
var
    oneD : OneD;
    twoD : TwoD;
    threeD : ThreeD;
.DE
For multi-dimensional dynamic arrays,
only the first dimension is "dynarray of";
subsequent dimensions are specified simply as "array of".
It is possible to specify "dynarray of" more than once when a dynamic array of
dynamic arrays (i.e., a descriptor for an array of descriptors) is desired.
E.g.,
.DS L
    almostArgv : dynarray of dynarray of char;
.DE
.LP
Dynamic arrays are manipulated like pointers.
Assigning one dynamic array to a second causes both to point to the same
array.
A comparison for equality of one dynamic array to another will be true if
both point to the same address and have the same size(s).
Because dynamic arrays are like pointers, they may be assigned and compared
to nil.
Dynamic arrays are allocated using
.I new
by specifying the number of elements in each dimension.
Dynamic arrays are deallocated using
.I dispose.
E.g.,
.DS L
    oneD := nil;
    if oneD = nil then
	new(oneD,100);         (* create array of 100 integers *)
    end;
    dispose(oneD);             (* deallocate the array *)

    readf(input,"%d %d",m,n);
    new(twoD,m,n);             (* create 2 dimensional array *)

    dispose(twoD);
.DE
Elements of dynamic arrays are accessed by dereferencing and subscripting.
A good analogy is the way pointers to records work.
With a pointer to a record, you first dereference the pointer, then
dot it with the field name;
with dynamic arrays, you first dereference the pointer, then
subscript it with the element number.
Dynamic arrays are always subscripted by cardinals or integers with the
first element 0.
E.g.,
.DS L
    if almostArgv^[0]^[1] = 'x' then
	oneD^[i] := twoD^[i,j];
	oneD^[i] := twoD^[i][j];  (* same as previous line *)
    end;
.DE
Dereferencing a dynamic array without subscripting produces a value that can
be passed as an open array parameter.
E.g., if P is declared as
.DS L
    procedure P(a : array of integer);
.DE
the following are valid:
.DS L
    P(oneD^);           (* pass dynamic array as open array *)
    P(twoD^[i]);        (* pass row of dynamic array as an open array *)
.DE
.LP
In addition to their use with parameters (described below), the
.I number
and
.I high
take a dynamic array as a parameter and return the number of elements and the
index of the last element, respectively.
A constant second parameter may be specified to indicate the
dimension (starting at 1 for the leftmost subscript) desired.
.SH
Multi-dimensional open arrays
.LP
The standard Modula-2 open array parameters have been extended to be
multi-dimensional.
The actual parameter may be a normal array, a dereferenced dynamic array,
or an open array parameter.
The
.I number
and
.I high
functions accept a constant second parameter that specifies the
dimension (starting at 1 for the leftmost subscript) desired.
The last k >= 1 dimension(s) of a formal open array parameter
may be used as a k-dimensional actual open array parameter.
E.g.,
.DS L
procedure P1(var a : array of integer);
begin
    i := number(a);     (* standard open array *)
    a[10] := i;
end P1;

procedure P2(var a : array of array of integer);
begin
    i := number(a,1);   (* number of elements in first dimension *)
    j := number(a,2);   (* number of elements in second dimension *)
    P1(a[i-1]);         (* pass last row as an open array *)
end P2;

var
    a1 : array [1..100] of integer;
    a2 : array [1..10],[20..30] of integer;
begin
    P1(a1);             (* normal array passed as open array *)
    P1(a2[i]);          (* row of array passed as open array *)
    P2(a2);             (* 2-d array passed as open array *)
.DE
.LP
It is sometimes desirable to pass as an array a subrange of the values in
the array, e.g., the last 20 characters in a string.
(In Ada, this is called a
.I slice.)
A subrange of the last dimension of a normal array, dereferenced dynamic array
or open array parameter may be used as an open array parameter.
A specification of a[i:n] means n elements starting at element i of
array a (i.e., elements i..i+n-1).
E.g.,
.DS L
    P1(a1[i:n]);      (* elements a1[i],a1[i+2],...,a1[i+n-1] *)
    P2(a2[i:n]);      (* rows a2[i],a2[i+2],...,a2[i+n-1] *)
    P1(a2[i][j:m]);   (* elements a2[i][j],a2[i][j+1],...,a2[i][j+m-1] *)
.DE
.SH
Subarrays
.LP
Dynamic arrays and open arrays have elements that are allocated to consecutive
storage locations.
However, some algorithms require processing sets of values that are not
stored contiguously, e.g., a column of a two-dimensional array.
A
.I subarray
formal parameter may accept as an actual parameter a set of values that are
equally spaced in memory.
Subarray parameters may also be subranges of elements.
Because the elements need not be contiguous, the subrange may be specified
in any dimension of the actual parameter.
.LP
It is possible to have an open array or subarray whose elements are fixed size
arrays.
In this case, subscript lists and subrange specifications may include both the 
open and fixed dimensions.
However,
it is not possible for a subscript list or subrange specification to span
a dynamic array dimension.
E.g., given the above definition, the following is illegal:
.DS L
    almostArgv^[i:n]^[j:m]
.DE
.LP
The actual parameter for a subarray parameter may be a normal array, a
dereferenced dynamic array, an open array parameter, or a subarray parameter.
To the subroutine, a subarray parameter looks just like an open array parameter.
E.g.,
.DS L
procedure PS1(var a : subarray of integer);
begin
    i := number(a);
    a[10] := i;
end PS1;
.DE
.DS L
procedure PS2(var a : subarray of array of integer);
begin
    i := number(a,1);
    j := number(a,2);
    PS1(a[i-1]);
end PS2;
.DE
.DS L
var
    a1 : array [1..100] of integer;
    a2 : array [1..10],[20..30] of integer;
    a3 : array [1..10],[20..30],[40..50] of integer;
    d1 : dynarray of integer;
    d2 : dynarray of array of integer;
    d3 : dynarray of array of array of integer;
begin
    (* a 1-d subarray may be *)
    PS1(a1);             (* 1-d normal array *)
    PS1(d1^);            (* 1-d dynamic array *)
    PS1(a1[i:n]);        (* subrange of a 1-d normal array *)
    PS1(a2[i]);          (* row of a 2-d normal array *)
    PS1(a2[i][j:n]);     (* subrange of a row of a 2-d array *)
    PS1(a2[i:n][j]);     (* subrange of a column of a 2-d array *)
    (* a 2-d subarray may be *)
    PS2(a2);             (* 2-d normal array *)
    PS2(a2[i:n][j:m]);   (* subarray of 2-d normal array *)
    PS2(a3[i][j:n,k:m]); (* subarray of 3-d array *)
    PS2(a3[i:n,j:m,k]);  (* cross section of 3-d normal array *)
    PS2(d3^[i:n,j:m,k]); (* cross section of a 3-d dynamic array *)
.DE
.LP
Note:
A subarray parameter must be either a const or a var parameter
(i.e., it may not be a value parameter).
.LP
Normal arrays, dereferenced dynamic arrays, open array parameters and subarray
parameters form a hierarchy of increasing generality with respect to parameter
passing.
The following are valid actual parameters for a formal open array parameter.
.DS
Normal array
Dereferenced dynamic array
Open array parameter
Subrange of the last specified dimension of one of the above
.DE
In addition to the above,
the following are valid actual parameters for a formal subarray parameter.
.DS
Subarray parameter
Subrange of one or more dimensions of any of the above
.DE
.SH
System programming features
.LP
The extended array facilities interact with the system programming features
of Modula-2 in the following ways:
.LP
A dereferenced dynamic array or an open array parameter of any number of
dimensions may be passed as a one-dimensional open array of word (or byte).
The number of words (or bytes) apparent in the subroutine
reflects the size of the storage occupied by all dimensions of the array.
Thus a 10 by 10 array of integers would appear as an array of 100 words.
.LP
@nocount may be specified for dynamic arrays as well as for open arrays,
but is restricted to one-dimensional arrays in both cases.
A @nocount array may be passed as a counted open array by using the subrange
notation, e.g.,
.DS L
type
    CStringPointer = dynarray @nocount of char;
var
    str : CStringPointer;

procedure P(a : array of char);
end P;

begin
    P(str^[0:10]);   
.DE
.LP
For dynamic arrays, the same attributes as for pointers may be specified to
cause the array to be allocated in different heaps and to be validated
appropriately on dereference.
E.g.,
.DS L
type
    PascalArray = dynarray @pascal of integer;
    CArray = dynarray @c of integer;
    UncheckedModulaArray = dynarray @nocheck of integer;
    LocalArray = dynarray @local of integer;
.DE
.LP
A dynamic array with attribute @local is allocated using the
.I local
procedure.
Storage for the array is allocated in the activation record that is
active at the point of the call to local and is released when that
procedure returns.
This facility is intended for allocation of temporary arrays needed for the
lifetime of a procedure.
