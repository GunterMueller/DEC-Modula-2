MODULE ComputeHashFunction;

(*****************************************************************************
 *									     *
 *             Copyright 1984-1990 Digital Equipment Corporation             *
 *                         All Rights Reserved				     *
 *								             *
 * Permission to use, copy, and modify this software and its documentation   *
 * is hereby granted only under the following terms and conditions.  Both    *
 * the above copyright notice and this permission notice must appear in all  *
 * copies of the software, derivative works or modified versions, and any    *
 * portions thereof, and both notices must appear in supporting              *
 * documentation.							     *
 *									     *
 * Users of this software agree to the terms and conditions set forth        *
 * herein, and hereby grant back to Digital a non-exclusive, unrestricted,   *
 * royalty-free right and license under any changes, enhancements or         *
 * extensions made to the core functions of the software, including but not  *
 * limited to those affording compatibility with other hardware or software  *
 * environments, but excluding applications which incorporate this software. *
 * Users further agree to use their best efforts to return to Digital any    *
 * such changes, enhancements or extensions that they make and inform        *
 * Digital of noteworthy uses of this software.  Correspondence should be    *
 * provided to Digital at:						     *
 * 									     *
 *                       Director of Licensing				     *
 *                       Western Research Laboratory			     *
 *                       Digital Equipment Corporation			     *
 *                       100 Hamilton Avenue				     *
 *                       Palo Alto, California  94301  			     *
 * 									     *
 * This software may be distributed (but not offered for sale or transferred *
 * for compensation) to third parties, provided such third parties agree to  *
 * abide by the terms and conditions of this notice.  			     *
 * 									     *
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS    *
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED        *
 * WARRANTIES OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL    *
 * EQUIPMENT CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR     *
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF    *
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR     *
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR    *
 * PERFORMANCE OF THIS SOFTWARE.				    	     *
 *									     *
 *****************************************************************************)

(*************************************************************************

 The character mapping table for a dense, no-conflict hash function is
 computed for a fixed set of strings, where the form of the function is:
 
 Hash = codeForLetter[Letter1(name)] +
	codeForLetter[Letter2(name)] +
	ExtraHash(name);
 
 Suggested forms for Letter1 and Letter2 are:
 
 Letter1 = name[1]
 Letter2 = name[Length(name)]
 
 Suggested forms for ExtraHash are:
 
 ExtraHash = ORD(name[2]) - ORD(MINLETTERUSED)
	  or
 ExtraHash = Length(name)
 
 The coding table codeForLetter is computed using an algorithm described by
 Cook, C.R., "A Letter Oriented Minimal Perfect Hashing Function," SIGPLAN
 Notices, Vol. 17, No. 9 (Sep 1982).

 Written by Joel McCormack, 10-12 July 1986


 *************************************************************************)


FROM IO IMPORT Writef, output, terminal, File, Open, Close;

FROM PCodeOps IMPORT PCodeOp, PCodeOpName, pCodeOpNames;

CONST
$IF vms THEN
    SUCCESS	  = 1;
    FAILURE	  = 0;
$ELSE
    SUCCESS	  = 0;
    FAILURE	  = 1;
$END

(*************************************************************************
 User data specific constants, types
 *************************************************************************)

CONST
    MAXKEYNUMBER  = ORD(LAST(PCodeOp));
    MAXEXTRAHASH  = ORD('z') - ORD('a');
    SLOP	  = 20; (* Number of empty hash slots allowed *)
    DEFNAME       = 'Hash.def';
    MODNAME	  = 'Hash.mod';
TYPE
    Name	  = PCodeOpName;

(*************************************************************************
 General types, etc., based on user-provided data
 *************************************************************************)

TYPE
    CharSet     = SET OF CHAR;
    KeyRange    = [0..MAXKEYNUMBER];
    ExtraRange  = [0..MAXEXTRAHASH];

    Triple      = RECORD
	letter1   : CHAR;
	letter2   : CHAR;
	extraHash : ExtraRange;
	keyNumber : KeyRange;
    END (* RECORD Triple *);

    TripleArray = ARRAY KeyRange OF Triple;

VAR
    triples : TripleArray;


(*************************************************************************
 User data specific mappings into triples, names
 *************************************************************************)

PROCEDURE ConstructTriples(VAR triples : TripleArray);
    VAR op        : PCodeOp;
BEGIN
    FOR op := FIRST(PCodeOp) TO LAST(PCodeOp) DO
	WITH triples[ORD(op)] DO
	    letter1 := pCodeOpNames[op, 1];
	    letter2 := pCodeOpNames[op, 3];
	    extraHash := ORD(pCodeOpNames[op, 2]) - ORD('a');
	    keyNumber := ORD(op);
	END (* WITH *);
    END (* FOR *);
END ConstructTriples;

PROCEDURE KeyToName(key : KeyRange) : Name;
BEGIN
    RETURN pCodeOpNames[VAL(PCodeOp, key)];
END KeyToName;


(*************************************************************************
 Compute frequencies of letter1 and letter2 over all names
 *************************************************************************)

CONST
    SAMELETTERINC   = 20; (* Weight for a letter appearing as both letter1 and
			     letter2 in a name. *)
TYPE
    LetterFrequency = ARRAY CHAR OF CARDINAL;

VAR letterFrequency : LetterFrequency;
    AFrequency      : LetterFrequency;

PROCEDURE ComputeLetterFrequencies(CONST triples   : TripleArray;
				   VAR   frequency : LetterFrequency);
    VAR ch	   : CHAR;
        keyNumber  : KeyRange;
	BFrequency : LetterFrequency;
	CFrequency : LetterFrequency;

    PROCEDURE PrintFrequencies(CONST message   : ARRAY OF CHAR;
			       CONST frequency : LetterFrequency);
        VAR ch  : CHAR;
    BEGIN
	Writef(output, '%s\n', message);
	FOR ch := FIRST(CHAR) TO LAST(CHAR) DO
	    IF frequency[ch] > 0 THEN
		Writef(output, '%c %5d\n', ch, frequency[ch]);
	    END (* END *);
	END (* FOR *);
	Writef(output, '\n\n\n');
    END PrintFrequencies;

BEGIN
    (* AFrequency is just raw counts of how often each letter appears in
       letter1 or letter2; if it appears in both positions in a given
       triple, though, bump its frequency by a lot (SAMELETTERINC), as
       we only have 1 degree of freedom in determining the hash value
       for such a triple, rather than the usual 2 degrees. *)
    FOR ch := FIRST(CHAR) TO LAST(CHAR) DO
	AFrequency[ch] := 0;
    END (* FOR *);
    FOR keyNumber := FIRST(KeyRange) TO LAST(KeyRange) DO
	WITH triples[keyNumber] DO
	    IF letter1 = letter2 THEN
		INC(AFrequency[letter1], SAMELETTERINC);
	    ELSE
		INC(AFrequency[letter1]);
		INC(AFrequency[letter2]);
	    END (* IF *);
	END (* WITH triples[keyNumber] *);
    END (* FOR *);
(* |||
    PrintFrequencies('A frequency count', AFrequency);
*)

    (* BFrequency is adjusted for letters that only occur once.  In such a
       case, the codeForLetter for the letter affects only the hash value
       of the triple it is contained in, so we decrement the count (if >1)
       on the associated letter in the triple. *)
    BFrequency := AFrequency;
  (* My words ran slow with this change, hence it is commented out.  
     Use at your own risk. *)
(* |||
    FOR keyNumber := FIRST(KeyRange) TO LAST(KeyRange) DO
	WITH triples[keyNumber] DO
	    IF AFrequency[letter1] = 1 THEN
		IF BFrequency[letter2] > 1 THEN
		    DEC(BFrequency[letter2]);
		END (* IF *);
	    ELSIF AFrequency[letter2] = 1 THEN
		IF BFrequency[letter1] > 1 THEN
		    DEC(BFrequency[letter1]);
		END (* IF *);
	    END (* IF *);
	END (* WITH triples[keyNumber] *);
    END (* FOR *);
    PrintFrequencies('B frequency count', BFrequency);
||| *)

    (* CFrequency creates uses BFrequency as a major key (by multiplying
       by 1000), and constructs a minor key which is the sum of the
       BFrequencies associated with the other letter in each triple
       this letter is in. *)
(* ||| made performance MUCH worse for my data set! 
    FOR ch := FIRST(CHAR) TO LAST(CHAR) DO
	CFrequency[ch] := 1000 * BFrequency[ch];
    END (* FOR *);
    FOR keyNumber := FIRST(KeyRange) TO LAST(KeyRange) DO
	WITH triples[keyNumber] DO
	    INC(CFrequency[letter1], BFrequency[letter2]);
	    INC(CFrequency[letter2], BFrequency[letter1]);
	END (* WITH triples[keyNumber] *);
    END (* FOR *);
    PrintFrequencies('C frequency count', CFrequency);
    
    frequency := CFrequency;
*)
    
    frequency := AFrequency;
END ComputeLetterFrequencies;


(*************************************************************************
 Rearrange Letter1 and Letter2 in triples so that
    letterFrequency[letter1] >= letterFrequency[letter2] for all names
    (for frequency ties, put the smaller letter first)
 *************************************************************************)

PROCEDURE MakeLetter1MostFrequent(VAR triples : TripleArray);
    VAR keyNumber : KeyRange;
	ch        : CHAR;
BEGIN
    FOR keyNumber := FIRST(KeyRange) TO LAST(KeyRange) DO
	WITH triples[keyNumber] DO
	    IF (letterFrequency[letter1] < letterFrequency[letter2]) OR
		    ((letterFrequency[letter1] = letterFrequency[letter2])
			AND (letter1 > letter2)) THEN
		ch := letter1;
		letter1 := letter2;
		letter2 := ch;
	    END (* IF *);
	END (* WITH triples[keyNumber] *);
    END (* FOR *);
END MakeLetter1MostFrequent;


(*************************************************************************
 Sort triples in decreasing order of frequency of letter2.
 *************************************************************************)

PROCEDURE SortTriples(VAR triples : TripleArray);

    PROCEDURE Before(CONST a, b : KeyRange) : BOOLEAN;
    (* Nasty 4-key compare.  Descending order on frequency of letter2,
       ascending on letter2, letter1, extraHash *)
	VAR
	    a1, a2, b1, b2 : CHAR;
    BEGIN
	a2 := triples[a].letter2;
	b2 := triples[b].letter2;
	IF letterFrequency[a2] > letterFrequency[b2] THEN
	    RETURN TRUE;
	ELSIF letterFrequency[a2] = letterFrequency[b2] THEN
	    IF a2 < b2 THEN
		RETURN TRUE;
	    ELSIF a2 = b2 THEN
		a1 := triples[a].letter1;
		b1 := triples[b].letter1;
		IF a1 < b1 THEN
		    RETURN TRUE;
		ELSIF a1 = b1 THEN
		    RETURN (triples[a].extraHash < triples[b].extraHash);
		END (* IF *);
	    END (* IF *);
	END (* IF *);
	RETURN FALSE;
    END Before;

    PROCEDURE Exchange(CONST a, b : KeyRange);
        VAR triple : Triple;
    BEGIN
	triple := triples[a];
	triples[a] := triples[b];
	triples[b] := triple;
    END Exchange;

    PROCEDURE QuickSort(CONST low, high : KeyRange);
	VAR i, j : [0..LAST(KeyRange)+1];
    BEGIN
	ASSERT(low < high, 'Should be at least 2 elements in partition');
	(* Compute midpoint and move that element to the beginning of the
	   partition. *)
	Exchange(low, (low + high) DIV 2);
	(* Partition around midpoint *)
	i := low;
	j := high+1;
	REPEAT
	    REPEAT
		INC(i);
	    UNTIL (i > high) OR (NOT Before(i, low));
	    REPEAT
		DEC(j);
	    UNTIL NOT Before(low, j);
	    IF i < j THEN
		Exchange(i, j);
	    END (* IF *);
	UNTIL i >= j;
	(* Move midpoint partition element to (we hope close to) middle *)
	Exchange(low, j);
	(* Recurse on the two partitions around j *)
	IF (j+1) < high THEN
	    QuickSort(j+1, high);
	END (* IF *);
	IF low < (j-1) THEN
	    QuickSort(low, j-1);
	END (* IF *);
    END QuickSort;

BEGIN
    IF FIRST(KeyRange) < LAST(KeyRange) THEN
	QuickSort(FIRST(KeyRange), LAST(KeyRange));
    END (* IF *);
END SortTriples;


(*************************************************************************
 Check triples for exact duplicates of letter1, letter2, and extraHash,
 which means it is impossible to compute a non-conflicting hash function.
 *************************************************************************)

PROCEDURE CheckForDuplicateTriples(CONST triples : TripleArray);
    VAR index : KeyRange;
BEGIN
    FOR index := FIRST(KeyRange) TO LAST(KeyRange)-1 DO
	IF (triples[index].letter1 = triples[index+1].letter1) AND
		(triples[index].letter2 = triples[index+1].letter2) AND
		(triples[index].extraHash = triples[index+1].extraHash) THEN
	    Writef(terminal, 
		    'Identifiers %s and %s yield identical hash values\n', 
		    KeyToName(triples[index].keyNumber),
		    KeyToName(triples[index+1].keyNumber));
	    HALT(FAILURE);
	END (* IF *);
    END (* FOR *);
END CheckForDuplicateTriples;


CONST
    AllChars	    = CharSet{FIRST(CHAR)..LAST(CHAR)};

TYPE
    HashRange       = [0..MAXKEYNUMBER + SLOP];
    
    HashTable       = ARRAY HashRange OF RECORD
	used	    : BOOLEAN;
	tripleIndex : KeyRange;
    END (* HashTable *);
VAR
    hashTable       : HashTable;

TYPE
    CodeTable       = ARRAY CHAR OF HashRange;
VAR
    codeForLetter   : CodeTable;

PROCEDURE InitHashTable(VAR hashTable : HashTable);
    VAR hash : HashRange;
BEGIN
    FOR hash := FIRST(HashRange) TO LAST(HashRange) DO
	hashTable[hash].used := FALSE;
    END (* FOR *);
END InitHashTable;


(*************************************************************************
 Search recursively for a good codeForLetter table.  

 Each call attempts to find a value for codeForLetter[letter2] of the
 next group of entries in triples (starting at startTripleIndex) that
 have the same letter2.  (letter1 >= letter2 in all names, so letter1
 has already been assigned a code or letter1=letter2.  See starting
 call to Search in main program.)

 A call can fail because two triples in this group get the same hash
 value.  This means that we must go back to the nearest Search call that
 assigns a codeForLetter value for letter1 of either conflicting triple.
 Search returns a CharSet containing only the two chars that will make a
 difference in the hash values of these two triples.  The caller will
 continue if it is trying to assign a code value to one of these two
 letters; otherwise it will pass thru the CharSet to its caller, etc.

 A call can also fail because it cannot assign any code to letter2 that does
 not cause a hash conflict with previous groups (either at this level or at
 all recursive calls).  In this case, the search is backed up merely to the
 assignment of a code for the previous group's letter2 by returning a
 CharSet containing all letters.
 
 *************************************************************************)

PROCEDURE Search(CONST hashTable        : HashTable;
		 VAR   codeForLetter    : CodeTable;
		 CONST triples          : TripleArray;
		 CONST startTripleIndex : KeyRange) : CharSet;
VAR
    tripleIndex    : KeyRange;  (* Index over current group *)
    currentLetter2 : CHAR;
    currentCode    : HashRange; (* Code currently assigned to letter2 *)
    newHashTable   : HashTable;
    hash	   : [0..2*LAST(HashRange) + LAST(ExtraRange)];
    matchSet	   : CharSet;   (* Return set from nexted calls to Search *)
    exitReason     : (CONFLICT, GROUPSUCCESS, TOTALSUCCESS);
BEGIN
    currentLetter2 := triples[startTripleIndex].letter2;
    FOR currentCode := FIRST(HashRange) TO LAST(HashRange) DO
	newHashTable := hashTable;
	codeForLetter[currentLetter2] := currentCode;
	(* Will all triples in this group map to unused locations? *)
        tripleIndex := startTripleIndex;
	LOOP
	    hash := codeForLetter[triples[tripleIndex].letter1] +
			 codeForLetter[triples[tripleIndex].letter2] +
			 triples[tripleIndex].extraHash;
	    IF hash > LAST(HashRange) THEN
		(* Oops, hash gets too big.  Back to previous group. *)
		RETURN AllChars;
	    ELSIF newHashTable[hash].used THEN
		IF newHashTable[hash].tripleIndex >= startTripleIndex THEN
		    (* No hope, two in this group get same hash value *)
		    RETURN CharSet{triples[tripleIndex].letter1,
			    triples[newHashTable[hash].tripleIndex].letter1};
		ELSE
		    (* Just conflicted with a previous group, try next code *)
		    exitReason := CONFLICT;
		    EXIT;
		END (* IF *);
	    ELSE
		(* No conflict, enter into table *)
		newHashTable[hash].used := true;
		newHashTable[hash].tripleIndex := tripleIndex;
	    END (* IF *);
	    IF tripleIndex = LAST(KeyRange) THEN
		(* Success!  All triples have been mapped. *)
		exitReason := TOTALSUCCESS;
		EXIT;
	    END (* IF *);
	    INC(tripleIndex);
	    IF triples[tripleIndex].letter2 # currentLetter2 THEN
		exitReason := GROUPSUCCESS;
		EXIT;
	    END (* IF *);
	END (* LOOP thru this group *);
	
	CASE exitReason OF
	| CONFLICT :
	    (* Just go on to next value for currentCode. *)

	| GROUPSUCCESS :
	    (* All triples in this group are mapped into empty locations.  Try
	       the next group. *)
	    matchSet := Search(newHashTable, codeForLetter, 
			       triples, tripleIndex);
	    IF NOT (currentLetter2 IN matchSet) THEN
		RETURN matchSet;
	    END;
	    (* Somehow things did not work out in lower level, but we have the
	       potential still to make things right by trying out next code *)

	| TOTALSUCCESS :
	    PrintResults(newHashTable, codeForLetter, triples);
	    HALT(SUCCESS);
	END (* CASE *);
    END (* FOR *);
    
    (* Well, we could not find a good mapping.  Back up to previous group. *)
    RETURN AllChars;
END Search;

(*************************************************************************
 Print out the letter mapping table in a form suitable for input to the
 Modula-2 compiler, and print out the hash table just to look at it and
 admire.  (Real programs should compute that hash table, just to insure
 that there are no conflicts arising from an out-of-date codeForLetter
 table.)
 *************************************************************************)

PROCEDURE PrintResults(CONST hashTable     : HashTable;
		       CONST codeForLetter : CodeTable;
		       CONST triples       : TripleArray);
    VAR ch : CHAR;
        hash : HashRange;
        defFile : File;
	modFile : File;
BEGIN
    Writef(output, 'Solution found\n');
    Writef(output, '%d keywords in %d slots (efficiency %d%%)\n', 
	LAST(KeyRange)+1, LAST(HashRange)+1,
	(100 * (LAST(KeyRange)+1) + ((LAST(KeyRange)+1) DIV 2)) DIV 
	    (LAST(HashRange)+1));
    
(* |||
    FOR ch := FIRST(CHAR) TO LAST(CHAR) DO
	IF AFrequency[ch] > 0 THEN
	    Writef(output, "codeForLetter['%c'] = %2d;\n",
		    ch, codeForLetter[ch]);
	END (* IF *);
    END (* FOR *);
    Writef(output, '\n\n\n');
    FOR hash := FIRST(HashRange) TO LAST(HashRange) DO
	IF hashTable[hash].used then
	    Writef(output, "hashTable[%2d] = %s\n", hash, 
		KeyToName(triples[hashTable[hash].tripleIndex].keyNumber));
	END (* IF *);
    END (* FOR *);
*)

    (* Create Hash.def and Hash.mod files *)
    defFile := Open(DEFNAME, 'w');
    IF defFile = NIL THEN
	Writef(terminal, 'Could not open definition file "%s".\n', DEFNAME);
	HALT(FAILURE);
    END;
    modFile := Open(MODNAME, 'w');
    IF modFile = NIL THEN
	Writef(terminal, 'Could not open implementation file "%s".\n', MODNAME);
	HALT(FAILURE);
    END;
    Writef(defFile, 'DEFINITION MODULE Hash;\n');
    Writef(defFile, '\n');
    Writef(defFile, 'EXPORT QUALIFIED HashRange, codeForLetter;\n');
    Writef(defFile, '\n');
    Writef(defFile, 'TYPE\n');
    Writef(defFile, '    HashRange = [0..%d];\n', LAST(HashRange));
    Writef(defFile, 'VAR\n');
    Writef(defFile, '    codeForLetter : ARRAY CHAR OF CARDINAL;\n');
    Writef(defFile, '\n');
    Writef(defFile, 'END Hash.\n');
    Close(defFile);
    
    Writef(modFile, 'IMPLEMENTATION MODULE Hash;\n');
    Writef(modFile, '\n');
    Writef(modFile, 'VAR\n');
    Writef(modFile, '    letter : CHAR;\n');
    Writef(modFile, 'BEGIN\n');
    Writef(modFile, '    FOR letter := FIRST(CHAR) TO LAST(CHAR) DO\n');
    Writef(modFile, '        codeForLetter[letter] := 1000;\n');
    Writef(modFile, '    END;\n');
    FOR ch := FIRST(CHAR) TO LAST(CHAR) DO
	IF AFrequency[ch] > 0 THEN
	    Writef(modFile, "    codeForLetter['%c'] := %2d;\n",
		    ch, codeForLetter[ch]);
	END (* IF *);
    END (* FOR *);
    Writef(modFile, 'END Hash.\n');
    Close(modFile);
END PrintResults;


VAR matchSet : CharSet;
    currentCode : HashRange;

BEGIN (* ComputeHashFunction *)
    ConstructTriples(triples);
    ComputeLetterFrequencies(triples, letterFrequency);
    MakeLetter1MostFrequent(triples);
    SortTriples(triples);
    CheckForDuplicateTriples(triples);
    InitHashTable(hashTable);
    IF triples[0].letter1 = triples[0].letter2 THEN
	matchSet := Search(hashTable, codeForLetter, triples, 0);
    ELSE
	FOR currentCode := FIRST(HashRange) TO Last(HashRange) DO
	    codeForLetter[triples[0].letter1] := currentCode;
	    matchSet := Search(hashTable, codeForLetter, triples, 0);
	END (* FOR *);
    END (* IF *);
    (* Damn, it didn't work out. *)
    Writef(terminal, 'Sorry, could not find a hash function\n');
    HALT(FAILURE);
END ComputeHashFunction.
