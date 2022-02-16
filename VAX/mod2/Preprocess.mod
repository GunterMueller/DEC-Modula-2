implementation module Preprocess;

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


from io import
    SWriteF;

from MemLib import
    ALLOCATE;

from Strings import
    String, EqualAnyCase, Compare, AddChar, NewString, NewText, NonHashString;

from parameters import
    NumParameters, GetParameter;

type
    CharSet = set of char;

(* Grammar accepted and processed:

ProgramArgument     = 	"-x"Ident"="ConstantExpression
		    |   "-D"Ident"="ConstantExpression.

Directive	    =   "$IF" ConstantExpression "THEN" ProgramText
			{"$ELSIF" ConstantExpression "THEN" ProgramText}
			["$ELSE" ProgramText]
			"$END" [";"]
		    |   "$CONST" Ident "=" ConstantExpression [";"].
(It actually only parses one line at a time, and keeps track of its
 state in the array stateStack)

ConstantExpression  =	SimpleConstExpr [Relation SimpleConstExpr].

Relation	    =   "=" | "#" | "<>" | "<" | "<=" | ">" | ">=" | IN.

SimpleConstExpr     =   ["+" | "-"] ConstTerm {AddOperator ConstTerm}.

AddOperator	    =   "+" | "-" | "OR".

ConstTerm	    =   ConstFactor {MultiplyOperator ConstFactor}.

MultiplyOperator    =   "*" | "DIV" | "MOD" | "AND" | "&".

ConstFactor	    =   Ident | Number | String | Set | "NOT" ConstFactor
		    |   "~" ConstFactor | "(" ConstantExpression ")".

Set		    =  "{" SetRange {"," SetRange} "}".

SetRange	    =  Number [".." Number].

Number		    =   digit {digit}.

digit		    =   "0".."9".

String		    =   "'" {character} "'"
		    |   '"' {character} '"'.

Ident		    =   letter {letter | digit}.

letter		    =   "A".."Z" | "a".."z" | "_".
*)


type
    TokenKind = (TKIF, TKELSIF, TKTHEN, TKELSE, TKEND, TKCONST, TKSEMICOLON,
		 TKEQUALS, TKNOTEQUAL, TKLESS, TKLSEQUAL, TKGREQUAL, TKGREATER,
		    TKIN,
		 TKPLUS, TKMINUS, TKOR, 
		 TKASTERISK, TKDIV, TKMOD, TKAND, 
		 TKLPAREN, TKRPAREN, TKNOT,
		 TKLBRACE, TKRBRACE, TKCOMMA, TKDOTDOT,
		 TKIDENT, TKINTEGER, TKSTRING, TKBOOLEAN, TKSET,
		 TKANY, (* for error handling of undefined identifiers *)
		 TKENDOFLINE, TKNULL);
    TokenSet  = set of TokenKind;

    Bits       = [0..255];
    BitsSet    = set of Bits;
    DefineKind = (DEFINEPROMPT, DEFINECONST, DEFINELITERAL);

    (* Token is used both to return values from the scanner NextToken, and to
	return expression values from the parsing procedures. *)
    Token   =   record
	case kind : TokenKind of
	| TKINTEGER :   int    : integer;
	| TKBOOLEAN :   bool   : boolean;
	| TKSET     :   bits   : BitsSet;
	| TKSTRING, 
	  TKANY     :   string : String;
	end;
	(* Really just for INTEGER, BOOLEAN, SET, STRING *)
	defined   : DefineKind;
	(* Just for tokens that have a symbolic name *)
	name	  : String;
    end (* record Token *);

var
    token       : Token;


module SymbolTable;
    import ALLOCATE, String, NewText, EqualAnyCase, 
	Token, TokenKind, DefineKind, Compare;

    export LookupSymbol, EnterSymbol, OverrideSymbol, InitSymbol;

(* Keeps all "reserved words" and identifiers in a hash table.  No
   scoping or redefinition. 

   EnterSymbol EXPECTS that the name is not in the table, and simply
   stuffs the name at the front of the list for that hash index.
   
   LookupSymbol looks up a name.  If the name is a predefined name, ala
   the Modula-2 compiler's behavior on reserved words and builtins, and
   standardKeywords is false, the name will be found no matter what case.
   If the name is found, its value (a token) is returned.  Else the token
   TKIDENT is returned.
   
   EnterPredefined enters a predefined name or reserved word.  It sets the
   anyCase bit TRUE, so case doesn't matter in LookupSymbol.  It sets the
   defined field to DEFINECONST so that predefined constants cannot be
   redeclared.

*)
   
    const
	HASHSIZE = 41;

    type
        HashIndex = [0..HASHSIZE-1];
	
	Symbol  = pointer to SymbolRec;
	SymbolRec = record
	    next    : Symbol;
	    anyCase : boolean;
	    value   : Token;
	end (* Symbol *);

    var 
	hashTable   : array HashIndex of Symbol;
	latestSymbol: Symbol;

    procedure LookupSymbol(const name : String) : Token;
	var search    : Symbol;
	    result    : Token;
    begin
	search := hashTable[name^.hash mod HASHSIZE];
	while search # nil do
	    if (Compare(search^.value.name, name) = 0) or
		   (search^.anyCase and 
		      EqualAnyCase(search^.value.name, name)) then
		return search^.value;
	    else
		search := search^.next;
	    end;
	end (* while *);
	result.kind := TKIDENT;
	result.name := name;
	return result;
    end LookupSymbol;

    procedure EnterSymbol(const value : Token);
	var hashIndex : HashIndex;
    begin
	(* EnterSymbol assumes you never re-enter a name! *)
	hashIndex := value.name^.hash mod HASHSIZE;
	new(latestSymbol);
	latestSymbol^.next := hashTable[hashIndex];
	hashTable[hashIndex] := latestSymbol;
	latestSymbol^.anyCase := FALSE;
	latestSymbol^.value := value;
    end EnterSymbol;


    procedure OverrideSymbol(const value : Token);
	var search : Symbol;
    begin
        search := hashTable[value.name^.hash mod HASHSIZE];
	while search # nil do
	    if (Compare(search^.value.name, value.name) = 0) or
		   (search^.anyCase and 
		      EqualAnyCase(search^.value.name, value.name)) then
		search^.value := value;
		return;
	    else
		search := search^.next;
	    end;
	end (* while *);
	Assert(false, "OverrideSymbol: Couldn't find symbol to override?");
    end OverrideSymbol;

    procedure InitSymbol(standardKeywordFlag: boolean);

	procedure EnterPredefined(const name : array of char;
				  const kind : TokenKind);
	    var
		value   : Token;
	begin
	    value.kind := kind;
	    value.defined := DEFINECONST;
	    value.name := NewText(name);
	    EnterSymbol(value);
	    latestSymbol^.anyCase := not standardKeywordFlag;
	end EnterPredefined;

    begin
	EnterPredefined('IF',     TKIF      );
	EnterPredefined('ELSIF',  TKELSIF   );
	EnterPredefined('THEN',   TKTHEN    );
	EnterPredefined('ELSE',   TKELSE    );
	EnterPredefined('END',    TKEND     );
	EnterPredefined('CONST',  TKCONST   );
	EnterPredefined('IN',     TKIN      );
	EnterPredefined('OR',     TKOR      );
	EnterPredefined('DIV',    TKDIV     );
	EnterPredefined('MOD',    TKMOD     );
	EnterPredefined('AND',    TKAND     );
	EnterPredefined('NOT',    TKNOT     );

	EnterPredefined('FALSE',  TKBOOLEAN );
	latestSymbol^.value.bool := FALSE;
	EnterPredefined('TRUE',   TKBOOLEAN );
	latestSymbol^.value.bool := TRUE;

	EnterPredefined('unix',   TKBOOLEAN );
$if unix then
	latestSymbol^.value.bool := TRUE;
$else
	latestSymbol^.value.bool := FALSE;
$end

	EnterPredefined('vms',    TKBOOLEAN );
$if vms then
	latestSymbol^.value.bool := TRUE;
$else
	latestSymbol^.value.bool := FALSE;
$end
	EnterPredefined('vax',   TKBOOLEAN );
    end InitSymbol;


    var hashIndex : HashIndex;
begin (* SymbolTable *)
    for hashIndex := 0 to HASHSIZE-1 do
	hashTable[hashIndex] := nil;
    end;
end SymbolTable;
   

(* The IF THEN ... ELSIF state is kept in stateStack, which starts off at
   NOTHING.  TRUENOTSEEN means we are in a conditional statement, but have
   not yet seen a TRUE condition.  TRUEPART means this is the program text
   for the first TRUE condition.  TRUESEEN means either (1) we are in a
   conditional and have already processed a TRUE condition, or (2) we are in a
   conditional which is nested inside of a non-TRUEPART branch of an enclosing
   conditional.  elseSeen is TRUE if the ELSE of a conditional has been
   processed, and is used to ensure the only thing following a ELSE is a
   END.
*)

const
    STACKSIZE  = 31;
type
    State      = (NOTHING, TRUENOTSEEN, TRUEPART, TRUESEEN);
    StackIndex = [0..STACKSIZE];
    StateSet   = set of State;
var
    stateStack : array StackIndex of record
	state       : State;
	elseSeen    : boolean;
	fileNest    : cardinal; (* what file nesting level $IF seen in *)
	lineNumber  : integer;  (* of the $IF *)
    end (* stateStack *);
    top	    : StackIndex;
    fileNest: cardinal;

procedure Directive (const line       : array of char;
		     const lineNumber : integer;
		     const error      : ErrorProc;
		     var   skip       : boolean);

    procedure Error(const message : array of char);
    begin
	error(message, startOfToken);
    end Error;
    
    module Scanner;
	import TokenKind, DefineKind, token, CharSet, AddChar, NewString,
	       NonHashString, LookupSymbol, Error, line;
	export startOfToken, nextCharPos, NextToken, InitScanner;
    
	var startOfToken : cardinal;
	    nextCharPos  : cardinal;
    
	procedure InitScanner();
	begin
	    nextCharPos := 0;
	    while line[nextCharPos] in CharSet{' ', '\t'} do
		inc(nextCharPos);
	    end;
	    if line[nextCharPos] = '$' then
		inc(nextCharPos);
	    end;
	    NextToken();
	end InitScanner;
	
	procedure NextToken();
	begin
	    token.kind := TKNULL;
	    token.defined := DEFINELITERAL;
	    token.name := NIL;
	    repeat
		startOfToken := nextCharPos;
		case line[nextCharPos] of
		| ' ', '\t', '\n' :
		    inc(nextCharPos);
    
		| 'a'..'z', 'A'..'Z', '_' :
		    repeat
			AddChar(line[nextCharPos]);
			inc(nextCharPos);
		    until not (line[nextCharPos] in 
				CharSet{'a'..'z', 'A'..'Z', '_', '0'..'9'});
		    token := LookupSymbol(NewString());
		 
		| '0'..'9' :
		    token.kind := TKINTEGER;
		    token.int := 0;
		    repeat
			token.int :=
			    token.int*10 + ord(line[nextCharPos]) - ord('0');
			inc(nextCharPos);
		    until not (line[nextCharPos] in CharSet{'0'..'9'});
		    
		| '"' :
		    token.kind := TKSTRING;
		    inc(nextCharPos);
		    repeat
			AddChar(line[nextCharPos]);
			inc(nextCharPos);
		    until line[nextCharPos] in CharSet{'"', 0C};
		    if line[nextCharPos] = 0C then
			Error('String cannot cross line boundary');
		    else
			inc(nextCharPos);
		    end;
		    token.string := NonHashString();
	
		| "'" :
		    token.kind := TKSTRING;
		    inc(nextCharPos);
		    repeat
			AddChar(line[nextCharPos]);
			inc(nextCharPos);
		    until line[nextCharPos] in CharSet{"'", 0C};
		    if line[nextCharPos] = 0C then
			Error('String cannot cross line boundary');
		    else
			inc(nextCharPos);
		    end;
		    token.string := NonHashString();
	
		| '{' :
		    inc(nextCharPos);
		    token.kind := TKLBRACE;

		| '.' :
		    inc(nextCharPos);
		    if line[nextCharPos] = '.' then
			inc(nextCharPos);
		    else
			Error('".." expected');
		    end;
		    token.kind := TKDOTDOT;
		    
		| ',' :
		    inc(nextCharPos);
		    token.kind := TKCOMMA;

		| '}' :
		    inc(nextCharPos);
		    token.kind := TKRBRACE;

		| ';' :
		    inc(nextCharPos);
		    token.kind := TKSEMICOLON;

		| '=' :
		    inc(nextCharPos);
		    token.kind := TKEQUALS;
		    
		| '<' :
		    inc(nextCharPos);
		    if line[nextCharPos] = '>' then
			inc(nextCharPos);
			token.kind := TKNOTEQUAL;
		    elsif line[nextCharPos] = '=' then
			inc(nextCharPos);
			token.kind := TKLSEQUAL;
		    else
			token.kind := TKLESS;
		    end;
	
		| '#' :
		    inc(nextCharPos);
		    token.kind := TKNOTEQUAL;
		   
		| '>' : 
		    inc(nextCharPos);
		    if line[nextCharPos] = '=' then
			inc(nextCharPos);
			token.kind := TKGREQUAL;
		    else
			token.kind := TKGREATER;
		    end;
		
		| '+' :
		    inc(nextCharPos);
		    token.kind := TKPLUS;
		    
		| '-' :
		    inc(nextCharPos);
		    token.kind := TKMINUS;
		    
		| '*' :
		    inc(nextCharPos);
		    token.kind := TKASTERISK;
	
		| '&' :
		    inc(nextCharPos);
		    token.kind := TKAND;
	
		| '~' :
		    inc(nextCharPos);
		    token.kind := TKNOT;
	
		| '(' :
		    inc(nextCharPos);
		    if line[nextCharPos] = '*' then     (* a comment *)
			inc(nextCharPos);
			loop
			    if line[nextCharPos] = '*' then
				inc(nextCharPos);
				if line[nextCharPos] = ')' then
				    inc(nextCharPos);
				    exit;
				end;
			    elsif line[nextCharPos] = 0C then
				Error('Comment cannot cross line boundary');
				exit;
			    else
				inc(nextCharPos);
			    end;
			end (* loop *);
		    else
			token.kind := TKLPAREN;
		    end;
	
		| ')' :
		    inc(nextCharPos);
		    token.kind := TKRPAREN;
	
		| 0C :
		    (* DO NOT inc(nextCharPos) *)
		    token.kind := TKENDOFLINE;
		
		| else
		    Error('Invalid character');
		    inc(nextCharPos);
		    
		end (* case *);
	    until token.kind # TKNULL;
	end NextToken;
	
    end Scanner;
    
    procedure Scan(const stopTokens : TokenSet);
    begin
	while not (token.kind in stopTokens) do
	    NextToken();
	end;
    end Scan;

    procedure TestToken(const okayTokens : TokenSet;
			const stopTokens : TokenSet;
			const message    : array of char);
    begin
	if not (token.kind in okayTokens) then
	    Error(message);
	    Scan(stopTokens + okayTokens);
	end;
    end TestToken;
    

    procedure ConstantExpression(const followSet : TokenSet; 
				 var   result    : Token);
    
	procedure SimpleConstExpr(const followSet : TokenSet; 
				  var   result    : Token);
    
	    procedure ConstTerm(const followSet : TokenSet; 
				var   result    : Token);
    
		procedure ConstFactor(const followSet : TokenSet; 
				      var   result    : Token);

    procedure ConstSet(const followSet : TokenSet;
		       var   result    : Token);
	procedure ConstSetRange(const followSet : TokenSet;
				var   result    : Token);
	    procedure ConstSetRangeNumber(const followSet : TokenSet;
					  var   result    : Token);
		var s : array [0..1023] of char;
	    begin
		result := token;
		case token.kind of
		| TKINTEGER :
		    if result.int < FIRST(Bits) then
			SWriteF(s, 'Set element cannot be smaller then %d',
			    FIRST(Bits));
			Error(s);
			result.int := FIRST(Bits);
		    elsif result.int > LAST(Bits) then
			SWriteF(s, 'Set element cannot be larger then %d',
			    LAST(Bits));
			Error(s);
			result.int := LAST(Bits);
		    end;
		    NextToken();
		| TKIDENT :
		    Error('Undefined identifier');
		    EnterSymbol(token);
		    NextToken();
		| TKANY :
		    NextToken();
		| else
		    Error('Number expected');
		    Scan(followSet);
		end;
	    end ConstSetRangeNumber;

        var
	    lower, upper : Token;
	    i : integer;
	begin (* ConstSetRange *)
	    ConstSetRangeNumber(TokenSet{TKDOTDOT} + followSet, lower);
	    if token.kind = TKDOTDOT then
		NextToken();
		ConstSetRangeNumber(followSet, upper);
	    else
		upper := lower;
	    end;
	    result.kind := TKSET;
	    if (lower.kind = TKINTEGER) and (upper.kind = TKINTEGER) then
(* |||		result.bits := BitsSet{lower.int .. upper.int}; *)
		for i := lower.int to upper.int do
		    incl(result.bits, i);
		end;
	    else
		result.bits := BitsSet{FIRST(Bits) .. LAST(Bits)};
	    end;
	end ConstSetRange;
    
	var 
	    newRange : Token;
    begin (* ConstSet *)
	result.kind := TKSET;
	result.bits := BitsSet{};
	loop
	    ConstSetRange(TokenSet{TKCOMMA,TKIDENT,TKINTEGER,TKANY} + followSet,
		    newRange);
	    result.bits := result.bits + newRange.bits;
	    case token.kind of
	    | TKCOMMA :
		NextToken();
	    | TKDOTDOT, TKIDENT, TKINTEGER, TKANY :
		Error('"," expected');
	    | else
		exit;
	    end;
	end (* loop *);
    end ConstSet;

		    const FACTORBEGINTOKENS = 
			    TokenSet{TKIDENT, TKINTEGER, TKSTRING, TKBOOLEAN,
				       TKSET, TKLBRACE, TKANY, TKNOT, TKLPAREN};

		begin (* ConstFactor *)
		    result.kind := TKANY;
		    TestToken(FACTORBEGINTOKENS, followSet,
		       'Identifier, number, string, set, NOT, or "(" expected');
		    while token.kind in FACTORBEGINTOKENS do
			case token.kind of
			| TKIDENT :
			    Error('Undefined identifier');
			    token.kind := TKANY;
			    EnterSymbol(token);
			    result := token;
			    NextToken();
			| TKINTEGER, TKSTRING, TKBOOLEAN, TKSET, TKANY :
			    result := token;
			    NextToken();
			| TKLBRACE :
			    NextToken();
			    ConstSet(TokenSet{TKRBRACE}+followSet, result);
			    TestToken(TokenSet{TKRBRACE}, followSet,
			        '"}" expected');
			    if token.kind = TKRBRACE then
				NextToken();
			    end;
			| TKNOT :
			    NextToken();
			    ConstFactor(followSet, result);
			    if result.kind = TKBOOLEAN then
				result.bool := NOT result.bool;
			    else
			        Error('NOT allowed only on boolean expression');
				result.kind := TKBOOLEAN;
				result.bool := TRUE;
			    end;
			| TKLPAREN :
			    NextToken();
			    ConstantExpression(TokenSet{TKRPAREN}+followSet,
					       result);
			    TestToken(TokenSet{TKRPAREN}, followSet,
				'")" expected');
			    if token.kind = TKRPAREN then
				NextToken();
			    end;
			end (* case token.kind *);
			TestToken(followSet, FACTORBEGINTOKENS,
			     'Operator expected');
		    end (* while *);

		end ConstFactor;
    
	        const
		    MULTIPLYTOKENS = TokenSet{TKASTERISK, TKDIV, TKMOD, TKAND};
		var rightFactor : Token;
		    operator    : TokenKind;
	    begin (* ConstTerm *)
		ConstFactor(MULTIPLYTOKENS + followSet, result);
		while token.kind in MULTIPLYTOKENS do
		    operator := token.kind;
		    NextToken();
		    ConstFactor(MULTIPLYTOKENS + followSet, rightFactor);
		    if result.kind # rightFactor.kind then
			if (result.kind # TKANY) and 
				(rightFactor.kind # TKANY) then
			    Error('Operands must be the same type');
			end;
		    else
			case result.kind of
			| TKINTEGER :
			    case operator of
			    | TKASTERISK : 
				result.int := result.int * rightFactor.int;
			    | TKDIV :
				if rightFactor.int = 0 then
				    Error('DIV by 0 not allowed');
				else
				    result.int :=
					result.int div rightFactor.int;
				end;
			    | TKMOD :
				if rightFactor.int = 0 then
				    Error('MOD by 0 not allowed');
				else
				    result.int :=
					result.int mod rightFactor.int;
				end;
			    | TKAND :
				Error('AND allowed on booleans only');
			    end (* case operator *);
			
			| TKBOOLEAN :
			    case operator of
			    | TKAND :
				result.bool := result.bool AND rightFactor.bool;
			    | TKASTERISK, TKDIV, TKMOD :
				Error('*, DIV, MOD not allowed on booleans');
			    end (* case operator *);
			
			| TKSTRING :
			    Error('*, DIV, MOD, AND not allowed on strings');

			| TKSET :
			    case operator of
			    | TKASTERISK :
				result.bits := result.bits * rightFactor.bits;
			    | TKDIV, TKMOD, TKAND :
				Error('DIV, MOD, and AND not allowed on sets');
			    end (* case operator *);
			end (* case result.kind *);
		    end (* if result.kind # rightFactor.kind *);
		end (* while *);
	    end ConstTerm;
    
	    const
		ADDTOKENS = TokenSet{TKOR, TKPLUS, TKMINUS};
	    var rightTerm : Token;
		operator  : TokenKind;
	begin (* SimpleConstExpr *)
	    operator := TKNULL;
	    if token.kind in TokenSet{TKPLUS, TKMINUS} then
		operator := token.kind;
		NextToken();
	    end;
	    ConstTerm(ADDTOKENS + followSet, result);
	    if operator # TKNULL then
		if result.kind # TKINTEGER then
		    if result.kind # TKANY then
			Error('+, - allowed on integer only');
		    end;
		else
		    if token.kind = TKMINUS then
			result.int := - result.int;
		    end;
		end;
	    end (* if token.kind # TKNULL *);
	    while token.kind in ADDTOKENS do
		operator := token.kind;
		NextToken();
		ConstTerm(ADDTOKENS + followSet, result);
		if result.kind # rightTerm.kind then
		    if (result.kind # TKANY) and 
			    (rightTerm.kind # TKANY) then
			Error('Operands must be the same type');
		    end;
		else
		    case result.kind of
		    | TKINTEGER :
			case operator of
			| TKPLUS :
			    result.int := result.int + rightTerm.int;
			| TKMINUS :
			    result.int := result.int - rightTerm.int;
			| TKOR :
			    Error('OR not allowed on integers');
			end (* case operator *);
		    
		    | TKBOOLEAN :
			case operator of
			| TKOR :
			    result.bool := result.bool OR rightTerm.bool;
			| TKPLUS, TKMINUS :
			    Error('+, - not allowed on booleans');
			end (* case operator *);
		    
		    | TKSTRING :
			Error('+, -, OR not allowed on strings');

		    | TKSET :
			case operator of
			| TKPLUS :
			    result.bits := result.bits + rightTerm.bits;
			| TKMINUS :
			    result.bits := result.bits - rightTerm.bits;
			| TKOR :
			    Error('OR not allowed on sets');
			end (* case operator *);

		    end (* case result.kind *);
		end (* if result.kind # rightTerm.kind *);
	    end (* while token.kind in ADDTOKENS *);
	end SimpleConstExpr;
    
	const
	    COMPARETOKENS = TokenSet{TKEQUALS, TKNOTEQUAL, TKLESS, 
				     TKLSEQUAL, TKGREQUAL, TKGREATER, TKIN};
	var operator : TokenKind;
	    leftSimp : Token;
	    rightSimp: Token;

    begin (* ConstantExpression *)
	SimpleConstExpr(COMPARETOKENS + followSet, result);
	if token.kind in COMPARETOKENS then
	    leftSimp := result;
	    result.kind := TKBOOLEAN;
	    result.bool := TRUE;
	    operator := token.kind;
	    NextToken();
	    SimpleConstExpr(COMPARETOKENS + followSet, rightSimp);
	    if (leftSimp.kind = TKANY) or (rightSimp.kind = TKANY) then
		(* Don't bother doing anything *)
	    elsif operator = TKIN then
		if (leftSimp.kind = TKINTEGER) and (rightSimp.kind = TKSET) then
		    result.bool := leftSimp.int IN rightSimp.bits;
		else
		    if leftSimp.kind # TKINTEGER then
			Error('Left operand must be an integer');
		    end;
		    if rightSimp.kind # TKSET then
			Error('Right operand must be a set');
		    end;
		end;
	    elsif leftSimp.kind # rightSimp.kind then
		Error('Operands must be the same type');
	    else
		case leftSimp.kind of
		| TKINTEGER :
		    case operator of
		    | TKEQUALS :
			result.bool := leftSimp.int = rightSimp.int;
		    | TKNOTEQUAL :
			result.bool := leftSimp.int # rightSimp.int;
		    | TKLESS :
			result.bool := leftSimp.int < rightSimp.int;
		    | TKLSEQUAL :
			result.bool := leftSimp.int <= rightSimp.int;
		    | TKGREQUAL :
			result.bool := leftSimp.int >= rightSimp.int;
		    | TKGREATER :
			result.bool := leftSimp.int > rightSimp.int;
		    end (* case operator *);
		
		| TKBOOLEAN :
		    case operator of
		    | TKEQUALS :
			result.bool := leftSimp.bool = rightSimp.bool;
		    | TKNOTEQUAL :
			result.bool := leftSimp.bool # rightSimp.bool;
		    | TKLESS :
			result.bool := leftSimp.bool < rightSimp.bool;
		    | TKLSEQUAL :
			result.bool := leftSimp.bool <= rightSimp.bool;
		    | TKGREQUAL :
			result.bool := leftSimp.bool >= rightSimp.bool;
		    | TKGREATER :
			result.bool := leftSimp.bool > rightSimp.bool;
		    end (* case operator *);
		
		| TKSTRING :
		    case operator of
		    | TKEQUALS :
			result.bool := Compare(leftSimp.string,
						rightSimp.string) = 0;
		    | TKNOTEQUAL :
			result.bool := Compare(leftSimp.string,
						rightSimp.string) # 0;
		    | TKLESS :
			result.bool := Compare(leftSimp.string,
						rightSimp.string) < 0;
		    | TKLSEQUAL :
			result.bool := Compare(leftSimp.string,
						rightSimp.string) <= 0;
		    | TKGREQUAL :
			result.bool := Compare(leftSimp.string,
						rightSimp.string) >= 0;
		    | TKGREATER :
			result.bool := Compare(leftSimp.string,
						rightSimp.string) > 0;
		    end (* case operator *);

		| TKSET :
		    case operator of
		    | TKEQUALS :
			result.bool := leftSimp.bits = rightSimp.bits;
		    | TKNOTEQUAL :
			result.bool := leftSimp.bits # rightSimp.bits;
		    | TKLSEQUAL :
			result.bool := leftSimp.bits <= rightSimp.bits;
		    | TKGREQUAL :
			result.bool := leftSimp.bits >= rightSimp.bits;
		    | TKLESS :
			result.bool := (leftSimp.bits <= rightSimp.bits)
				   AND (leftSimp.bits #  rightSimp.bits);
		    | TKGREATER :
			result.bool := (leftSimp.bits >= rightSimp.bits)
				   AND (leftSimp.bits #  rightSimp.bits);
		    end (* case operator *);

		end (* case leftSimp.kind *);
	    end (* if leftSimp.kind # rightSimp.kind *);
	end (* if token.kind in COMPARETOKENS *);
    end ConstantExpression;

    procedure Push(state : State);
    begin
	top := top + 1;
	stateStack[top].state := state;
	stateStack[top].elseSeen := FALSE;
	stateStack[top].lineNumber := lineNumber;
	stateStack[top].fileNest := fileNest;
    end Push;


    var
	constName   : String;
	result      : Token;
	defined     : DefineKind;

begin (* Directive *)
    InitScanner();
    case token.kind of
    | TKIF :
	NextToken();
	ConstantExpression(TokenSet{TKTHEN, TKENDOFLINE}, result);
	if result.kind # TKBOOLEAN then
	    if result.kind # TKANY then
		Error('Expression must be of type boolean');
	    end;
	    result.kind := TKBOOLEAN;
	    result.bool := TRUE;
	end;
	case stateStack[top].state of
	| NOTHING, TRUEPART :       (* continue processing text? *)
	    if result.bool then
		Push(TRUEPART);
	    else
		Push(TRUENOTSEEN);
	    end;
	| TRUENOTSEEN, TRUESEEN :   (* skipping text, IF irrelevant *)
	    Push(TRUESEEN);
	end (* case *);
	if token.kind = TKTHEN then
	    NextToken();
	    if token.kind # TKENDOFLINE then
		Error('End-of-line expected');
	    end;
	else
	    Error('THEN expected');
	end;


    | TKELSIF :
	NextToken();
	ConstantExpression(TokenSet{TKTHEN, TKENDOFLINE}, result);
	if result.kind # TKBOOLEAN then
	    if result.kind # TKANY then
		Error('Expression must be of type boolean');
	    end;
	    result.kind := TKBOOLEAN;
	    result.bool := TRUE;
	end;
	case stateStack[top].state of
	| NOTHING :
	    Error('$ELSIF must have a preceding $IF');
	    if result.bool then
		Push(TRUEPART);
	    else
		Push(TRUENOTSEEN);
            end;
	| TRUEPART : (* just got thru true part, ELSIF irrelevant *) 
	    stateStack[top].state := TRUESEEN;
	| TRUESEEN : (* parsed true part a while ago, ELSIF irrelevant *)
	    (* Nothing *)
	| TRUENOTSEEN : (* no true section yet.  Is this cond. true? *)
   	    if result.bool then
		stateStack[top].state := TRUEPART;
	    end;
	end (* case *);
	if stateStack[top].elseSeen = TRUE then
	    Error('$ELSIF cannot follow $ELSE');
	elsif stateStack[top].fileNest # fileNest then
	    Error('$ELSIF must have preceding $IF in same file'); 
	end;
	if token.kind = TKTHEN then
	    NextToken();
	    if token.kind # TKENDOFLINE then
		Error('End-of-line expected');
	    end;
	else
	    Error('THEN expected');
	end;

    | TKELSE :
	case stateStack[top].state of
	| NOTHING :
	    Error('$ELSE must have a preceding $IF');
	    Push(TRUEPART);
	| TRUEPART :
	    stateStack[top].state := TRUESEEN;
	| TRUESEEN :
            (* Nothing *)
        | TRUENOTSEEN :
	    stateStack[top].state := TRUEPART;
	end (* case *);
	if stateStack[top].elseSeen = TRUE then
	    Error('$ELSE cannot follow $ELSE');
	elsif stateStack[top].fileNest # fileNest then
	    Error('$ELSE must have preceding $IF in same file'); 
	end;
	stateStack[top].elseSeen := TRUE;
	NextToken();
	if token.kind # TKENDOFLINE then
	    Error('End-of-line expected');
	end;

    | TKEND :
	case stateStack[top].state of
	| NOTHING :
	    Error('$END must have a preceding $IF');
	| TRUEPART, TRUESEEN, TRUENOTSEEN :
	    if stateStack[top].fileNest # fileNest then
		Error('$END must have preceding $IF in same file');
	    end;
	    top := top - 1;
	end (* case *);
	NextToken();
	if token.kind = TKSEMICOLON then
	    NextToken();
	end;
	if token.kind # TKENDOFLINE then
	    Error('End-of-line expected');
	end;

    | TKCONST, TKMINUS :
	if token.kind = TKMINUS then (* -D is not a separate token, yechh *)
	    defined := DEFINEPROMPT;
	    if line[nextCharPos] = 'x' then
		Error('Use "-D" instead of "-x"');
		inc(nextCharPos);
	    elsif line[nextCharPos] = 'D' then
		inc(nextCharPos);
	    else
		Error('"-D" expected');
	    end;
	else
	    defined := DEFINECONST;
	end;
	NextToken();
	if token.kind = TKIDENT then (* we know it isn't defined *)
	    constName := token.name;
	    NextToken();
	    if token.kind = TKEQUALS then
		NextToken();
		ConstantExpression(TokenSet{TKSEMICOLON, TKENDOFLINE}, result);
		result.defined := defined;
		result.name := constName;
		EnterSymbol(result);
		if token.kind = TKSEMICOLON then
		    NextToken();
		end;
		if token.kind # TKENDOFLINE then
		    Error('End-of-line expected');
		end;
	    else
		Error('"=" expected');
	    end;
	elsif token.kind in TokenSet{TKINTEGER, TKBOOLEAN, TKSTRING, TKSET} then
	    case token.defined of
	    | DEFINEPROMPT :
		(* Prompt overrides $CONST value *)
	    | DEFINECONST :
		if defined = DEFINEPROMPT then
		    (* Override predefined identifier (like "unix" or "vms" *)
		    constName := token.name;
		    NextToken();
		    if token.kind = TKEQUALS then
			NextToken();
			ConstantExpression(
			    TokenSet{TKSEMICOLON, TKENDOFLINE}, result);
			result.defined := DEFINEPROMPT;
			result.name := constName;
			OverrideSymbol(result);
			if token.kind = TKSEMICOLON then
			    NextToken();
			end;
			if token.kind # TKENDOFLINE then
			    Error('End-of-line expected');
			end;
		    else
			Error('"=" expected');
		    end;
		else
		    Error("Cannot redefine a constant that's already declared");
		end;
	    | DEFINELITERAL :
		Error('Cannot redefine a literal constant');
	    end;
	else
	    Error('Identifier expected');
	end;
    else
	Error('$IF, $ELSIF, $ELSE, $END, or $CONST expected');
    end (* case token.kind *);

    skip := stateStack[top].state in StateSet{TRUENOTSEEN, TRUESEEN};
end Directive;



    var outsideInitError : InitErrorProc;
	line : array [0..131] of char;
	
procedure MyInitError(const message : array of char;
		      const position: integer);
begin
    outsideInitError(message, line, position);
end MyInitError;

procedure InitPreprocess(const standardKeywordFlag  : boolean;
		         const error		    : InitErrorProc);
    var parm : cardinal;
	skip : boolean;
	len  : integer;
begin
    InitSymbol(standardKeywordFlag);
    top := 0;
    stateStack[0].state := NOTHING;
    stateStack[0].elseSeen := FALSE;
    stateStack[0].fileNest := 0;

    outsideInitError := error;
    for parm := 1 to NumParameters-1 do
	GetParameter(parm, line, len);
	if (line[0] = '-') and ((line[1] = 'x') or (line[1] = 'D')) then
	    Directive(line, 0, MyInitError, skip);
	end;
    end;
end InitPreprocess;


procedure PreprocessStartFile ();
begin
    inc(fileNest);
end PreprocessStartFile;


procedure PreprocessEndFile (const error : ErrorNumberProc);
begin
    while stateStack[top].fileNest = fileNest do
	error('Unclosed $IF at line', stateStack[top].lineNumber);
	dec(top);
    end;
    dec(fileNest);
end PreprocessEndFile;
	

begin
fileNest := 0;
end Preprocess.
