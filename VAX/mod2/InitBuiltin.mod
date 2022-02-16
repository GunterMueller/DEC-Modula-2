implementation module InitBuiltin;

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


from MemLib import
    ALLOCATE;

from Machine import
    HugeInteger, WORDSIZE, BYTESIZE, CHARSIZE, BOOLEANSIZE,
    LONGREALSIZE, MAXINT, MAXCARD,
    ExitProgram;
$if modula2 then
from Machine import MININT, BYTESPERWORD, UNITSIZE; 
$else
from Machine import FILESIZE;
$end

from Tokens import
    Token;

from Strings import
    String, NewText, NonHashText;

from Globals import
    TargetMachine, target;
$if modula2 then
    from Globals import host, standardCardinalFlag;
$end

from Symbols import
    MAXBUILTINSCOPES, Scope, ONECASE, Symbol, ConstNode, TypeNode,
    VarNode, ProcNode, EnumNode, IdentList, SYMCONST, SYMTYPE, MEMNORMAL, 
    DataType, DataTypeSet, BuiltinProcType, ModuleNode,
    integerTypeNode, realTypeNode, charTypeNode,
    cardinalTypeNode, bitsetTypeNode, booleanTypeNode,
    wordTypeNode, byteTypeNode, addressTypeNode, stringTypeNode,
    cardIntTypeNode, 
    fileTypeNode, arrayOfCharTypeNode, longrealTypeNode,
    realConstTypeNode, charConstTypeNode, processTypeNode,
    packedCharTypeNode, packedCharConstTypeNode, ioStringTypeNode,
    packedBooleanTypeNode, packedByteTypeNode,
    anyTypeNode, procTypeNode, indexableTypes, generateBlockNumber,
    currScope, builtinScope, DefineSymbol, constBYTESIZE,
    packOrder, PackOrder, intsetTypeNode, unknownSetTypeNode
$if modula2 then
    , GSNORMAL
$else
    , alfaTypeNode, jmpBufTypeNode, GSEXTERNAL,
    outputDeclared, outputString, inputDeclared, inputString,
    fnilProcNode, getProcNode, putProcNode, unitProcNode,
    longfloatProcNode, read4ProcNode, read8ProcNode, readeProcNode,
    readcProcNode, writelnProcNode,
    writecProcNode, fputcProcNode, writefProcNode, fprintfProcNode,
    writesProcNode, fwriteProcNode, maxProcNode, minProcNode, namProcNode
$end
    ;

from Errors import
    Error;

from TypeInfo import
    NewTypeNode;

from TypeDefs import
    PointerType, ArrayType, MakeSubrange, TypeWithSize, AddToEnumList;

from Decls import
    DefineConst, DefineType, DefineVar, DefineProc,
    EndProc, AddToIdentList;

$if modula2 then
from Decls import DefineModule, EndModule, ExportDecl, MakeIdent;
$else
from Alloc import RoundUp;
$end

from Consts import
    CardinalConst;


procedure InitBuiltin();
var
    name	: String;
    tn		: TypeNode;
    mn		: ModuleNode;
    pn		: ProcNode;
    exports     : IdentList;
    cn		: ConstNode;
    vn		: VarNode;
    saveScope   : Scope;
    realConvert : record 
		    case boolean of
		    | true : realVal : longreal;
		    | false : uns1, uns2 : unsigned;
		    end;
		  end;

const
    MAXPARAMS   = 10000;

$if pascal then
var
    textTypeNode : TypeNode;
$end

    procedure BuiltinIntCardConst(const constName : array of char;
				  const kind      : DataType;
				  const value     : HugeInteger);
    begin
	name := NewText(constName);
$if modula2 then
	exports := AddToIdentList(exports,MakeIdent(name));
$end
	new(cn);
	cn^.kind := kind;
	cn^.cardVal := value;
	DefineConst(name, cn);
    end BuiltinIntCardConst;

$if modula2 then
    procedure BuiltinRealConst(const constName : array of char;
			       const val1,val2 : unsigned);
    begin
	name := NewText(constName);
	exports := AddToIdentList(exports,MakeIdent(name));
	new(cn);
	cn^.kind := DTREAL;
	realConvert.uns1 := val1;
	realConvert.uns2 := val2;
	cn^.realVal := realConvert.realVal;
	DefineConst(name, cn);
    end BuiltinRealConst;
$end

    procedure BuiltinProc(const procName    : array of char; 
			  const whichBIP    : BuiltinProcType;
			  const minParams   : integer;
			  const maxParams   : integer;
			  const isFunc      : boolean);
    begin
	(* name and pn left around for ALLOCATE/DEALLOCATE *)
	name := NewText(procName);
$if modula2 then
	exports := AddToIdentList(exports,MakeIdent(name));
$end
	pn := DefineProc(name,TKPROCEDURE);
	pn^.builtin := whichBIP;
	pn^.procType := procTypeNode;
	pn^.minParams := minParams;
	pn^.maxParams := maxParams;
	if isFunc then
	    incl(builtinFunctions, whichBIP);
	end;
	EndProc(pn,nil,nil);
    end BuiltinProc;

begin
    builtinFunctions := BuiltinProcTypeSet{};

$if modula2 then
    (* Enter system types *)

    (* DEFINITION MODULE SYSTEM *)
    mn := DefineModule(NewText('SYSTEM'),TKBEGIN);
    exports := AddToIdentList(nil,nil);
    mn^.noinit := true;

    name := NewText('WORD');
    exports := AddToIdentList(exports,MakeIdent(name));
    DefineType(name,wordTypeNode);

    name := NewText('BYTE');
    exports := AddToIdentList(exports,MakeIdent(name));
    DefineType(name,byteTypeNode);

    name := NewText('UNIT');
    exports := AddToIdentList(exports,MakeIdent(name));
    DefineType(name,byteTypeNode);

    name := NewText('ADDRESS');
    exports := AddToIdentList(exports,MakeIdent(name));
    DefineType(name,addressTypeNode);

    BuiltinProc('ADR',      BIPADR,	    1, 1, true);
    BuiltinProc('SIZE',     BIPBYTESIZE,    1, 1, true);
    BuiltinProc('TSIZE',    BIPTBYTESIZE,   1, MAXPARAMS, true);
    BuiltinProc('BITSIZE',  BIPBITSIZE,     1, 1, true);
    BuiltinProc('TBITSIZE', BIPTBITSIZE,    1, MAXPARAMS, true);
    BuiltinProc('BYTESIZE', BIPBYTESIZE,    1, 1, true);
    BuiltinProc('TBYTESIZE',BIPTBYTESIZE,   1, MAXPARAMS, true);
    BuiltinProc('WORDSIZE', BIPWORDSIZE,    1, 1, true);
    BuiltinProc('TWORDSIZE',BIPTWORDSIZE,   1, MAXPARAMS, true);
    BuiltinProc('CPUTIME',  BIPCPUTIME,     0, 0, true);
    BuiltinProc('XFC',	    BIPXFC,	    1, 1, true);    (* CED 8/13/87 *)

    (* If you trap on XFC, you need to know the instruction size so that *)
    (* you can increment the PC before continuing. Rich Kittell suggested *)
    (* the CONST name, I adopted it today. CED 8/15/87 *)
    BuiltinIntCardConst('XFC_INST_BYTES',DTCARDINAL,longfloat(1));

    BuiltinIntCardConst('BYTESPERWORD', DTCARDINAL, longfloat(BYTESPERWORD));
    BuiltinIntCardConst('BITSPERWORD',  DTCARDINAL, longfloat(WORDSIZE));
    BuiltinIntCardConst('BITSPERBYTE',  DTCARDINAL, longfloat(BYTESIZE));
    BuiltinIntCardConst('BITSPERUNIT',  DTCARDINAL, longfloat(UNITSIZE));
    BuiltinIntCardConst('MOSTSIGBIT',   DTCARDINAL, longfloat(WORDSIZE-1));
    BuiltinIntCardConst('LEASTSIGBIT',  DTCARDINAL, 0.0);
    BuiltinIntCardConst('MAXUNSIGNED',  DTCARDINAL, MAXCARD);
    BuiltinIntCardConst('MINUNSIGNED',  DTCARDINAL, 0.0);
    if standardCardinalFlag then
	BuiltinIntCardConst('MAXCARD',  DTCARDINAL, MAXCARD);
    else
	BuiltinIntCardConst('MAXCARD',  DTCARDINAL, MAXINT);
    end;
    BuiltinIntCardConst('MINCARD',      DTCARDINAL, 0.0);
    BuiltinIntCardConst('MAXINT',       DTCARDINAL,  MAXINT);
    BuiltinIntCardConst('MININT',       DTINTEGER,  MININT);

    (* VAX     64 bit floating is D_format,  32 bit floating is F_format.
       Titan   64 bit floating is G_format,  32 bit floating is F_format.
       uTitan  64 bit floating is G_format,  32 bit floating doesn't exist.
       Even when VAX and Titan use F_format, they're layed out differently.
       Cross-compiles from VAX to Titan and uTitan expected, but not from
       Titan to VAX.  All REAL and LONGREAL values maintained internally
       as LONGREALs. *)

    BuiltinRealConst('MINLONGREAL', 0ffffffffH, 0ffffffffH);
    minlongreal := realConvert.realVal;
    if host = TARGETVAX then		(* Map (badly) G_format onto D_format *)
	BuiltinRealConst('MAXLONGREAL', 0ffff7fffH, 0ffffffffH);
	maxlongreal := realConvert.realVal;
	BuiltinRealConst('MINPOSLONGREAL', 00000080H, 0);
    else				(* Use G_format *)
	BuiltinRealConst('MAXLONGREAL', 07fffffffH, 0ffffffffH);
	maxlongreal := realConvert.realVal;
	BuiltinRealConst('MINPOSLONGREAL', 00100000H, 0);
    end;

    if host = TARGETVAX then
	if target = TARGETTITANM then   (* Map (badly) G_format onto D_format *)
	    BuiltinRealConst('MINREAL', 0ffffffffH, 0ffffffffH);
	    minreal := realConvert.realVal;
	    BuiltinRealConst('MAXREAL', 0ffff7fffH, 0ffffffffH);
	    maxreal := realConvert.realVal;
	    BuiltinRealConst('MINPOSREAL', 00000080H, 0);
	else				(* Map F_format onto D_format *)
	    BuiltinRealConst('MINREAL', 0ffffffffH, 0);
	    minreal := realConvert.realVal;
	    BuiltinRealConst('MAXREAL', 0ffff7fffH, 0);
	    maxreal := realConvert.realVal;
	    BuiltinRealConst('MINPOSREAL', 00000080H, 0);
	end;
    else     
	if target = TARGETTITANM then (* No short reals, use G_format *)
	    BuiltinRealConst('MINREAL', 0ffffffffH, 0ffffffffH);
	    minreal := realConvert.realVal;
	    BuiltinRealConst('MAXREAL', 07fffffffH, 0ffffffffH);
	    maxreal := realConvert.realVal;
	    BuiltinRealConst('MINPOSREAL', 00100000H, 0);
	else			      (* Map F_format onto G_format *)
	    BuiltinRealConst('MINREAL', 0c7ffffffH, 0e0000000H);
	    minreal := realConvert.realVal;
	    BuiltinRealConst('MAXREAL', 047ffffffH, 0e0000000H);
	    maxreal := realConvert.realVal;
	    BuiltinRealConst('MINPOSREAL', 38100000H, 0);
	end;
    end;
	
    (* ||| Why COROUTINE substituted for PROCESS? *)
    name := NewText('COROUTINE');
    exports := AddToIdentList(exports,MakeIdent(name));
    DefineType(name,processTypeNode);

    name := NewText('PROCESS');
    exports := AddToIdentList(exports,MakeIdent(name));
    DefineType(name,processTypeNode);

    BuiltinProc('NEWPROCESS',   BIPNEWPROCESS,  4, 4, false); 
    BuiltinProc('NEWCOROUTINE', BIPNEWPROCESS,  4, 4, false);
    BuiltinProc('TRANSFER',     BIPTRANSFER,    2, 2, false);

    ExportDecl(exports,TKQUALIFIED);

    EndModule(mn,nil,nil);

    (* ||| These stay caps for now as the library implementation
	and loader are case-sensitive.  Later should be changed to
	reasonable case, not all upper *)
    mn := DefineModule(NewText('BITOPERATIONS'),TKBEGIN);
    exports := AddToIdentList(nil,nil);
    mn^.noinit := true;

    BuiltinProc('BITNOT',	    BIPBITNOT,		1, 1, true);
    BuiltinProc('BITAND',	    BIPBITAND,		2, 2, true);
    BuiltinProc('BITOR',	    BIPBITOR,		2, 2, true);
    BuiltinProc('BITXOR',	    BIPBITXOR,		2, 2, true);
    BuiltinProc('BITSHIFTLEFT',     BIPBITSHIFTLEFT,	2, 2, true);    
    BuiltinProc('BITSHIFTRIGHT',    BIPBITSHIFTRIGHT,	2, 2, true); 
    BuiltinProc('BITINSERT',	    BIPBITINSERT,	4, 4, true); 
    BuiltinProc('BITEXTRACT',       BIPBITEXTRACT,	3, 3, true); 

    ExportDecl(exports,TKQUALIFIED);

    EndModule(mn,nil,nil);

    (* get strings for allocate and deallocate *)
    (* as you define memory module *)

    mn := DefineModule(NewText('MEMORY'),TKBEGIN);
    exports := AddToIdentList(nil,nil);
    mn^.noinit := true;

    BuiltinProc('ALLOCATE',     BIPALLOCATE,    2, 2, false); 
    allocateString := name;
    allocateProc := pn;

    BuiltinProc('DEALLOCATE',   BIPDEALLOCATE,  2, 2, false); 
    deallocateString := name;
    deallocateProc := pn;

    ExportDecl(exports,TKQUALIFIED);

    EndModule(mn,nil,nil);


    (* DEFINITION MODULE IO *)
    mn := DefineModule(NewText('IO'),TKBEGIN);
    exports := AddToIdentList(nil,nil);
    mn^.noinit := true;

    name := NewText('FILE');
    exports := AddToIdentList(exports,MakeIdent(name));
    DefineType(name,fileTypeNode);

    BuiltinProc('WRITEF',   BIPWRITEF,  2, MAXPARAMS, false);
    BuiltinProc('READF',    BIPREADF,   2, MAXPARAMS, true);

    BuiltinProc('SWRITEF',  BIPSWRITEF, 2, MAXPARAMS, false);
    BuiltinProc('SREADF',   BIPSREADF,  2, MAXPARAMS, true);

    BuiltinProc('WRITEB',   BIPWRITEB,  3, 3, false);
    BuiltinProc('READB',    BIPREADB,   3, 3, true);

    BuiltinProc('WRITES',   BIPWRITES,  2, 2, false);
    BuiltinProc('READS',    BIPREADS,   2, 3, true);

    BuiltinProc('WRITEC',   BIPWRITEC,  2, 2, false);
    BuiltinProc('READC',    BIPREADC,   2, 2, true);

    BuiltinProc('OPEN',     BIPOPENF,   2, 2, true);
    BuiltinProc('CLOSE',    BIPCLOSEF,  1, 1, false);

    (* Note: DefineVar will convert MEMNORMAL to MEMGLOBAL *)
    name := NewText('INPUT');
    exports := AddToIdentList(exports,MakeIdent(name));
    vn := DefineVar(name,fileTypeNode,MEMNORMAL,GSNORMAL, nil);

    name := NewText('OUTPUT');
    exports := AddToIdentList(exports,MakeIdent(name));
    vn := DefineVar(name,fileTypeNode,MEMNORMAL,GSNORMAL, nil);

    name := NewText('TERMINAL');
    exports := AddToIdentList(exports,MakeIdent(name));
    vn := DefineVar(name,fileTypeNode,MEMNORMAL,GSNORMAL, nil);

    ExportDecl(exports,TKQUALIFIED);

    EndModule(mn,nil,nil);
$end (* if modula2 *)


    saveScope := currScope;
    currScope := builtinScope;
    exports := AddToIdentList(nil,nil);

$if modula2 then
    BuiltinProc('ABS',      BIPABS,	    1, 1, true);
    BuiltinProc('ASSERT',   BIPASSERT,      1, 2, false);
    BuiltinProc('CAP',      BIPCAP,	    1, 1, true);
    BuiltinProc('CHR',      BIPCHR,	    1, 1, true);
    BuiltinProc('DEC',      BIPDEC,	    1, 2, false);
    BuiltinProc('DESCRIPTOR',BIPDESCRIPTOR, 2, MAXPARAMS, true);
    BuiltinProc('DISPOSE',  BIPDISPOSE,     1, MAXPARAMS, false);
    BuiltinProc('EXCL',     BIPEXCL,	    2, 2, false);
    BuiltinProc('FIRST',    BIPFIRST,       1, 1, true);
    if target = TARGETTITANM then
	BuiltinProc('FLOAT',BIPLONGFLOAT,   1, 1, true);
    else
	BuiltinProc('FLOAT',BIPFLOAT,       1, 1, true);
    end;
    BuiltinProc('HALT',     BIPHALT,	    0, 1, false);
    BuiltinProc('HIGH',     BIPHIGH,	    1, 2, true);
    BuiltinProc('INC',      BIPINC,	    1, 2, false);
    BuiltinProc('INCL',     BIPINCL,	    2, 2, false);
    BuiltinProc('LAST',     BIPLAST,        1, 1, true);
    BuiltinProc('LOCAL',    BIPLOCAL,       1, MAXPARAMS, false);
    BuiltinProc('LONGFLOAT',BIPLONGFLOAT,   1, 1, true);
    BuiltinProc('LOW',      BIPLOW,	    1, 2, true);
    BuiltinProc('MAX',      BIPMAX,	    1, 2, true);
    BuiltinProc('MIN',      BIPMIN,	    1, 2, true);
    BuiltinProc('NEW',      BIPNEW,         1, MAXPARAMS, false);
    BuiltinProc('NUMBER',   BIPNUMBER,	    1, 2, true);
    BuiltinProc('ODD',      BIPODD,	    1, 1, true);
    BuiltinProc('ORD',      BIPORD,	    1, 1, true);
    BuiltinProc('TRUNC',    BIPTRUNC,	    1, 1, true);
    BuiltinProc('VAL',      BIPVAL,	    2, 2, true);

$else (* pascal *)

    BuiltinIntCardConst('maxint',       DTINTEGER,  MAXINT);

    name := NewText('alfa');
    DefineType(name, alfaTypeNode);

    textTypeNode := NewTypeNode(DTFILE);
    textTypeNode^.size := RoundUp(FILESIZE + CHARSIZE, WORDSIZE);
    textTypeNode^.containsFiles := true;
    textTypeNode^.fileType := charTypeNode;
    textTypeNode^.isTextFile := true;

    name := NewText('text');
    DefineType(name, textTypeNode);

    name := NewText('intset');
    DefineType(name, intsetTypeNode);

    inputString := NewText('input');
    inputVarNode := DefineVar(inputString,textTypeNode,MEMNORMAL,
	GSEXTERNAL, nil);
    inputDeclared := false; (* no access unless declared in program heading *)

    outputString := NewText('output');
    outputVarNode :=
        DefineVar(outputString,textTypeNode,MEMNORMAL,GSEXTERNAL, nil);
    outputDeclared := false;

    name := NewText('terminal');
    errorVarNode := DefineVar(name,textTypeNode,MEMNORMAL,GSEXTERNAL, nil);
    errorVarNode^.address.gvn^.globalName := NonHashText('_err');
    
    name := NonHashText('_argc');
    argcVarNode := DefineVar(name,integerTypeNode, MEMNORMAL, GSEXTERNAL, nil);

    name := NonHashText('_seed');
    seedVarNode := DefineVar(name,integerTypeNode, MEMNORMAL, GSEXTERNAL, nil);

(* fnil proc - never checked.  See CheckExpr:DerefExpr. *)
    BuiltinProc('$FNIL',    BIPfnil,	    1, 1, true);
    fnilProcNode := pn;

(* longfloat proc - never checked.  See CheckExpr:IntegerToReal. *)
    BuiltinProc('$longfloat', BIPlongfloat, 1, 1, true);
    longfloatProcNode := pn;

(* unit proc.  
   See CheckBuiltin:CheckWriteBinaryParams, CheckReadBinaryParams,
	and BIPput and BIPget cases *)
    BuiltinProc('$UNIT',    BIPunit,	    1, 1, true);
    unitProcNode := pn;

(* Calls for read from text file.  See CheckBuiltin:CheckReadParams *)
    BuiltinProc('$READ4',   BIPread4,       1, 1, true);
    read4ProcNode := pn;
    BuiltinProc('$READ8',   BIPread8,       1, 1, true);
    read8ProcNode := pn;
    BuiltinProc('$READE',   BIPreade,       2, 2, true);
    readeProcNode := pn;
    BuiltinProc('$READC',   BIPreadc,       1, 1, true);
    readcProcNode := pn;

(* Calls for write to text file.  See CheckBuiltin:CheckWriteParams *)
    BuiltinProc('$WRITEC',  BIPwritec,      3, 3, false);
    writecProcNode := pn;
    BuiltinProc('$fputc',   BIPfputc,       2, 2, false);
    fputcProcNode := pn;
    BuiltinProc('$WRITEF',  BIPwritef,      4, 6, false);
    writefProcNode := pn;
    BuiltinProc('$fprintf', BIPfprintf,     3, 5, false);
    fprintfProcNode := pn;
    BuiltinProc('$WRITES',  BIPwrites,      5, 5, false);
    writesProcNode := pn;
    BuiltinProc('$fwrite',  BIPfwrite,      4, 4, false);
    fwriteProcNode := pn;
    BuiltinProc('$NAM',     BIPnam,	    2, 2, false);
    namProcNode := pn;
    BuiltinProc('$MAX',     BIPmax,	    3, 3, false);
    maxProcNode := pn;
    BuiltinProc('$min',     BIPMIN,	    2, 2, true);
    minProcNode := pn;

(* Standard Pascal procedures *)
    BuiltinProc('put',      BIPput,	    1, 1, false);
    putProcNode := pn;
    BuiltinProc('get',      BIPget,	    1, 1, false);
    getProcNode := pn;
    BuiltinProc('reset',    BIPreset,       1, 2, false);
    BuiltinProc('rewrite',  BIPrewrite,     1, 2, false);

    BuiltinProc('new',      BIPNEW,	    1, MAXPARAMS, false);
    BuiltinProc('dispose',  BIPDISPOSE,     1, MAXPARAMS, false);

    BuiltinProc('pack',     BIPpack,	    3, 3, false);
    BuiltinProc('unpack',   BIPunpack,      3, 3, false);
    
    BuiltinProc('read',     BIPread,	    1, MAXPARAMS, false);
    BuiltinProc('readln',   BIPreadln,      0, MAXPARAMS, false);
    BuiltinProc('write',    BIPwrite,       1, MAXPARAMS, false);
    BuiltinProc('writeln',  BIPwriteln,     0, MAXPARAMS, false);
    writelnProcNode := pn;
    BuiltinProc('page',     BIPpage,	    1, 1, false);

(* Standard Pascal functions *)
    BuiltinProc('abs',      BIPABS,	    1, 1, true);
    BuiltinProc('sqr',      BIPsqr,	    1, 1, true);
    BuiltinProc('sin',	    BIPsin,	    1, 1, true);
    BuiltinProc('cos',	    BIPcos,	    1, 1, true);
    BuiltinProc('exp',	    BIPexp,	    1, 1, true);
    BuiltinProc('ln',	    BIPln,	    1, 1, true);
    BuiltinProc('sqrt',	    BIPsqrt,	    1, 1, true);
    BuiltinProc('arctan',   BIParctan,	    1, 1, true);
    
    BuiltinProc('odd',      BIPODD,	    1, 1, true);
    BuiltinProc('eof',      BIPeof,	    0, 1, true);
    BuiltinProc('eoln',     BIPeoln,	    0, 1, true);
    
    BuiltinProc('trunc',    BIPTRUNC,       1, 1, true);
    BuiltinProc('round',    BIPround,       1, 1, true);
    BuiltinProc('ord',      BIPORD,	    1, 1, true);
    BuiltinProc('chr',      BIPCHR,	    1, 1, true);

    BuiltinProc('succ',	    BIPsucc,	    1, 1, true);
    BuiltinProc('pred',	    BIPpred,	    1, 1, true);

(* Non-standard Pascal procedures *)
    BuiltinProc('argv',     BIPargv,	    2, 2, false);
    BuiltinProc('assert',   BIPASSERT,      1, 2, false);
    BuiltinProc('date',     BIPdate,	    1, 1, false);
    BuiltinProc('flush',    BIPflush,       0, 1, false);
    BuiltinProc('halt',     BIPhalt,	    0, 0, false);
    BuiltinProc('linelimit',BIPlinelimit,   2, 2, false);
    BuiltinProc('message',  BIPmessage,     0, MAXPARAMS, false);
    BuiltinProc('null',     BIPnull,	    0, 0, false);
    BuiltinProc('remove',   BIPremove,      1, 1, false);
    BuiltinProc('stlimit',  BIPstlimit,     1, 1, false);
    BuiltinProc('time',     BIPtime,	    1, 1, false);

(* Non-standard Pascal functions *)
    BuiltinProc('argc',     BIPargc,	    0, 0, true);
    BuiltinProc('card',     BIPcard,	    1, 1, true);
    BuiltinProc('clock',    BIPclock,       0, 0, true);
    BuiltinProc('expo',     BIPexpo,	    1, 1, true);
    BuiltinProc('random',   BIPrandom,      1, 1, true);
    BuiltinProc('seed',     BIPseed,	    1, 1, true);
    BuiltinProc('sysclock', BIPsysclock,    0, 0, true);
    BuiltinProc('undefined',BIPundefined,   1, 1, true);
    BuiltinProc('wallclock',BIPwallclock,   0, 0, true);

$end 
    
    currScope := saveScope;

    if generateBlockNumber > MAXBUILTINSCOPES then
	Error('Compiler error: too many builtin scopes');
	ExitProgram(101);
    end;
    generateBlockNumber := MAXBUILTINSCOPES+1;

    pureFunctions := 
	BuiltinProcTypeSet{BIPABS, BIPCHR, BIPODD, BIPORD, BIPTRUNC
$if modula2 then
	, BIPCAP, BIPFLOAT, BIPHIGH, BIPVAL, BIPLONGFLOAT, BIPMIN, BIPMAX, 
	BIPLOW, BIPNUMBER, BIPDESCRIPTOR, BIPFIRST, BIPLAST, BIPADR,
	BIPBITSIZE, BIPTBITSIZE, 
	BIPBYTESIZE, BIPTBYTESIZE, BIPWORDSIZE, BIPTWORDSIZE,
	BIPBITAND, BIPBITOR, BIPBITNOT, BIPBITXOR, BIPBITSHIFTLEFT,
	BIPBITSHIFTRIGHT, BIPBITINSERT, BIPBITEXTRACT
$else (* pascal *)
	,BIPsqr, BIPsin, BIPcos, BIPexp, BIPsqrt, BIParctan, BIPround,
	BIPsucc, BIPpred, BIPargc, BIPcard, BIPexpo, BIPundefined
$end
	    };

$if modula2 then
    constFunctions := BuiltinProcTypeSet{BIPABS, BIPCAP, BIPCHR, BIPFLOAT, 
	BIPODD, BIPORD, BIPTRUNC, BIPVAL, BIPLONGFLOAT, BIPMIN, BIPMAX, 
	BIPFIRST, BIPLAST, BIPTBITSIZE, BIPTBYTESIZE, BIPTWORDSIZE, BIPBITSIZE, 
	BIPBYTESIZE, BIPWORDSIZE, BIPHIGH, BIPLOW, BIPNUMBER};
$end
end InitBuiltin;

procedure InitStandardTypes;
$if modula2 then
const
    INTEGERNAME = 'INTEGER';
    CHARNAME    = 'CHAR';
    LONGREALNAME= 'LONGREAL';
    BOOLEANNAME = 'BOOLEAN';
    FALSENAME   = 'FALSE';
    TRUENAME    = 'TRUE';
    NILNAME     = 'NIL';
$else (* pascal *)
const
    INTEGERNAME = 'integer';
    CHARNAME    = 'char';
    LONGREALNAME= 'real';
    BOOLEANNAME = 'boolean';
    FALSENAME   = 'false';
    TRUENAME    = 'true';
    NILNAME     = 'nil';
$end
var
    sym     : Symbol;
    cn      : ConstNode;
    en      : EnumNode;
    name    : String;
    error   : boolean;
    opaqueTypeNode : TypeNode;
begin
    constBYTESIZE := CardinalConst(longfloat(BYTESIZE));
    error := false;
    integerTypeNode := NewTypeNode(DTINTEGER);
    integerTypeNode^.size := WORDSIZE;
    error := error or not DefineSymbol(sym,NewText(INTEGERNAME),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := integerTypeNode;

    cardinalTypeNode := NewTypeNode(DTCARDINAL);
    cardinalTypeNode^.size := WORDSIZE;

    cardIntTypeNode := NewTypeNode(DTINTEGER);
    cardIntTypeNode^.size := WORDSIZE;

$if modula2 then
    error := error or not DefineSymbol(sym,NewText('CARDINAL'),builtinScope,
					ONECASE,SYMTYPE);
    if standardCardinalFlag then
	sym^.symType := cardinalTypeNode;
    else
	sym^.symType := cardIntTypeNode;
    end;

    error := error or not DefineSymbol(sym,NewText('UNSIGNED'),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := cardinalTypeNode;
$end

    charTypeNode := NewTypeNode(DTCHAR);
    case target of
    | TARGETVAX :
	charTypeNode^.size := CHARSIZE;
	packedCharTypeNode := charTypeNode;
    | TARGETTITAN, TARGETTITANM :
	charTypeNode^.size := WORDSIZE;
	packedCharTypeNode := TypeWithSize(charTypeNode,constBYTESIZE);
    end;
    error := error or not DefineSymbol(sym,NewText(CHARNAME),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := charTypeNode;

    charConstTypeNode := NewTypeNode(DTCHAR);
    case target of
    | TARGETVAX :
	charConstTypeNode^.size := CHARSIZE;
	packedCharConstTypeNode := charConstTypeNode;
    | TARGETTITAN, TARGETTITANM :
	charConstTypeNode^.size := WORDSIZE;
	packedCharConstTypeNode := 
		TypeWithSize(charConstTypeNode,constBYTESIZE);
    end;
 
    longrealTypeNode := NewTypeNode(DTLONGREAL);
    longrealTypeNode^.size := LONGREALSIZE;
    if target = TARGETTITANM then
	longrealTypeNode^.alignment := LONGREALSIZE;
    else 
	longrealTypeNode^.alignment := WORDSIZE;
    end;
    error := error or not DefineSymbol(sym,NewText(LONGREALNAME),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := longrealTypeNode;

    if target = TARGETTITANM then
	realTypeNode := longrealTypeNode;
    else
	realTypeNode := NewTypeNode(DTREAL);
	realTypeNode^.size := WORDSIZE;
    end;
$if modula2 then
    error := error or not DefineSymbol(sym,NewText('REAL'),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := realTypeNode;
$end

    realConstTypeNode := NewTypeNode(DTLONGREAL);
    realConstTypeNode^.size := LONGREALSIZE;

    stringTypeNode := NewTypeNode(DTSTRING);
    stringTypeNode^.stringLength := 0;
    stringTypeNode^.size := 0;

    ioStringTypeNode := NewTypeNode(DTSTRING);
    ioStringTypeNode^.stringLength := 100;
    ioStringTypeNode^.size := 100*CHARSIZE;

    booleanTypeNode := NewTypeNode(DTBOOLEAN);
    case target of
    | TARGETVAX :
	booleanTypeNode^.size := BOOLEANSIZE;
	packedBooleanTypeNode := booleanTypeNode;
    | TARGETTITAN, TARGETTITANM :
	booleanTypeNode^.size := WORDSIZE;
	packedBooleanTypeNode := TypeWithSize(booleanTypeNode,constBYTESIZE);
    end;

    (* Fill in info to allow boolean to be written.  Note that the EnumNode
       names are not entered into the scope at all...direct constants
       are used for that. *)
    booleanTypeNode^.enumMin := 0;
    booleanTypeNode^.enumMax := 1;
    booleanTypeNode^.enumContiguous := true;
    booleanTypeNode^.nameTable := nil;
    booleanTypeNode^.name := NewText(BOOLEANNAME);
    error := error or not DefineSymbol(sym,NewText(BOOLEANNAME),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := booleanTypeNode;

    new(cn);
    cn^.kind := DTBOOLEAN;
    cn^.boolVal := false;
    name := NewText(FALSENAME);
    error := error or not DefineSymbol(sym,name,builtinScope,
					ONECASE,SYMCONST);
    sym^.symConst := cn;

    new(en);
    en^.name := name;
    en^.enumSym := nil;
    en^.enumType := booleanTypeNode;
    en^.enumOrd := 0;
    booleanTypeNode^.enumList := AddToEnumList(nil, en);


    new(cn);
    cn^.kind := DTBOOLEAN;
    cn^.boolVal := true;
    name := NewText(TRUENAME);
    error := error or not DefineSymbol(sym,name,builtinScope,
					ONECASE,SYMCONST);
    sym^.symConst := cn;

    new(en);
    en^.name := name;
    en^.enumSym := nil;
    en^.enumType := booleanTypeNode;
    en^.enumOrd := 1;
    booleanTypeNode^.enumList := AddToEnumList(booleanTypeNode^.enumList, en);

    procTypeNode := NewTypeNode(DTPROC);
    procTypeNode^.size := WORDSIZE;
    procTypeNode^.paramList := nil;
    procTypeNode^.funcType := nil;
$if modula2 then
    error := error or not DefineSymbol(sym,NewText('PROC'),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := procTypeNode;
$end

    new(cn);
    cn^.kind := DTPOINTER;
    error := error or not DefineSymbol(sym,NewText(NILNAME),builtinScope,
					ONECASE,SYMCONST);
    sym^.symConst := cn;

    wordTypeNode := NewTypeNode(DTWORD);
    wordTypeNode^.size := WORDSIZE;

    byteTypeNode := NewTypeNode(DTBYTE);
    case target of
    | TARGETVAX :
	byteTypeNode^.size := CHARSIZE;
	packedByteTypeNode := byteTypeNode;
    | TARGETTITAN, TARGETTITANM :
	byteTypeNode^.size := WORDSIZE;
	packedByteTypeNode := TypeWithSize(byteTypeNode,constBYTESIZE);
    end;

    addressTypeNode := PointerType(wordTypeNode,TKATNOCHECK);

    anyTypeNode := NewTypeNode(DTANY);
    anyTypeNode^.size := WORDSIZE;

    (* fileTypeNode is kludge to allow comparison with nil.  It also
       allows NEWing, which is bad. *)
    opaqueTypeNode := NewTypeNode(DTOPAQUE);
    opaqueTypeNode^.opaqueName := NonHashText('_opaque_');
    opaqueTypeNode^.size := WORDSIZE;
    fileTypeNode := PointerType(opaqueTypeNode,TKATNOCHECK);

    processTypeNode := NewTypeNode(DTOPAQUE);
    processTypeNode^.size := WORDSIZE;
    processTypeNode^.opaqueName := NonHashText('SYSTEM_PROCESS');

    arrayOfCharTypeNode := ArrayType(nil, charTypeNode, TKARRAY, TKNULL);

    intsetTypeNode := NewTypeNode(DTSET);
$if modula2 then
    intsetTypeNode^.size := 1;
    intsetTypeNode^.setRange := MakeSubrange(MININT, MAXINT, integerTypeNode);
$else (* pascal *)
    intsetTypeNode^.size := 128;
    intsetTypeNode^.setRange := MakeSubrange(0.0, 127.0, integerTypeNode);
$end

    unknownSetTypeNode := NewTypeNode(DTSET);
    unknownSetTypeNode^.size := 1;
    unknownSetTypeNode^.setRange := MakeSubrange(0.0, MAXCARD, anyTypeNode);

$if pascal then
    jmpBufTypeNode := ArrayType(MakeSubrange(0.0, 19.0, integerTypeNode),
				wordTypeNode, TKARRAY, TKNULL);
    alfaTypeNode := ArrayType(MakeSubrange(1.0, 10.0, integerTypeNode),
				charTypeNode, TKARRAY, TKNULL);

$end

    
    indexableTypes := DataTypeSet{DTINTEGER, DTCARDINAL, DTCHAR, DTBOOLEAN, 
				DTENUMERATION, DTSUBRANGE,
				DTANY (* to minimize error propagation *)};

    (* Default packing order to use for sets and records *)
    packOrder := PACKRIGHTTOLEFT;


    bitsetTypeNode := NewTypeNode(DTSET);
    bitsetTypeNode^.size := WORDSIZE;
    bitsetTypeNode^.setRange := 
	MakeSubrange(0.0, longfloat(WORDSIZE-1),cardIntTypeNode);
    bitsetTypeNode^.bitOrder := packOrder;
$if modula2 then
    error := error or not DefineSymbol(sym,NewText('BITSET'),builtinScope,
					ONECASE,SYMTYPE);
    sym^.symType := bitsetTypeNode;
$end

    if error then
	Error('Compiler error:  Cannot initialize builtin types?');
	ExitProgram(101);
    end;
end InitStandardTypes;


end InitBuiltin.
