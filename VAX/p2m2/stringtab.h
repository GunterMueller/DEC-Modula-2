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

{ string table stuff }
const
    STRINGBLOCKSIZE = 10000;
    MAXSTRINGSIZE = 1000;
    STRINGHASHSIZE = 1357;
    SHORTSTRINGSIZE = 40;
    FILENAMESIZE = 100;

type
    StringIndex = 0..STRINGBLOCKSIZE;
    StringLength = 0..MAXSTRINGSIZE;
    StringBlock = ^ StringBlockRec;
    StringBlockRec = record;
	next : StringBlock;
	s : array [StringIndex] of char;
    end;
    StringEntry = ^ StringEntryRec;
    StringEntryRec = record
	block : StringBlock;
	index : StringIndex;
	length : StringLength;
	hash : integer;			{ hash value for quick comparisons }
	next : StringEntry;
    end;
    String = StringEntry;
    ShortString = array [1..SHORTSTRINGSIZE] of char;
    FileName = array [0..FILENAMESIZE] of char;

procedure InitStringTable; external;

function NewString : String; external;

procedure AddChar(c : char); external;

function GetChar(s : String; charNum : StringLength) : char; external;

procedure AddText(s : ShortString); external;

procedure AddString(str : String); external;

procedure WriteString(var f : text; s : String); external;

procedure WriteStringConst(var f : text; s : String); external;

procedure OutString(s : String); external;

procedure OutStringConst(s : String); external;

procedure StringToFileName(s : String; var fn : FileName); external;

procedure DumpStringTab; external;

function EqualAnyCase(a,b : String) : boolean; external;
