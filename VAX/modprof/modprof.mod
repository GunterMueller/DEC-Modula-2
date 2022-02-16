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

module modprof;
import unix;
from io import readf, writef, open, File, output, terminal, readc;
from system import TByteSize;
from parameters import GetParameter, NumParameters;
const
    COUNTFILE = "modmon.out";
    MAXLINESIZE = 1024;
    MAXFILENAME = 256;
    LARGENUMBER = 1000000;
var
    countFile : integer;
    sourceFile : File;
    profFileName, sourceFileName : array [0..MAXFILENAME] of char;
    line : array [1..MAXLINESIZE] of char;
    i, length, lineNumber, nameSize, numCounts : integer;
    lineArray, countArray : dynarray of integer;
    ignoreChar : char;
begin
    if NumParameters > 2 then
	writef(terminal,"usage: modprof [profile]\n");
	unix.uexit(1);
    end;
    GetParameter(1,profFileName,length);
    if length < 0 then
	profFileName := COUNTFILE;
    end;
    countFile := unix.open(profFileName,unix.OPENREADONLY,0);
    if countFile < 0 then
	writef(terminal,"Cannot open %s\n",profFileName);
	unix.uexit(1);
    end;
    loop
	length := unix.read(countFile,nameSize,TByteSize(integer));
	if length # TByteSize(integer) then
	    writef(terminal,"Unexpected end of file on %s\n",profFileName);
	    unix.uexit(1);
	end;
	if (nameSize < 0) or (nameSize > MAXFILENAME) then
	    writef(terminal,"Invalid profile data: %s\n",profFileName);
	    unix.uexit(1);
	end;
	if nameSize = 0 then
	    exit;
	end;
	length := unix.read(countFile,numCounts,TByteSize(integer));
	if length # TByteSize(integer) then
	    writef(terminal,"Unexpected end of file on %s\n",profFileName);
	    unix.uexit(1);
	end;
	if (numCounts < 0) or (numCounts > LARGENUMBER) then
	    writef(terminal,"Invalid profile data: %s\n",profFileName);
	    unix.uexit(1);
	end;
	length := unix.read(countFile,sourceFileName,nameSize);
	if length # nameSize then
	    writef(terminal,"Unexpected end of file on %s\n",profFileName);
	    unix.uexit(1);
	end;
	sourceFile := open(sourceFileName,"r");
	if sourceFile = nil then
	    writef(terminal,"Cannot open %s\n",sourceFileName);
	    unix.uexit(1);
	end;
	new(countArray,numCounts+1);
	length := unix.read(countFile,countArray^,numCounts*TByteSize(integer));
	if length # numCounts*TByteSize(integer) then
	    writef(terminal,"Unexpected end of file on %s\n",profFileName);
	    unix.uexit(1);
	end;
	new(lineArray,numCounts+1);
	length := unix.read(countFile,lineArray^,numCounts*TByteSize(integer));
	if length # numCounts*TByteSize(integer) then
	    writef(terminal,"Unexpected end of file on %s\n",profFileName);
	    unix.uexit(1);
	end;
	lineArray^[numCounts] := LARGENUMBER;
	countArray^[numCounts] := -1;
	writef(output,"Statement counts for file %s\n",sourceFileName);
	(*
	for i := 0 to numCounts-1 do
	    writef(output,"line %5d  count %6d\n",lineArray^[i],countArray^[i]);
	end;
	*)
	i := 0;
	lineNumber := 0;
	loop
	    length := readf(sourceFile,"%[^\n]",line);
	    if length < 0 then
		exit;
	    elsif length = 0 then
		line[1] := 0C;
	    end;
	    length := readc(sourceFile,ignoreChar);
	    lineNumber := lineNumber + 1;
	    if lineNumber = lineArray^[i] then
		writef(output,"%7d ",countArray^[i]);
		i := i + 1;
	    elsif lineNumber < lineArray^[i] then
		writef(output,"\t");
	    else
		writef(output,"Line number out of order: %d %d\n",i,lineNumber);
	    end;
	    writef(output,"%s\n",line);
	    while lineNumber = lineArray^[i] do
		writef(output,">> %7d\n",countArray^[i]);
		i := i + 1;
	    end;
	end;
    end;
end modprof.
