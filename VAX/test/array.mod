module testarray;
from system import MAXINT;
type
    CString = array @nocount of char;
    Rec = record
	sr : CString;
	tr : array of char;
    end;
    Ptr = pointer to record
	a : array [0..MAXINT+20] of integer;
    end;
var
    s : CString;
    t : array of char;
    p : Ptr;

procedure P(a : CString; b : array of char);
begin
end P;

end testarray.
