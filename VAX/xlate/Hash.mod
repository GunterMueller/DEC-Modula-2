IMPLEMENTATION MODULE Hash;

VAR
    letter : CHAR;
BEGIN
    FOR letter := FIRST(CHAR) TO LAST(CHAR) DO
        codeForLetter[letter] := 1000;
    END;
    codeForLetter['2'] := 11;
    codeForLetter['a'] := 65;
    codeForLetter['b'] := 45;
    codeForLetter['c'] :=  2;
    codeForLetter['d'] :=  3;
    codeForLetter['e'] := 68;
    codeForLetter['f'] :=  3;
    codeForLetter['g'] := 37;
    codeForLetter['i'] :=  0;
    codeForLetter['k'] := 17;
    codeForLetter['l'] := 32;
    codeForLetter['m'] := 38;
    codeForLetter['n'] := 55;
    codeForLetter['o'] := 63;
    codeForLetter['p'] := 18;
    codeForLetter['q'] := 34;
    codeForLetter['r'] := 34;
    codeForLetter['s'] :=  0;
    codeForLetter['t'] :=  4;
    codeForLetter['u'] :=  6;
    codeForLetter['v'] :=  0;
    codeForLetter['x'] :=  0;
    codeForLetter['z'] :=  0;
END Hash.
