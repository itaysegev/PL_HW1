program PascalTriangle;
uses Sysutils;
type 
    ln = array [1 .. 100] of integer;
var debug: boolean;
{
    Here you may write helper functions and procedures.
    Please do.
}
procedure printLine(const line: ln);
var j: integer;
var str: string;
begin
    j := low(line);
    str := '';
    repeat
        str += IntToStr(line[j]);
        str += ' ';
        j += 1;
    until (not debug and ((j = high(line)) or (line[j] = 0))) or (j = high(line));
    str := copy(str, low(str), length(str) - 1);
    WriteLn(str);
end;

procedure nextLine(var line: ln);
var j: integer;
var prev_line: ln;
begin
    prev_line := line;
    for j := (low(line) + 1) to high(line) do
    begin
        line[j] := prev_line[j - 1] + prev_line[j]
    end;
end;

var n: integer;
var line: ln;
var i: integer;

begin
    debug := false;
    line[low(line)] := 1;
    ReadLn(n);
    for i := 1 to n do
    begin
        printLine(line);
        nextLine(line);
    end;
end.
