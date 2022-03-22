program Ex1;
type Letter = 'a' .. 'z';
var c : char;
    letters, common : set of Letter;
 
procedure Init();
  var i : char;
begin
  letters := [];
  for i := 'a' to 'z' do letters := letters + [i];
  common := letters
end;
 
function CountUnique() : integer;
  {...}
  function GetSize() : integer;
    {...}
  begin
    {...}
  end;
begin
  {...}
end;
 
procedure PrintCommon();
  {...}
begin
  {...}
end;
 
begin
  Init();
  repeat
    Read(c);
    WriteLn(CountUnique());
  until c = '.';
  PrintCommon();
end.