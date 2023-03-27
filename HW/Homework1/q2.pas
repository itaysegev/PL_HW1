program PascalCyberAttack;

type
   ch_array = array[char] of 1..26;
var
   new_hist: ch_array;
   old_hist: ch_array;
   both_hist: ch_array;
   c: char;
   new_word, old_word: string;
   llegal: boolean;

function SetHist(str: string): ch_array;
var c:char;
begin
  for c := 'a' to 'z' do
    SetHist[c]:= 0;
  for c in str do
    SetHist[c]:= SetHist[c] + 1; 
end;

begin
ReadLn(old_word);
ReadLn(new_word);
llegal := true;


new_hist := SetHist(new_word);
old_hist := SetHist(old_word);
for c := 'a' to 'z' do
begin
    both_hist[c] := new_hist[c] + old_hist[c];
    if(new_hist[c] > 0 ) and (old_hist[c] > 0) then 
        llegal := false;
end;
writeLn(llegal);
for c := 'a' to 'z' do
begin
    if(both_hist[c] = 1) or (both_hist[c] >= 2) then
        writeLn(c + ' ', both_hist[c]);
end;
end.
