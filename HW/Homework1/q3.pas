program NecessaryComputations;
type Command = char;
type Arguments = record
    case cmd : Command of
        'P' : (a, b, c : integer);
        'C' : (ch : char; d : integer);
        'G' : (avg, test : integer);
    end;

type Result = record
    case cmd : Command of
        'P' : (resP : boolean);
        'C' : (resC : char);
        'G' : (resG : integer);
    end;
    
function isPythagorian(args : Arguments): Result;
var res : Result;
begin
    res.cmd := 'P';
    res.resP := (args.a * args.a + args.b * args.b = args.c * args.c);
    isPythagorian := res;
end;

function caesarCode(args : Arguments): Result;
var res : Result;
begin
    res.cmd := 'C';
    res.resC := chr(ord(args.ch) + args.d);
    caesarCode := res;
end;

function getFinalGrade(args : Arguments): Result;
var tmp_grade : real;
var res : Result;
begin
    res.cmd := 'G';
    if args.test < 55 then
        res.resG := args.test
    else
        begin
            tmp_grade := 0.2 * args.avg + 0.8 * args.test;
            res.resG := Round(tmp_grade);
        end;
    getFinalGrade := res;
end;

procedure getCommandInput(cmd : Command; var args : Arguments);
begin
    args.cmd := cmd;
    case args.cmd of
        'P' : ReadLn(args.a, args.b, args.c);
        'C' : ReadLn(args.ch, args.d);
        'G' : ReadLn(args.avg, args.test);
    end;
end;

var commands : array [1 .. 5] of char;
var all_args : array [low(commands) .. high(commands)] of Arguments;
var all_results : array [low(all_args) .. high(all_args)] of Result;
var i : integer;
begin
    for i := low(commands) to high(commands) do
        ReadLn(commands[i]);
    
    for i := low(commands) to high(commands) do
        getCommandInput(commands[i], all_args[i]);

    for i := low(all_args) to high(all_args) do
        case all_args[i].cmd of
            'P' : all_results[i] := isPythagorian(all_args[i]);
            'C' : all_results[i] := caesarCode(all_args[i]);
            'G' : all_results[i] := getFinalGrade(all_args[i]);
        end;

    for i := high(all_results) downto low(all_results) do
        case all_results[i].cmd of
            'P' : WriteLn(all_results[i].resP);
            'C' : WriteLn(all_results[i].resC);
            'G' : WriteLn(all_results[i].resG);
        end;
end.

