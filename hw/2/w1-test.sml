use "w1.sml";

datatype ('a, 'b) result = OK of 'a | ERROR of 'b;

fun test arg expected =
    let val res = atoi arg in
    if expected <> res
    then ERROR ("(atoi \"" ^ arg  ^ "\"; expected: " ^ Int.toString expected ^ "; got: " ^ Int.toString res)
    else OK ()
    end;

test "700" 700;
test "19500" 19500;
test "00500" 500;
test "" 0;
