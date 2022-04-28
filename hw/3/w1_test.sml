use "w1.sml";

exception AssertError;
fun assert b = if b then () else raise AssertError;

open W1;

assert (f_squared [[1, 2, 3], [3, 4, 5], [6, 7, 8]] = 213);
assert (f_squared [[5]] = 25);

assert (renumerate (explode "Hello world!") = [(12,#"H"),(11,#"e"),(10,#"l"),(9,#"l"),(8,#"o"),(7,#" "),(6,#"w"),(5,#"o"),(4,#"r"),(3,#"l"),(2,#"d"),(1,#"!")]);
assert (renumerate ["a", "B", "1", "2"] = [(4,"a"),(3,"B"),(2,"1"),(1,"2")]);