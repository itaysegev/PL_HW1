use "w2.sml";

exception AssertError;
fun assert b = if b then () else raise AssertError;

open W2;

assert (valAt [1,2,3,4] 3 = 4);
assert (valAt [1,2,3,4] 1 = 2);

assert (removeDupes [1, 2, 3, 1, 1, 5, 17, 3, 7, 1, 7] = [1,2,3,5,17,7]);
assert (removeDupes [1,2,9,8,1,4,1,12,6,7,8,8,16,11,9,12] = [1,2,9,8,4,12,6,7,16,11]);

assert (histogram [1, 2, 1, 1, 4, 19] = [(1,3),(2,1),(4,1),(19,1)]);
assert (histogram [12, 13, 11, 1, 11, 3, 5, 13, 15, 3, 11, 6] = [(12,1),(13,2),(11,3),(1,1),(3,2),(5,1),(15,1),(6,1)]);