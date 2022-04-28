type matrix = int list list;

signature S1 = sig
  val f_squared : matrix -> int
  val renumerate : 'a list -> (int * 'a) list
end;