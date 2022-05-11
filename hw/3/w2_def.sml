signature S2 = sig
  val valAt : 'a list -> int -> 'a
  val removeDupes : ''a list -> ''a list
  val histogram : int list -> (int * int) list
end;