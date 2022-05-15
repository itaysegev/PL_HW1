use "w2_def.sml";

structure ListIterable : Iterable = struct end;

structure SeqIterable : Iterable = struct end;

functor MakeModIterator (Iter: Iterable) : Iterator = struct end;

fun sumAll () = 0;