object print_head_with_match {
  val l:L.List = L.Cons(1, L.Cons(2, L.Nil()));
  Std.printString("######################################");
  //print the head
  l match{ //We shouldn't evaluate l here
    case L.Nil() => Std.printString("nil")
    case L.Cons(x, _) => Std.printInt(x)
    case _ => Std.printString("ERROR: matching strange behavior")
  };

  Std.printString("######################################");
  //print the whole list
  Std.printString(L.toString(l))
}
