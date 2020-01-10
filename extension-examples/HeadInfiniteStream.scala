object HeadInfiniteStream{
  def countFrom(start: Int): L.List = {
    L.Cons(start, countFrom(start + 1))
  }

  val l:L.List = countFrom(0);
  //val l:L.List = L.Cons(1, L.Cons(2, L.Nil()));

  //Std.printString(L.toString(L.take(countFrom(0), 1)))
  Std.printInt(L.head(l))
}