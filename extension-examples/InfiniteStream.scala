object InfiniteStream{
  def countFrom(start: Int): L.List = {
    L.Cons(start, countFrom(start + 1))
  }

  Std.printString(L.toString(L.take(countFrom(0), 5)))
}