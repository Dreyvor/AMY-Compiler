object TestLists2 {

  /*abstract class List

  case class Nil() extends List

  case class Cons(h: Int, t: List) extends List

  def concat(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    }
  }*/


  val l: L.List = L.Cons(5, L.Nil());
  //Std.printString(L.toString(L.concat(L.Cons(1, L.Cons(2, L.Nil())), L.Cons(3, L.Nil()))))
  //Std.printString(L.toString(l))
  Std.printInt(L.sum(l))
  //Std.printString(L.toString(L.mergeSort(l)))


}
