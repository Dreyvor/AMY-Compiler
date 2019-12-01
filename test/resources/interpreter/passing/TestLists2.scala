object TestLists2 {

  abstract class List

  case class Nil() extends List

  case class Cons(h: Int, t: List) extends List

  def concat(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    }
  }
}
