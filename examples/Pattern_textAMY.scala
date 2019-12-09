object P {

    abstract class List
    case class Nil() extends List
    case class Cons(h: Int, t: List) extends List

    abstract class  G
    case class GG(i : Int, j : Int) extends G
    case class FF(h: Boolean) extends G
    case class VV() extends G

    def t() : Int = {
        7 match { case _ => 7} match { case _ => 42};
        7 + 1 match { case _ => 8};
        if(true){ 8 }else{ 7} match { case _ => 7} match { case _=> 0 };
        GG(4, 4545; 8; 7) match {
            case VV() => 4
            case FF(true) => 45; 78; 1
            case GG(_, fe) => fe match { case _ => if(true){45}else {45}}; 4
        }
    }

    case class HH() extends G

    Std.printInt(t())
}
