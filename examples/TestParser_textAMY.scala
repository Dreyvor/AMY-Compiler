object TestParser {

    
    abstract class Farine
    case class Ble() extends Farine
    case class Autres() extends Farine
    case class Truc() extends Farine

    def r(): Int = {

     1+  ( 7; 7 ; 15; 7 match {
            case 7 => 7 match {
                case 7 => 7
                case _ => 0
            }
            case 4 => 4
            case _ => 0
        } ) + 3 ;

      //  (error(4;"RRRR"); 9) + 8 ;

        (1; 5; true; 4) + (7 ; 5 ; 7) ;

        7 + -7;

        val f : Int =( 7 match {
            case _ => 42
        } match {
            case _ => 0
        }; true match {
            case true => 7
            case false => 9 
        }); 8;


        1 match { case _ => 2 }; 1;

        7 match { case dfg => dfg + 4
        case L.Nil() => 4 } ;

        L.Nil() match { 
            case L.Nil() => 0
            case _ => 77
        } match {
            case _ => 45
        } match {
            case r => r + 4
        } match {
            case 5 => 544
            case _ => 2
        } match {
            case _ => 1
        }
    
    }
    
   def rr(x : Int): Int ={
       val ffff : Int = 7; ffff; 4 match {
           case _ => 7
       };
    (val y : Int = 5;
       10; 45; 9; 77; val u : Int = 7; 8) + (8; 78; r(); 7; 1);

       -7
    }
    def rrr(x : Int, y : Int): Int ={ y + x }
    // (exp; val x : TYPE = value) + (...) invalide
    // (exp; def ... ; exp) invalide
    // ;;  invalide

    val e : Boolean =  true;

    val kjhgf : Int = if(true){ 7 } else { 8 } match { case _ => 78 };

    val y : Int = ( 78; rr(7); e match {
        case false => 8
        case true => 9
        case _ => 0
    }; 8);


    Std.printInt(y);
    Std.printInt(rr(val g : Int = -7 ; 5; 4; 8));
    Std.printInt(-10+-4-7);
    Std.printInt(rrr( 7; 2; -(-10), 45));
    Std.printInt( 10 + 11 % 2 * 10);
    Std.printBoolean( 5 + 1 < 8 * 5 );
    Std.printBoolean( "er" ++ "err" == "errrr");
    Std.printInt(-(val v: Int = 4; v));
    Std.printInt(val v: Int = 4; val cc : Int = 7; 10);
    Std.printInt(r())
    
}