object Arithmetic {
  val a:Int = 0;

  val b:Int = 0;
  if(a == 0){a} else {a}
  val b:Int = 0;

  case class MyCase(a:Int) extends MySupClass
  abstract class MySupClass

  def pow(b: Int, e: Int): Int = {
    if (e == 0) { 1 }
    else {
      if (e % 2 == 0) {
        val rec: Int = pow(b, e/2);
        rec * rec
      } else {
        b * pow(b, e - 1)
      }
    }
  }

  def gcd(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      a + b
    } else {
      if (a < b) {
        gcd(a, b % a)
      } else {
        gcd(a % b, b)
      }
    }
  }

  Std.printInt(pow(0, 10));
  Std.printInt(pow(1, 5));
  Std.printInt(pow(2, 10));
  Std.printInt(pow(3, 3));
  Std.printInt(gcd(0, 10));
  Std.printInt(gcd(17, 99)); // 1
  Std.printInt(gcd(16, 46)); // 2
  Std.printInt(gcd(222, 888)) // 222
}
