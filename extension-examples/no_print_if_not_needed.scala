object no_print_if_not_needed {
  val a:Int = 1+2;
  val b:Int = 1+a;
  Std.printString("######################################");
  b; //We shouldn't eval b here as it's not needed
  a; //We shouldn't eval a here as it's not needed
  a+b; //We shouldn't eval a+b here as it's not needed
  Std.printInt(a);
  Std.printInt(b)
}
