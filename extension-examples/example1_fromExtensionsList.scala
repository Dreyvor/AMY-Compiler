object example1_fromExtensionsList {
  val x: Int = (Std.printInt(42); 0); // Nothing happens
  val y: Int = x + 1; // Still nothing...
  Std.printString("######################################");
  Std.printInt(y) // 42 and 1 are printed
}
