object example1_fromExtensionsList {
  val l: L.List = L.Cons(1, L.Cons(2, L.Cons(error("lazy"), L.Nil())));
  //val l: L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  // No error is thrown
  Std.printString("######################################");
  l match {
    case L.Nil() => () // At this point, we evaluate l just enough
    // to know it is a Cons
    //case L.Cons(h, t) => Std.printInt(h) // Prints 1
    case L.Cons(h1, L.Cons(h2, L.Cons(h3, _))) =>
      Std.printInt(h1);
      Std.printInt(h2);
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      // If you uncomment the line that print "h3", please end the next line with a ";"
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Std.printString("######################################")
      // Still no error...
      // As said above, you can try to comment/uncomment this line to see how the code is interpreted.
      // But don't forget the ";" as said in comment above
      //Std.printInt(h3)
    // This forces evaluation of the third list element
    // and an error is thrown!
  }
}
