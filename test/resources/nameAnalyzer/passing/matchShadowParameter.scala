object matchShadowParameter {
  def foo(p: Int): Int = {
    p match {
      case p => p
      //case _ => p
    }
  }
}