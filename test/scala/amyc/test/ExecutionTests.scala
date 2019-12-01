package amyc.test

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  val baseDir = "interpreter"

  val outputExt = "txt"

  @Test def testEmptyObject = shouldOutput("EmptyObject")
  @Test def testArithmetic = shouldOutput(List("Arithmetic", "List", "Option", "Std"), "Arithmetic")
  @Test def testFactorial = shouldOutput(List("Factorial", "List", "Option", "Std"),"Factorial")
  @Test def testHanoi = shouldOutput(List("Hanoi", "List", "Option", "Std"),"Hanoi")
  @Test def testHello = shouldOutput(List("Hello", "List", "Option", "Std"),"Hello")
  //@Test def testHelloInt = shouldOutput(List("HelloInt", "List", "Option", "Std"),"HelloInt")
  @Test def testPrinting = shouldOutput(List("Printing", "List", "Option", "Std"),"Printing")
  @Test def testTestLists = shouldOutput(List("TestLists", "List", "Option", "Std"),"TestLists")
  @Test def testTestLists2 = shouldOutput(List("TestLists2", "List", "Option", "Std"),"TestLists2")

  @Test def testMinimalError = shouldFail("MinimalError")

}
