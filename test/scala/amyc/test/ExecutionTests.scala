package amyc.test

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  val baseDir = "interpreter"

  val outputExt = "txt"

  @Test def testEmptyObject = shouldOutput("EmptyObject")
  @Test def testArithmetic = shouldOutput(List("library/Std", "library/List", "library/Option", "Arithmetic"), "Arithmetic")
  @Test def testFactorial = shouldOutput(List("library/Std", "library/List", "library/Option", "Factorial"),"Factorial")
  @Test def testHanoi = shouldOutput(List("library/Std", "library/List", "library/Option", "Hanoi"), "Hanoi")
  @Test def testHello = shouldOutput(List("library/Std", "library/List", "library/Option", "Hello"), "Hello")
  @Test def testPrinting = shouldOutput(List("library/Std", "library/List", "library/Option", "Printing"), "Printing")
  @Test def testTestLists = shouldOutput(List("library/Std", "library/List", "library/Option", "TestLists"), "TestLists")
  @Test def testMyOwnTest = shouldOutput(List("library/Std", "library/List", "library/Option", "myOwnTest"), "myOwnTest")
  @Test def testInfiniteStream = shouldOutput(List("library/Std", "library/List", "library/Option", "InfiniteStream"), "InfiniteStream")
  @Test def testHeadInfiniteStream = shouldOutput(List("library/Std", "library/List", "library/Option", "HeadInfiniteStream"), "HeadInfiniteStream")


  @Test def testMinimalError = shouldFail("MinimalError")

}
