package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")

  @Test def testArithmetic = shouldOutput("Arithmetic")
  @Test def testFactorial = shouldOutput("Factorial")
  @Test def testHanoi = shouldOutput("Hanoi")
  @Test def testHello = shouldOutput("Hello")
  @Test def testHelloInt = shouldOutput("HelloInt")
  @Test def testPrinting = shouldOutput("Printing")
  @Test def testTestLists = shouldOutput("TestLists")
  @Test def testMoodleTest = shouldOutput("MoodleTest")

  @Test def testPersonalTest = shouldOutput("PersonalTest")

  @Test def testSingleAmp = shouldFail("SingleAmp")

}
