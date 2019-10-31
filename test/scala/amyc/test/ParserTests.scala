package amyc.test

import amyc.parsing._
import org.junit.Test

class ParserTests extends TestSuite with amyc.MainHelpers {
  val pipeline = Lexer andThen Parser andThen treePrinterN("")

  val baseDir = "parser"

  val outputExt = "scala"

  @Test def testLL1 = {
    assert(Parser.program.isLL1)
  }

  @Test def testEmpty = shouldOutput("Empty")
  @Test def testLiterals = shouldOutput("Literals")
  @Test def persoTestPattern = shouldOutput("Pattern_textAMY")
  @Test def persoTestPattern2 = shouldOutput("TestParser_textAMY")
  @Test def testArithmetic = shouldOutput("Arithmetic")
  @Test def testFactorial = shouldOutput("Factorial")
  @Test def testHanoi = shouldOutput("Hanoi")
  @Test def testHello = shouldOutput("Hello")
  @Test def testPrinting = shouldOutput("Printing")
  @Test def testTestLists = shouldOutput("TestLists")

  @Test def testEmptyFile = shouldFail("EmptyFile")
}

