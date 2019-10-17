package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion.syntactic._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Syntaxes[Token, TokenKind] with Debug[Token, TokenKind]
                 with Operators {
  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
    case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] = abstractClassDefinition | caseClassDefinition | functionDefinition

  lazy val abstractClassDefinition: Syntax[ClassOrFunDef] =
    (kw("abstract") ~ kw("class") ~ identifier).map {
      case kw ~ _ ~ id => AbstractClassDef(id).setPos(kw)
    }

  lazy val caseClassDefinition: Syntax[ClassOrFunDef] =
    (kw("case") ~ kw("class") ~ identifier ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifier).map{
      case kw ~ _ ~ name ~ _ ~ params ~ _ ~ _ ~ parent => CaseClassDef(name, params.map(_.tt), parent).setPos(kw)
    }

  lazy val functionDefinition: Syntax[ClassOrFunDef] =
    (kw("def") ~ identifier ~ "(" ~ parameters ~ ")" ~ ":" ~ typeTree ~ "=" ~ "{" ~ expr ~ "}").map{
      case kw ~ name ~ _ ~ params ~ _ ~ _ ~ t ~ _ ~ _ ~ body ~ _ => FunDef(name, params, t, body).setPos(kw)
    }

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] =
    (identifierPos ~ ":" ~ typeTree).map{
      case namePos ~ _ ~ t => ParamDef(namePos._1, t).setPos(namePos._2)
    }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] =
    (opt(identifierPos ~ ".".skip) ~ identifierPos).map {
      case Some(m) ~ name => TypeTree(ClassType(QualifiedName(Option(m._1), name._1))).setPos(m._2)
      case None ~ name => TypeTree(ClassType(QualifiedName(None, name._1))).setPos(name._2)
    }


  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence
  lazy val expr: Syntax[Expr] = recursive { ??? }

  // A literal expression.
  lazy val literal: Syntax[Literal[_]] = ???

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | idPattern | ???
  }

  lazy val idPattern: Syntax[Pattern] =
    identifierPos.map{
      case name => IdPattern(name._1).setPos(name._2)
    }

  lazy val literalPattern: Syntax[Pattern] = ???


  lazy val wildPattern: Syntax[Pattern] = ???


  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] = literal.up[Expr] | variableOrCall | ???

  lazy val variableOrCall: Syntax[Expr] = ???


  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.



  // Ensures the grammar is in LL(1), otherwise prints some counterexamples
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      debug(program)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    program(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
