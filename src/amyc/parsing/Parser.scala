package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds.{LiteralKind, _}

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
    (kw("case") ~ kw("class") ~ identifier ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifier).map {
      case kw ~ _ ~ name ~ _ ~ params ~ _ ~ _ ~ parent => CaseClassDef(name, params.map(_.tt), parent).setPos(kw)
    }

  lazy val functionDefinition: Syntax[ClassOrFunDef] =
    (kw("def") ~ identifier ~ "(" ~ parameters ~ ")" ~ ":" ~ typeTree ~ "=" ~ "{" ~ expr ~ "}").map {
      case kw ~ name ~ _ ~ params ~ _ ~ _ ~ t ~ _ ~ _ ~ body ~ _ => FunDef(name, params, t, body).setPos(kw)
    }

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] =
    (identifierPos ~ ":" ~ typeTree).map {
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
  lazy val expr: Syntax[Expr] = recursive {
    simpleExpr |
      exprSeq |
      exprITE |
      exprMatch |
      exprError |
      exprOp
  }

  // A literal expression.
  lazy val literal: Syntax[Literal[_]] = boolIntStrLit | unitLit

  lazy val boolIntStrLit: Syntax[Literal[_]] = accept(LiteralKind) {
    case tk@BoolLitToken(boolVal) => BooleanLiteral(boolVal).setPos(tk)
    case tk@StringLitToken(value) => StringLiteral(value).setPos(tk)
    case tk@IntLitToken(value) => IntLiteral(value).setPos(tk)
  }

  lazy val unitLit: Syntax[Literal[_]] = ("(" ~ ")").map { case o ~ _ => UnitLiteral().setPos(o) }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | idPattern | caseClassPattern //ASK: Is it possible to upcast to Pattern ?
  }

  lazy val caseClassPattern: Syntax[Pattern] = (identifierType ~ "(" ~ patterns ~ ")").map {
    case id ~ _ ~ pats ~ _ => CaseClassPattern(id.tpe match { case ClassType(qname) => qname }, pats).setPos(id)
  }

  lazy val idPattern: Syntax[Pattern] =
    identifierPos.map {
      name => IdPattern(name._1).setPos(name._2)
    }

  lazy val literalPattern: Syntax[Pattern] = literal.map { lit => LiteralPattern(lit).setPos(lit) }

  lazy val wildPattern: Syntax[Pattern] = (kw("_")).map { kw => WildcardPattern().setPos(kw) }

  lazy val patterns: Syntax[List[Pattern]] = repsep(pattern, ",").map(_.toList)


  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] = exprId | literal.up[Expr] | variableOrCall | parenExpr

  lazy val variableOrCall: Syntax[Expr] = variable | call

  lazy val variable: Syntax[Expr] = (kw("val") ~ parameter ~ "=" ~ expr ~ ";" ~ expr).map {
    case kw ~ nameAndType ~ _ ~ e1 ~ _ ~ e2 => Let(nameAndType, e1, e2).setPos(kw)
  }

  lazy val call: Syntax[Expr] = (identifierType ~ "(" ~ arguments ~ ")").map {
    case id ~ _ ~ args ~ _ => Call(id.tpe match { case ClassType(qname) => qname }, args).setPos(id)
  }

  lazy val arguments: Syntax[List[Expr]] = repsep(expr, ",").map(_.toList)

  lazy val parenExpr: Syntax[Expr] = ("(" ~ expr ~ ")").map { case _ ~ e ~ _ => e }

  lazy val exprId: Syntax[Expr] = identifierPos.map { case id => StringLiteral(id._1).setPos(id._2) }

  lazy val exprSeq: Syntax[Expr] = (expr ~ ";" ~ expr).map { case e1 ~ _ ~ e2 => Sequence(e1, e2).setPos(e1) }

  lazy val exprITE: Syntax[Expr] = (kw("if") ~ "(".skip ~ expr ~ (")" ~ "{").skip ~ expr ~ ("}" ~ kw("else") ~ "{").skip ~ expr ~ "}".skip).map {
    case kw ~ eTest ~ eTrue ~ eFalse => Ite(eTest, eTrue, eFalse).setPos(kw)
  }

  lazy val matchCaseDef: Syntax[MatchCase] = (kw("case") ~ pattern ~ "=>" ~ expr).map {
    case kw ~ pat ~ _ ~ e => MatchCase(pat, e).setPos(kw)
  }

  lazy val exprMatch: Syntax[Expr] = (expr ~ (kw("match") ~ "{").skip ~ many1(matchCaseDef) ~ "}".skip).map {
    case e ~ cases => Match(e, cases.toList).setPos(e)
  }

  lazy val exprError: Syntax[Expr] = (kw("error") ~ "(".skip ~ expr ~ ")".skip).map {
    case kw ~ e => Error(e).setPos(kw)
  }

  lazy val exprOp: Syntax[Expr] = operators(expr)(
    op("!") | op("-") is LeftAssociative,
    op("%") | op("/") | op("*") is LeftAssociative,
    op("+") | op("++") | op("-") is LeftAssociative,
    op("<") | op("<=") is LeftAssociative,
    op("==") is LeftAssociative,
    op("&&") is LeftAssociative,
    op("||") is LeftAssociative
  )(
    {
      case (null, "!", rhs) => Not(rhs).setPos(rhs) //TODO: check if it's bullshit for setPos
      case (null, "-", rhs) => Neg(rhs).setPos(rhs) //TODO: check if it's bullshit for setPos
      case (lhs, "*", rhs) => Times(lhs, rhs).setPos(lhs)
      case (lhs, "/", rhs) => Div(lhs, rhs).setPos(lhs)
      case (lhs, "%", rhs) => Mod(lhs, rhs).setPos(lhs)
      case (lhs, "+", rhs) => Plus(lhs, rhs).setPos(lhs)
      case (lhs, "-", rhs) => Minus(lhs, rhs).setPos(lhs)
      case (lhs, "++", rhs) => Concat(lhs, rhs).setPos(lhs)
      case (lhs, "<", rhs) => LessThan(lhs, rhs).setPos(lhs)
      case (lhs, "<=", rhs) => LessEquals(lhs, rhs).setPos(lhs)
      case (lhs, "==", rhs) => Equals(lhs, rhs).setPos(lhs)
      case (lhs, "&&", rhs) => And(lhs, rhs).setPos(lhs)
      case (lhs, "||", rhs) => Or(lhs, rhs).setPos(lhs)
    },
  )

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
