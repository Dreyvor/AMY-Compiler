package amyc
package interpreter

import utils._
import ast.SymbolicTreeModule._
import ast.{Identifier, TreeModule}
import analyzer.SymbolTable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this match {
      case Thunk(value, e, _) =>
        if (value.isDefined) {
          value.get.asInstanceOf[IntValue].i
        } else {
          -1
        }
      case _ => asInstanceOf[IntValue].i
    }

    def asBoolean: Boolean = this match {
      case Thunk(value, e, _) =>
        if (value.isDefined) {
          value.get.asInstanceOf[BooleanValue].b
        } else {
          false
        }
      case _ => asInstanceOf[BooleanValue].b
    }

    def asString: String = this match {
      case Thunk(value, e, _) =>
        if (value.isDefined) {
          value.get.asInstanceOf[StringValue].s
        } else {
          ""
        }
      case _ => asInstanceOf[StringValue].s
    }

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
      case Thunk(value, e, env) =>
        if (value.isDefined && !value.get.isInstanceOf[Thunk]) {
          value.get.toString
        } else {
          "Not evaluated:" + e.toString()
        }
    }
  }

  case class IntValue(i: Int) extends Value

  case class BooleanValue(b: Boolean) extends Value

  case class StringValue(s: String) extends Value

  case object UnitValue extends Value

  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

  case class Thunk(var value: Option[Value], e: Expr, env: Map[Identifier, Value]) extends Value

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt") -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString") -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt") -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString") -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined

    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name

    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match {
        case Variable(name) => locals(name) match {
          case t@Thunk(value, e, env) =>
            if (value.isEmpty) {
              val tmp = locals(name).asInstanceOf[Thunk]
              val res = interpret(e)(env)
              res match {
                case r@IntValue(_) => tmp.value = Some(r)
                case r@StringValue(_) => tmp.value = Some(r)
                case r@BooleanValue(_) => tmp.value = Some(r)
                case r@UnitValue => tmp.value = Some(r)
                case r@Thunk(_, _, _) => tmp.value = Some(r)
                case r@CaseClassValue(_, _) => tmp.value = Some(r)
              }
              tmp.value.get
            } else {
              value.get
            }
          case _ => locals(name)
        }
        case IntLiteral(i) => //OK
          IntValue(i)
        case BooleanLiteral(b) => //OK
          BooleanValue(b)
        case StringLiteral(s) => //OK
          StringValue(s)
        case UnitLiteral() => //OK
          UnitValue
        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) =>
          if (interpret(rhs).asInt != 0) IntValue(interpret(lhs).asInt / interpret(rhs).asInt)
          else ctx.reporter.fatal("Division by 0")
        case Mod(lhs, rhs) =>
          if (interpret(rhs).asInt != 0) IntValue(interpret(lhs).asInt % interpret(rhs).asInt)
          else ctx.reporter.fatal("Modulo by 0")
        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean && interpret(rhs).asBoolean)
        case Or(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean || interpret(rhs).asBoolean)
        case Equals(lhs, rhs) =>
          interpret(lhs) match {
            case IntValue(i) => BooleanValue(i == interpret(rhs).asInt)
            case UnitValue => BooleanValue(true)
            case BooleanValue(b) => BooleanValue(b == interpret(rhs).asBoolean)
            case _ => BooleanValue(interpret(lhs) eq interpret(rhs))
          }
        // Hint: Take care to implement Amy equality semantics
        case Concat(lhs, rhs) =>
          StringValue(interpret(lhs).asString + interpret(rhs).asString)
        case Not(e) =>
          BooleanValue(!interpret(e).asBoolean)
        case Neg(e) =>
          IntValue(-interpret(e).asInt)
        case Call(qname, args) =>
          if (isConstructor(qname)) {
            CaseClassValue(qname, args.map(interpretLazy))
          }
          else if (builtIns.contains(findFunctionOwner(qname), qname.name)) {
            builtIns(findFunctionOwner(qname), qname.name)(args.map(interpret))
          }
          else {
            val myFunc: FunDef = findFunction(findFunctionOwner(qname), qname.name)
            val newLocals: Map[Identifier, Value] = myFunc.paramNames.zip(args.map(interpretLazy)).toMap
            interpret(myFunc.body)(newLocals)
          }
        // Hint: Check if it is a call to a constructor first,
        //       then if it is a built-in function (otherwise it is a normal function).
        //       Use the helper methods provided above to retrieve information from the symbol table.
        //       Think how locals should be modified.
        case Sequence(e1, e2) =>
          interpret(e1)
          interpret(e2)
        case Let(df, value, body) =>
          interpret(body)(value match {
            case IntLiteral(i) => locals + (df.name -> IntValue(i))
            case StringLiteral(s) => locals + (df.name -> StringValue(s))
            case BooleanLiteral(b) => locals + (df.name -> BooleanValue(b))
            case UnitLiteral() => locals + (df.name -> UnitValue)
            case _ => locals + (df.name -> Thunk(None, value, locals))
          })
        case Ite(cond, thenn, elze) =>
          if (interpret(cond).asBoolean) {
            interpret(thenn)
          }
          else {
            interpret(elze)
          }

        case Match(scrut, cases) =>
          // Hint: We give you a skeleton to implement pattern matching
          //       and the main body of the implementation

          val evS = interpret(scrut)

          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) =>
                Some(Nil)
              case (_, IdPattern(name)) =>
                Some(List(name -> v))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                /*
                x match {
                  case 12 => ...
                }
                 */
                if (i1 == i2) Some(Nil)
                else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if (b1 == b2) Some(Nil)
                else None
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                //Never matches
                None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(Nil)
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                /*
                Cons(h, t) match {
                  case Cons (h, t) => ...
                }
                 */
                if (con1 == con2) {
                  val myMap = formalArgs.zip(realArgs).map(e => matchesPattern(e._2, e._1))
                  if (myMap.contains(None)) None
                  else Some(myMap.flatten.flatten)
                } else None

              case (Thunk(value, e, env), _) =>
                val tmp = interpretLazy(e)(env)
                matchesPattern(tmp, pat)
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpret(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

        case Error(msg) => {
          ctx.reporter.fatal(interpret(msg).asString)
        }
      }
    }

    def interpretLazy(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match {
        case Variable(name) => locals(name) match {
          case t@Thunk(value, e, env) =>
            if (value.isDefined) {
              value.get
            } else {
              t
            }
          case _ => locals(name)
        }

        case IntLiteral(i) => //OK
          IntValue(i)
        case BooleanLiteral(b) => //OK
          BooleanValue(b)
        case StringLiteral(s) => //OK
          StringValue(s)
        case UnitLiteral() => //OK
          UnitValue
        case Plus(lhs, rhs) =>
          Thunk(None, Plus(lhs, rhs), locals)
        case Minus(lhs, rhs) =>
          Thunk(None, Minus(lhs, rhs), locals)
        case Times(lhs, rhs) =>
          Thunk(None, Times(lhs, rhs), locals)
        case Div(lhs, rhs) =>
          if (interpret(rhs).asInt != 0) Thunk(None, Div(lhs, rhs), locals)
          else ctx.reporter.fatal("Division by 0")
        case Mod(lhs, rhs) =>
          if (interpret(rhs).asInt != 0) Thunk(None, Mod(lhs, rhs), locals)
          else ctx.reporter.fatal("Modulo by 0")
        case LessThan(lhs, rhs) =>
          Thunk(None, LessThan(lhs, rhs), locals)
        case LessEquals(lhs, rhs) =>
          Thunk(None, LessEquals(lhs, rhs), locals)
        case And(lhs, rhs) =>
          Thunk(None, And(lhs, rhs), locals)
        case Or(lhs, rhs) =>
          Thunk(None, Or(lhs, rhs), locals)
        case Equals(lhs, rhs) =>
          Thunk(None, Equals(lhs, rhs), locals)
        // Hint: Take care to implement Amy equality semantics
        case Concat(lhs, rhs) =>
          Thunk(None, Concat(lhs, rhs), locals)
        case Not(e) =>
          Thunk(None, Not(e), locals)
        case Neg(e) =>
          Thunk(None, Neg(e), locals)
        case Call(qname, args) => //We need to interpret only the first step of args for a List. Return thunks otherwise
          if (isConstructor(qname)) {
            CaseClassValue(qname, args.map(e => Thunk(None, e, locals)))
          }
          else if (builtIns.contains(findFunctionOwner(qname), qname.name)) {
            builtIns(findFunctionOwner(qname), qname.name)(args.map(interpret))
          }
          else {
            val myFunc: FunDef = findFunction(findFunctionOwner(qname), qname.name)
            val newLocals: Map[Identifier, Value] = myFunc.paramNames.zip(args.map(e => Thunk(None, e, locals))).toMap
            interpretLazy(myFunc.body)(newLocals)
          }
        // Hint: Check if it is a call to a constructor first,
        //       then if it is a built-in function (otherwise it is a normal function).
        //       Use the helper methods provided above to retrieve information from the symbol table.
        //       Think how locals should be modified.
        case Sequence(e1, e2) =>
          interpretLazy(e1)
          interpretLazy(e2)
        case Let(df, value, body) =>
          interpretLazy(body)(value match {
            case IntLiteral(i) => locals + (df.name -> IntValue(i))
            case StringLiteral(s) => locals + (df.name -> StringValue(s))
            case BooleanLiteral(b) => locals + (df.name -> BooleanValue(b))
            case UnitLiteral() => locals + (df.name -> UnitValue)
            case _ => locals + (df.name -> Thunk(None, value, locals))
          })
        case Ite(cond, thenn, elze) =>
          if (interpret(cond).asBoolean) {
            interpretLazy(thenn)
          }
          else {
            interpretLazy(elze)
          }

        case Match(scrut, cases) =>
          // Hint: We give you a skeleton to implement pattern matching
          //       and the main body of the implementation

          val evS = interpret(scrut)

          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            (v, pat) match {
              case (_, WildcardPattern()) =>
                Some(Nil)
              case (_, IdPattern(name)) =>
                Some(List(name -> v))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                /*
                x match {
                  case 12 => ...
                }
                 */
                if (i1 == i2) Some(Nil)
                else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if (b1 == b2) Some(Nil)
                else None
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                //Never matches
                None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(Nil)
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                /*
                Cons(h, t) match {
                  case Cons (h, t) => ...
                }
                 */
                if (con1 == con2) {
                  val myMap = formalArgs.zip(realArgs).map(e => matchesPattern(e._2, e._1))
                  if (myMap.contains(None)) None
                  else Some(myMap.flatten.flatten)
                } else None
              case (Thunk(value, e, env), _) =>
                if (value.isDefined) {
                  matchesPattern(value.get, pat)
                } else {
                  matchesPattern(interpretLazy(e)(env), pat)
                }
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpretLazy(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

        case Error(msg) => {
          ctx.reporter.fatal(interpret(msg).asString)
        }
      }
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpretLazy(e)(Map())
    }
  }

}
