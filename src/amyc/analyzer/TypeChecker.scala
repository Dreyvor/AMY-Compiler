package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private(id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]

      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {

      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))

      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)

        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)

        case StringLiteral(_) =>
          topLevelConstraint(StringType)

        case UnitLiteral() =>
          topLevelConstraint(UnitType)

        case Variable(name) =>
          topLevelConstraint(env(name))

        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          //Both should have same type ==> generate a new one instead of get the type of first
          val newType = TypeVariable.fresh()
          genConstraints(lhs, newType) ++ genConstraints(rhs, newType) ++ topLevelConstraint(BooleanType)


        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
          (List[Constraint], Map[Identifier, Type]) = {
            pat match {
              case CaseClassPattern(constr, args) =>
                val constraintConstructor = Constraint(table.getConstructor(constr).get.retType, scrutExpected, pat.position)
                //We have to re-evaluate the pattern from args. Need to zip them with their respective type from table
                val constraintArgs = args.zip(table.getConstructor(constr).get.argTypes).map(e => handlePattern(e._1, e._2))
                //We have to put everything together
                constraintArgs.foldLeft((List(constraintConstructor), Map[Identifier, Type]())) {
                  case ((lAcc, mAcc), (l, m)) => (lAcc ++ l, mAcc ++ m)
                }

              case IdPattern(name) =>
                (genConstraints(Variable(name), scrutExpected)(env + (name -> scrutExpected)), Map(name -> scrutExpected))

              case LiteralPattern(lit) =>
                (genConstraints(lit, scrutExpected), Map.empty)

              case WildcardPattern() =>
                (Nil, Map.empty)
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv) ++ topLevelConstraint(expected)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))


        case Plus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Minus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Times(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Div(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Mod(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case LessThan(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)

        case LessEquals(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)

        case And(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)

        case Or(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)

        case Concat(lhs, rhs) =>
          genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType) ++ topLevelConstraint(StringType)

        case Not(lhs) =>
          genConstraints(lhs, BooleanType) ++ topLevelConstraint(BooleanType)

        case Neg(lhs) =>
          genConstraints(lhs, IntType) ++ topLevelConstraint(IntType)

        case Call(qname, args) =>
          //It could be a function or a constructor ==> Check if function or constructor
          val fct = table.getFunction(qname)
          if (fct.isEmpty) {
            //It's a constructor, thus get it from table
            val constr = table.getConstructor(qname)
            //generate constraints for arguments
            val constrArgs = args.zip(constr.get.argTypes).foldLeft(List[Constraint]()) {
              case (acc, e) => acc ++ genConstraints(e._1, e._2)
            }
            // Add top lvl
            constrArgs ++ topLevelConstraint(constr.get.retType)

          } else {
            //It's a function
            //generate constraints for arguments
            val constrArgs = args.zip(fct.get.argTypes).foldLeft(List[Constraint]()) {
              case (acc, e) => acc ++ genConstraints(e._1, e._2)
            }
            //Add top lvl
            constrArgs ++ topLevelConstraint(fct.get.retType)
          }

        case Sequence(e1, e2) =>
          val e2Type = TypeVariable.fresh()
          genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, e2Type) ++ topLevelConstraint(e2Type) //TODO: Check return type

        case Let(df, value, body) =>
          val bodyType = TypeVariable.fresh()
          genConstraints(value, df.tt.tpe) ++ genConstraints(body, bodyType)(env + (df.name -> df.tt.tpe)) ++ topLevelConstraint(bodyType) //TODO: Check return type

        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected) ++ topLevelConstraint(expected)

        case Error(msg) =>
          genConstraints(msg, StringType) ++ topLevelConstraint(expected)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          //Expected can be a variable ==> replace the variable type by the type found
          //Expected is a "simple defined" type (Int, String, ...) ==> verify if it matches with the found type, else ==> return error
          expected match {
            case TypeVariable(tid) =>
              //replace it by the actual type
              solveConstraints(subst_*(more, tid, found))

            case _ =>
              //Check if types match or not
              if (found.toString == expected.toString) {
                solveConstraints(more)
              } else {
                error("Types don't match (expect: " + expected + "; found: " + found + ")", pos)
              }
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map { case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
