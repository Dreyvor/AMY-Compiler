package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    val namesByModule: List[(N.Name, List[N.ClassOrFunDef])] = p.modules.map(mod => (mod.name, mod.defs))
    namesByModule.foreach {
      mod =>
        mod._2.groupBy(_.name).foreach {
          case (name, l) => if (l.size > 1) fatal(s"Two definitions named $name in ${mod._1}", l.head.position)
        }
    }


    // Step 3: Discover types and add them to symbol table
    p.modules.foreach {
      mod =>
        mod.defs.foreach {
          case N.AbstractClassDef(name) => table.addType(mod.name, name)
          case _ =>
        }
    }

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach {
      mod =>
        mod.defs.foreach {
          case ccd@N.CaseClassDef(name, fields, parent) =>
            if (table.getType(mod.name, parent).isEmpty) {
              fatal(s"Parent ($parent) type of class ($name) must be in the same module ($mod)", ccd)
            } else {
              table.addConstructor(mod.name, name, fields.map(transformType(_, mod.name)), table.getType(mod.name, parent).get)
            }
          case _ =>
        }
    }

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach {
      mod =>
        mod.defs.foreach {
          case N.FunDef(name, params, retType, _) =>
            table.addFunction(mod.name, name, params.map(param => transformType(param.tt, mod.name)), transformType(retType, mod.name))
          case _ =>
        }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions

    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = {
      df match {
        case N.AbstractClassDef(name) =>
          S.AbstractClassDef(table.getType(module, name).get)
        case N.CaseClassDef(name, _, _) =>
          val Some((id, constrSig)) = table.getConstructor(module, name)
          S.CaseClassDef(id, constrSig.argTypes.map(S.TypeTree), constrSig.parent)
        case fd: N.FunDef =>
          transformFunDef(fd, module)
      }
      }.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy ==> shadowing
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
              case N.CaseClassPattern(constr, args) =>
                val myClass: Option[(Identifier, ConstrSig)] = table.getConstructor(constr.module.getOrElse(module), constr.name)
                if (myClass.isEmpty || myClass.get._2.argTypes.size != args.size) {
                  fatal("There does not exist a constructor with such signature", pat.position)
                } else {
                  val myArgs: List[(S.Pattern, List[(String, Identifier)])] = args.map(transformPattern)
                  (S.CaseClassPattern(myClass.get._1, myArgs.map(_._1)).setPos(pat.position), myArgs.flatMap(_._2))
                }

              case pat2@N.IdPattern(name) =>
                if (locals.contains(name)) {
                  fatal(s"Multiple definitions for $name", pat2)
                } else {
                  //check if there is a nullary constructor with same name
                  val myConstr = table.getConstructor(module, name)
                  if (myConstr.isEmpty || myConstr.get._2.argTypes.isEmpty) {
                    warning(s"An identifier pattern ($name) is used which has the same name with a nullary constructor", pat2)
                  }
                  val newName = Identifier.fresh(name)
                  (S.IdPattern(newName).setPos(pat.position), List((name, newName)))
                }

              case N.LiteralPattern(lit) => (S.LiteralPattern(lit.asInstanceOf[S.Literal[lit.type]]).setPos(pat.position), Nil)

              case N.WildcardPattern() => (S.WildcardPattern(), Nil)
            }
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, newLocals) = transformPattern(pat)
            newLocals.groupBy(_._1).foreach {
              case (name, tuples) =>
                if (tuples.size > 1) {
                  fatal(s"Duplicate variable $name in $pat", cse)
                }
            }
            S.MatchCase(newPat, transformExpr(rhs)(module, (params, locals ++ newLocals))).setPos(cse.position)
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))


        case N.IntLiteral(v) => S.IntLiteral(v)
        case N.BooleanLiteral(v) => S.BooleanLiteral(v)
        case N.StringLiteral(s) => S.StringLiteral(s)
        case N.UnitLiteral() => S.UnitLiteral()

        case N.Neg(e) => S.Neg(transformExpr(e))
        case N.Not(e) => S.Not(transformExpr(e))
        case N.Error(msg) => S.Error(transformExpr(msg))
        case N.Or(lhs, rhs) => S.Or(transformExpr(lhs), transformExpr(rhs))
        case N.And(lhs, rhs) => S.And(transformExpr(lhs), transformExpr(rhs))
        case N.Div(lhs, rhs) => S.Div(transformExpr(lhs), transformExpr(rhs))
        case N.Mod(lhs, rhs) => S.Mod(transformExpr(lhs), transformExpr(rhs))
        case N.Plus(lhs, rhs) => S.Plus(transformExpr(lhs), transformExpr(rhs))
        case N.Minus(lhs, rhs) => S.Minus(transformExpr(lhs), transformExpr(rhs))
        case N.Times(lhs, rhs) => S.Times(transformExpr(lhs), transformExpr(rhs))
        case N.Concat(lhs, rhs) => S.Concat(transformExpr(lhs), transformExpr(rhs))
        case N.Equals(lhs, rhs) => S.Equals(transformExpr(lhs), transformExpr(rhs))
        case N.Sequence(e1, e2) => S.Sequence(transformExpr(e1), transformExpr(e2))
        case N.LessThan(lhs, rhs) => S.LessThan(transformExpr(lhs), transformExpr(rhs))
        case N.LessEquals(lhs, rhs) => S.LessEquals(transformExpr(lhs), transformExpr(rhs))
        case N.Ite(cond, thenn, elze) => S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))

        case myVar@N.Variable(name) =>
          if (locals.contains(name)) {
            S.Variable(locals(name))
          } else if (params.contains(name)) {
            S.Variable(params(name))
          } else {
            fatal(s"No such variable ($name) declared in locals or in parameters", myVar)
          }

        case c@N.Call(qname, args) =>
          val fct = table.getFunction(qname.module.getOrElse(module), qname.name)
          if (fct.isEmpty || fct.get._2.argTypes.size != args.size) {
            //fct not defined as a fct. Let's check constructors
            val myConstr = table.getConstructor(qname.module.getOrElse(module), qname.name)
            if (myConstr.isEmpty || myConstr.get._2.argTypes.size != args.size) {
              fatal("There is no function or constructor named " + qname.name + " with this signature (number of params may be different)", c)
            } else {
              //There is constructor
              S.Call(myConstr.get._1, args.map(transformExpr))
            }
          } else {
            // There is a fct
            S.Call(fct.get._1, args.map(transformExpr))
          }

        case n@N.Let(df, value, body) =>
          if (locals.contains(df.name)) {
            fatal("The variable " + df.name + " is already defined in locals", n)
          } else if (params.contains(df.name)) {
            warning("The variable " + df.name + " is already defined in params ==> shadowing", n)
          }
          //get new id
          val newId = Identifier.fresh(df.name)
          // adapt locals
          val localToAdd = (df.name, newId)
          S.Let(S.ParamDef(newId, S.TypeTree(transformType(df.tt, module))), transformExpr(value), transformExpr(body)(module, (params, locals + localToAdd)))

        case _ => fatal("Unknown expression in transformCase", expr)
      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }

}
