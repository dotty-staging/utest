package utest
package asserts

import scala.quoted._

/**
 * Macro implementation to take a block of code and trace through it,
 * converting it into an [[AssertEntry]] and inserting debug loggers.
 */
object Tracer {

  def traceOne[I, O](func: Expr[AssertEntry[I] => O], expr: Expr[I])(using QuoteContext, Type[I], Type[O]): Expr[O] =
    traceOneWithCode(func, expr, codeOf(expr))

  def traceOneWithCode[I, O](func: Expr[AssertEntry[I] => O], expr: Expr[I], code: String)(using qctx: QuoteContext, tt: Type[I], to: Type[O]): Expr[O] = {
    val tree = makeAssertEntry(expr, code)
    Expr.betaReduce('{ $func($tree)})
  }

  def apply[T](func: Expr[Seq[AssertEntry[T]] => Unit], exprs: Expr[Seq[T]])(using qctx: QuoteContext, tt: Type[T]): Expr[Unit] = {
    exprs match {
      case Varargs(ess) =>
        val trees: Expr[Seq[AssertEntry[T]]] = Expr.ofSeq(ess.map(e => makeAssertEntry(e, codeOf(e))))
        Expr.betaReduce('{ $func($trees)})

      case _ => throw new RuntimeException(s"Only varargs are supported. Got: ${exprs.asReflectTree}")
    }
  }

  def codeOf[T](expr: Expr[T])(using QuoteContext): String =
    expr.asReflectTree.pos.sourceCode

  private def tracingMap(logger: Expr[TestValue => Unit])(using QuoteContext) =
    import qctx.reflect._
    new TreeMap {
      // Do not descend into definitions inside blocks since their arguments are unbound
      override def transformStatement(tree: Statement)(using ctx: Context): Statement = tree match
        case _: DefDef => tree
        case _ => super.transformStatement(tree)

      override def transformTerm(tree: Term)(implicit ctx: Context): Term = {
        tree match {
          case i @ Ident(name) if i.symbol.pos.exists
            && i.pos.exists
            // only trace identifiers coming from the same file,
            // since those are the ones people probably care about
            && i.symbol.pos.sourceFile == i.pos.sourceFile
            // Don't trace methods, since you cannot just print them "standalone"
            // without providing arguments
            && !i.symbol.isDefDef && !i.symbol.isClassConstructor
            // Don't trace identifiers which are synthesized by the compiler
            // as part of the language implementation
            && !i.symbol.flags.is(Flags.Artifact)
            // Don't trace "magic" identifiers with '$'s in them
            && !name.toString.contains('$') =>

            tree.tpe.widen.asType match
              case '[t] => wrapWithLoggedValue[t](tree.asExpr, logger)

          // Don't worry about multiple chained annotations for now...
          case Typed(_, tpt) =>
            tpt.tpe match {
              case AnnotatedType(underlying, annot) if annot.tpe =:= TypeRepr.of[utest.asserts.Show] =>
                underlying.widen.asType match
                  case '[t] => wrapWithLoggedValue[t](tree.asExpr, logger)
              case _ => super.transformTerm(tree)
            }

          // Don't recurse and trace the LHS of assignments
          case t@Assign(lhs, rhs) => Assign.copy(t)(lhs, super.transformTerm(rhs))

          case _ => super.transformTerm(tree)
        }
      }
    }

  private def wrapWithLoggedValue[T: Type](expr: Expr[Any], logger: Expr[TestValue => Unit])(using QuoteContext) = {
    val tpeString =
      try Type.show[T]
      catch
        case _ => Type.of[T].toString // Workaround lampepfl/dotty#8858
    expr match {
      case '{ $x: t } =>
        '{
          val tmp: t = $x
          $logger(TestValue(
            ${Expr(expr.show)},
            ${Expr(StringUtilHelpers.stripScalaCorePrefixes(tpeString))},
            tmp
          ))
          tmp
        }.asReflectTree
    }
  }

  private def makeAssertEntry[T](expr: Expr[T], code: String)(using QuoteContext, Type[T]) =
    def entryBody(logger: Expr[TestValue => Unit]) =
      tracingMap(logger).transformTerm(expr.asReflectTree).asExprOf[T]
    '{AssertEntry(
      ${Expr(code)},
      logger => ${entryBody('logger)})}
}

object StringUtilHelpers {
  def stripScalaCorePrefixes(tpeName: String): String = {
    val pattern = """(?<!\.)(scala|java\.lang)(\.\w+)*\.(?<tpe>\w+)""".r // Match everything under the core `scala` or `java.lang` packages
    pattern.replaceAllIn(tpeName, _.group("tpe"))
  }

  extension (str: String) def trim: String =
    str.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse
}
