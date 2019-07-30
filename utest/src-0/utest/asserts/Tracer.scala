package utest
package asserts

import scala.quoted._
import delegate scala.quoted._
import scala.tasty._


/**
 * Macro implementation to take a block of code and trace through it,
 * converting it into an [[AssertEntry]] and inserting debug loggers.
 */
object Tracer{
  val wrapWithLoggedValue = given (ctx: QuoteContext) => (tree: ctx.tasty.Term, logger: Expr[TestValue => Unit], tpe: String) => {
    import ctx.tasty._
    '{
      val tmp = ${TermToQuotedAPI(tree).seal}
      $logger(TestValue(
        tree.toString,
        tpe,
        tmp
      ))
      tmp
    }
  }

  def apply[T](func: Expr[Seq[AssertEntry[T]] => Unit], exprs: Expr[Seq[T]]) given (ctx: QuoteContext): Expr[Unit] = {
    import ctx.tasty._

    def tracingTransformer(logger: Expr[TestValue => Unit]) = new TreeMap {
      override def transformTerm(tree: Term)(implicit ctx: Context): Term = {
        tree match {
          case i @ Ident(name) if i.symbol.pos.exists
            && i.pos.exists
            // only trace identifiers coming from the same file,
            // since those are the ones people probably care about
            && i.symbol.pos.sourceFile == i.pos.sourceFile
            // Don't trace methods, since you cannot just print them "standalone"
            // without providing arguments
            && !IsDefDefSymbol.unapply(i.symbol).isDefined && !i.symbol.isClassConstructor
            // Don't trace identifiers which are synthesized by the compiler
            // as part of the language implementation
            && !i.symbol.hasFlag(BRIDGE | VBRIDGE | ARTIFACT)
            // Don't trace "magic" identifiers with '$'s in them
            && !name.toString.contains('$') =>

            wrapWithLoggedValue(tree, logger, tree.tpe.widen)
         
          case i: Typed =>
            i.tpe match {
              case t: AnnotatedType
                // Don't worry about multiple chained annotations for now...
                if t.annotations.map(_.tpe) == Seq(typeOf[utest.asserts.Show]) =>
                wrapWithLoggedValue(tree, logger, t.underlying.widen)
              case _ => super.transformTerm(tree)
            }

          // Don't recurse and trace the LHS of assignments
          case i: Assign => super.transform(i.rhs)

          case _ => super.transformTerm(tree)
        }
      }
    }

    val trees: Expr[Seq[AssertEntry[T]]] = '{$exprs.map(expr =>
      AssertEntry(
        expr.show,
        logger => ${tracingTransformer('logger).transform(expr.tree)})
    )}

    '{$func($trees)}
  }
}
