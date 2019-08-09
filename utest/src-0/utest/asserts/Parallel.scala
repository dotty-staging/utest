package utest
package asserts

import scala.concurrent.duration._
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * Asserts which only make sense when running on multiple threads.
 */
object Parallel extends ParallelCommons {
  def eventuallyProxy(exprs: Expr[Seq[Boolean]]) given (ctx: QuoteContext): Expr[Unit] = {
    val res = Tracer[Boolean]('{ (esx: Seq[AssertEntry[Boolean]]) => utest.asserts.Parallel.eventuallyImpl(esx: _*) }, exprs)
    '{$res: Unit}
  }

  def continuallyProxy(exprs: Expr[Seq[Boolean]]) given (ctx: QuoteContext): Expr[Unit] = {
    val res = Tracer[Boolean]('{ (esx: Seq[AssertEntry[Boolean]]) => utest.asserts.Parallel.continuallyImpl(esx: _*) }, exprs)
    '{$res: Unit}
  }
}

