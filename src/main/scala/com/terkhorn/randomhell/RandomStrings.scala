package com.terkhorn.randomhell

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.util.Random

/**
  * before:
  *
  * def foo(z: Int) = { val s = "hello"; Range(0,z).map(_ => s).mkString("\n") }
  *
  * potentially after:
  *
  * def foo(z: Int) = { val s = "gxzha"; Range(0,z).map(_ => s).mkString("\n") }
  */
class RandomStrings extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case fn: Defn.Def => RandomStrings.expandDef(fn)
      case t: Term => RandomStrings.expandTerm(t)
      case other => abort(other.pos, "@RandomStrings must annotate a method.")
    }
  }
}

object RandomStrings {
  private val rnd = new Random

  private def randomize(s: String) = rnd.alphanumeric.take(s.length).mkString

  private [randomhell] def expandDef(fn: Defn.Def, rnd: String => String = randomize) =
    fn.copy(body = RandomStrings.expandTerm(fn.body, rnd))

  private def expandStat(stat: Stat, rnd: String => String = randomize): Stat = stat match {
    case t: Term => expandTerm(t)
    case q"val ${Pat.Var.Term(n)} = $v" =>
      q"val ${Pat.Var.Term(n)} = ${expandTerm(v, rnd)}"
    case q"var ${Pat.Var.Term(n)} = ${Some(v)}"  =>
      q"var ${Pat.Var.Term(n)} = ${expandTerm(v, rnd)}"
    case fn: Defn.Def => fn.copy(body = RandomStrings.expandTerm(fn.body, rnd))
    case other => other
  }

  private[randomhell] def expandTerm(expr: Term, rnd: String => String = randomize): Term = expr match {
    case Lit(s: String) => Lit.String(rnd(s))
    case Term.Block(stats) => Term.Block(stats.map(c => RandomStrings.expandStat(c, rnd)))
    case Term.Return(e) => Term.Return(RandomStrings.expandTerm(e, rnd))
    case other => other
  }


}
