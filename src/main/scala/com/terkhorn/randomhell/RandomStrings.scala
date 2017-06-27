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

object Randomize {
  private val rnd = new Random
}

trait Randomize {
  def randomize(s: String): String = Randomize.rnd.alphanumeric.take(s.length).mkString
}

object RandomStrings {


  private[randomhell] def expandDef(fn: Defn.Def)(implicit rnd: Randomize = new Randomize {}) =
    fn.copy(body = RandomStrings.expandTerm(fn.body))

  private def expandStat(stat: Stat)(implicit rnd: Randomize = new Randomize {}): Stat = stat match {
    case t: Term => expandTerm(t)
    case q"val ${Pat.Var.Term(n)} = $v" =>
      q"val ${Pat.Var.Term(n)} = ${expandTerm(v)}"
    case q"var ${Pat.Var.Term(n)} = ${Some(v)}" =>
      q"var ${Pat.Var.Term(n)} = ${expandTerm(v)}"
    case fn: Defn.Def => fn.copy(body = RandomStrings.expandTerm(fn.body))
    case other => other
  }

  private[randomhell] def expandTerm(expr: Term)(implicit rnd: Randomize = new Randomize {}): Term = expr match {
    case Lit(s: String) => Lit.String(rnd.randomize(s))
    case Term.Block(stats) => Term.Block(stats.map(c => RandomStrings.expandStat(c)))
    case Term.Return(e) => Term.Return(RandomStrings.expandTerm(e))
    case other => other
  }


}
