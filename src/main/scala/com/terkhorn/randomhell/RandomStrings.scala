package com.terkhorn.randomhell

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.util.Random

/**
  * before:
  *
  * def foo(z: Int) = { val s = "hello"; Range(0,z).map(_ => s).mkstring("\n") }
  *
  * potentially after:
  *
  * def foo(z: Int) = { val s = "gxzha"; Range(0,z).map(_ => s).mkstring("\n") }
  */
class RandomStrings extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case fn: Defn.Def => fn.copy(body = RandomStrings.randomizeTerm(fn.body))
      case Term.Block(stats) => Term.Block(stats.map(c=>RandomStrings.randomizeStat(c)))
      case Term.Return(expr) => Term.Return(RandomStrings.randomizeTerm(expr))
      case other => abort(other.pos, "@RandomString must annotate a method.")
    }
  }
}

object RandomStrings {
  private [randomhell] val rnd = new Random

  private [randomhell] def randomizeStat(stat: Stat): Stat = stat match {
    case t: Term => randomizeTerm(t)
    case other => other
  }

  private  [randomhell] def randomizeTerm(expr: Term): Term = expr match {
    case Lit(s: String) => Lit.String(RandomStrings.rnd.alphanumeric.take(s.length).mkString)
    case Term.Block(stats) => Term.Block(stats.map(c=>RandomStrings.randomizeStat(c)))
    case other => other
  }


}
