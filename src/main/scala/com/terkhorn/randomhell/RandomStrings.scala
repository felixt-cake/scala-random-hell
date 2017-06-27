package com.terkhorn.randomhell

import scala.annotation.StaticAnnotation
import scala.meta.Pat.Var
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
      case fn: Defn.Def => fn.copy(body = RandomStrings.expandTerm(fn.body))
      case t: Term => RandomStrings.expandTerm(t)
      case other => abort(other.pos, "@RandomStrings must annotate a method.")
    }
  }
}

object RandomStrings {
  private val rnd = new Random

  private def randomize(s: String) = rnd.alphanumeric.take(s.length).mkString

  private def expandStat(stat: Stat): Stat = stat match {
    case t: Term => expandTerm(t)
    case d @ Decl.Val(_,pats,_) =>  d.copy(pats=pats.map(expandPat))
    /*case q"val $n = $v" =>
      val name: Term.Name = n.name
      q"val ${Pat.Var.Term(Term.Name(name.value))} = $v"*/
    case other => other
  }

  private def expandPat(pat: Pat.Var.Term): Pat.Var.Term = pat

  private  [randomhell] def expandTerm(expr: Term): Term = expr match {
    case Lit(s: String) => Lit.String(randomize(s))
    case Term.Block(stats) => Term.Block(stats.map(c=>RandomStrings.expandStat(c)))
    case Term.Return(e) => Term.Return(RandomStrings.expandTerm(e))
    case other => other
  }


}
