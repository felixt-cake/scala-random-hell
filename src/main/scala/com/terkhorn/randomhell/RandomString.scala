package com.terkhorn.randomhell

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * before:
  *
  * def foo(z: Int) = { val s = "hello"; Range(0,z).map(_ => s).mkstring("\n") }
  *
  * potentially after:
  *
  * def foo(z: Int) = { val s = "gxzha"; Range(0,z).map(_ => s).mkstring("\n") }
  */
class RandomString extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case defn: Defn.Def =>
        defn
      case other => abort(other.pos, "@RandomString must annotate a method.")
    }
  }

}
