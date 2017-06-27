package com.terkhorn.randomhell

import scala.meta._
import scala.meta.testkit._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest.FunSuite


class RandomStringsSpec extends FunSuite {

  def assertStructurallyEqual(obtained: Tree, expected: Tree): Unit = {
    StructurallyEqual(obtained, expected) match {
      case Left(AnyDiff(x, y)) =>
        fail(s"""Not Structurally equal!:
                |obtained: $x
                |expected: $y
             """.stripMargin)
      case _ =>
    }
  }

  def testRnd(s: String): String = Seq.fill(s.length)('a').mkString

  test("@RandomStrings randomizes simple functions") {
    val obtained =
      RandomStrings.expandDef(q"""
        def foo(n: Int) = {
          val s = "hello"
          Range(0,z).map(_ => s).mkString("\n")
        }
       """, testRnd)
    val expected =
      q"""
        def foo(n: Int) = {
          val s = "aaaaa"
          Range(0,z).map(_ => s).mkString("\n")
        }
       """
    assertStructurallyEqual(obtained, expected)
  }
}
