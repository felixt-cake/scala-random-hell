package com.terkhorn.randomhell

import scala.meta._
import scala.meta.testkit._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest.FunSuite

import org.scalatest.{FlatSpec, MustMatchers}

class RandomStringsIntegrationSpec extends FlatSpec with MustMatchers {
  "the @RandomStrings annotation" must "randomize assigned string values in functions" in {

    @RandomStrings
    def foo(z: Int) = {
      val s = "hello"
      Range(0,z).map(_ => s).mkString("\n")
    }

    val result = foo(5)
    println(result)
    // TODO

  }

  it must "randomize literal values in function blocks" in {
    @RandomStrings
    def foo(z: Int) = {
      "first expression thrown away"
      "hello"
    }

    val result = foo(5)
    println(result)
    result mustNot equal("hello")
    result.length mustEqual 5
  }

  it must "randomize returned values in functions " in {
    @RandomStrings
    def foo(z: Int) = {
      "hello"
    }

    val result = foo(5)
    println(result)
    result mustNot equal("hello")
    result.length mustEqual 5
  }
}


class MainUnitTest extends FunSuite {

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

  test("@Main creates a main method") {
    val obtained = MainMacroImpl.expand(q"AnswerToEverything",
      List(q"val x = 42", q"println(x)"))
    val expected =
      q"""
        object AnswerToEverything {
          def main(args: Array[String]): Unit = {
            val x = 42
            println(x)
          }
        }
       """
    assertStructurallyEqual(obtained, expected)
  }
}
