package com.terkhorn.randomhell

import scala.meta._
import scala.meta.testkit._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest.FunSuite

// If you are doing complicated macro expansions, it's recommeded to unit test
// the trickiest bits instead of relying only on integration tests.
class MainUnitTest extends FunSuite {

  // TODO(olafur) this method should be exposed in testkit
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
    val obtained = ???
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
