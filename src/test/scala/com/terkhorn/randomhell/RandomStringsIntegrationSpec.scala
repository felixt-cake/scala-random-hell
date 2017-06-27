package com.terkhorn.randomhell


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
    result.length mustEqual 29 // 5 lines * 5 chars + 4 newlines
    result.contains("hello") mustNot be( true)
  }

  it must "randomize value declarations" in {
    @RandomStrings
    def foo() = {
      val s = "foo"
      s
    }

    val result = foo()
    println(result)
    result mustNot equal("foo")
    result.length mustEqual 3
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
