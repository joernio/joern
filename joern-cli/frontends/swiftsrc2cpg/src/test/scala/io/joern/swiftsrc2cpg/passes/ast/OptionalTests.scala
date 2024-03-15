// This test file has been translated from swift/test/Parse/optional.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class OptionalTests extends AstSwiftSrc2CpgSuite {

  "OptionalTests" should {

    "testOptional3a" ignore {
      val cpg = code("var c = a?")
      ???
    }

    "testOptional3b" ignore {
      val cpg = code("var d : ()? = a?.foo()")
      ???
    }

    "testOptional4" ignore {
      val cpg = code("""
        |var e : (() -> A)?
        |var f = e?()
        |""".stripMargin)
      ???
    }

    "testOptional5" ignore {
      val cpg = code("""
        |struct B<T> {}
        |var g = B<A?>()
        |""".stripMargin)
      ???
    }
  }

}
