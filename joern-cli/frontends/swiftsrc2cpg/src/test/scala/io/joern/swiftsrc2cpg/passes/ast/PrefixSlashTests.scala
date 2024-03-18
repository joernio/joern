package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class PrefixSlashTests extends AstSwiftSrc2CpgSuite {

  "PrefixSlashTests" should {

    "testPrefixSlash2" ignore {
      val cpg = code("""
        |prefix operator /
        |prefix func / <T> (_ x: T) -> T { x }
        |""".stripMargin)
      ???
    }

    "testPrefixSlash4" ignore {
      val cpg = code("""
        |prefix operator /
        |_ = /E.e
        |(/E.e).foo(/0)
        |""".stripMargin)
      ???
    }

    "testPrefixSlash6" ignore {
      val cpg = code("""
        |prefix operator /
        |foo(/E.e, /E.e)
        |foo((/E.e), /E.e)
        |foo((/)(E.e), /E.e)
        |""".stripMargin)
      ???
    }

    "testPrefixSlash8" ignore {
      val cpg = code("""
        |prefix operator /
        |_ = bar(/E.e) / 2
        |""".stripMargin)
      ???
    }

  }

}
