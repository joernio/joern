package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class TypeTests extends AstSwiftSrc2CpgSuite {

  "TypeTests" should {

    "testClosureParsing" ignore {
      val cpg = code("""
        |let a: (a, b) -> c
        |let a: @MainActor (a, b) async throws -> c
        |() -> (\u{feff})
        |""".stripMargin)
      ???
    }

    "testClosureSignatures" ignore {
      val cpg = code("""
        |simple { [] str in
        |  print("closure with empty capture list")
        |}
        |{ ()
        |  throws -> Void in }
        |{ [weak a, unowned(safe) self, b = 3] (a: Int, b: Int, _: Int) -> Int in }
        |""".stripMargin)
      ???
    }

    "testOpaqueReturnTypes" ignore {
      val cpg =
        code("""public typealias Body = @_opaqueReturnTypeOf("$s6CatKit10pspspspspsV5cmereV6lilguyQrvp", 0) __""")
      ???
    }

    "testVariadics" ignore {
      val cpg = code("""
        |func takesVariadicFnWithGenericRet<T>(_ fn: (S...) -> T) {}
        |let _: (S...) -> Int = \.i
        |let _: (S...) -> Int = \Array.i1
        |let _: (S...) -> Int = \S.i2
        |""".stripMargin)
      ???
    }

  }

}
