// This test file has been translated from swift/test/Parse/trailing_closures.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class TrailingClosuresTests extends AstSwiftSrc2CpgSuite {

  "TrailingClosuresTests" should {

    "testTrailingClosures1" ignore {
      val cpg = code("func foo<T, U>(a: () -> T, b: () -> U) {}")
      ???
    }

    "testTrailingClosures2" ignore {
      val cpg = code("""
        |foo { 42 }
        |b: { "" }
        |""".stripMargin)
      ???
    }

    "testTrailingClosures3" ignore {
      val cpg = code("""foo { 42 } b: { "" }""")
      ???
    }

    "testTrailingClosures5" ignore {
      val cpg = code("let _ = when (2 < 3) { 3 } else: { 4 }")
      ???
    }

    "testTrailingClosures7" ignore {
      val cpg = code("""
        |let _: S = .foo {
        |  $0 = $0 + 1
        |}
        |""".stripMargin)
      ???
    }

    "testTrailingClosures8" ignore {
      val cpg = code("let _: S = .foo {} b: { $0 = $0 + 1 }")
      ???
    }

    "testTrailingClosures11" ignore {
      val cpg = code("multiple_trailing_with_defaults(duration: 42) {}")
      ???
    }

    "testTrailingClosures12" ignore {
      val cpg = code("multiple_trailing_with_defaults(duration: 42) {} completion: {}")
      ???
    }

    "testTrailingClosures13a" ignore {
      val cpg = code("""
        |fn {} g: {}
        |fn {} _: {}
        |multiple {} _: { }
        |mixed_args_1 {} _: {}
        |mixed_args_1 {} a: {}  //  {{none}}
        |mixed_args_2 {} a: {} _: {}
        |mixed_args_2 {} _: {} //  {{none}}
        |mixed_args_2 {} _: {} _: {} //  {{none}}
        |""".stripMargin)
      ???
    }

    "testTrailingClosures15" ignore {
      val cpg = code("func f() -> Int { 42 }")
      ???
    }

  }

}
