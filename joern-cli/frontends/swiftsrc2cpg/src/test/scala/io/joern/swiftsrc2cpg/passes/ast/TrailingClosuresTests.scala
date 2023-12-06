// This test file has been translated from swift/test/Parse/trailing_closures.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TrailingClosuresTests extends AbstractPassTest {

  "TrailingClosuresTests" should {

    "testTrailingClosures1" ignore AstFixture("func foo<T, U>(a: () -> T, b: () -> U) {}") { cpg => ??? }

    "testTrailingClosures2" ignore AstFixture("""
        |foo { 42 }
        |b: { "" }
        |""".stripMargin) { cpg => ??? }

    "testTrailingClosures3" ignore AstFixture("""foo { 42 } b: { "" }""") { cpg => ??? }

    "testTrailingClosures5" ignore AstFixture("let _ = when (2 < 3) { 3 } else: { 4 }") { cpg => ??? }

    "testTrailingClosures7" ignore AstFixture("""
        |let _: S = .foo {
        |  $0 = $0 + 1
        |}
        |""".stripMargin) { cpg => ??? }

    "testTrailingClosures8" ignore AstFixture("let _: S = .foo {} b: { $0 = $0 + 1 }") { cpg => ??? }

    "testTrailingClosures11" ignore AstFixture("multiple_trailing_with_defaults(duration: 42) {}") { cpg => ??? }

    "testTrailingClosures12" ignore AstFixture("multiple_trailing_with_defaults(duration: 42) {} completion: {}") {
      cpg => ???
    }

    "testTrailingClosures13a" ignore AstFixture("""
        |fn {} g: {}
        |fn {} _: {}
        |multiple {} _: { }
        |mixed_args_1 {} _: {}
        |mixed_args_1 {} a: {}  //  {{none}}
        |mixed_args_2 {} a: {} _: {}
        |mixed_args_2 {} _: {} //  {{none}}
        |mixed_args_2 {} _: {} _: {} //  {{none}}
        |""".stripMargin) { cpg => ??? }

    "testTrailingClosures15" ignore AstFixture("func f() -> Int { 42 }") { cpg => ??? }

  }

}
