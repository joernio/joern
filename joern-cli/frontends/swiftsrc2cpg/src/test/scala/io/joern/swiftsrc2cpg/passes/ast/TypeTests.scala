package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TypeTests extends AbstractPassTest {

  "TypeTests" should {

    "testClosureParsing" ignore AstFixture("""
        |let a: (a, b) -> c
        |let a: @MainActor (a, b) async throws -> c
        |() -> (\u{feff})
        |""".stripMargin) { cpg => ??? }

    "testClosureSignatures" ignore AstFixture("""
        |simple { [] str in
        |  print("closure with empty capture list")
        |}
        |{ ()
        |  throws -> Void in }
        |{ [weak a, unowned(safe) self, b = 3] (a: Int, b: Int, _: Int) -> Int in }
        |""".stripMargin) { cpg => ??? }

    "testOpaqueReturnTypes" ignore AstFixture(
      """public typealias Body = @_opaqueReturnTypeOf("$s6CatKit10pspspspspsV5cmereV6lilguyQrvp", 0) __"""
    ) { cpg => ??? }

    "testVariadics" ignore AstFixture("""
        |func takesVariadicFnWithGenericRet<T>(_ fn: (S...) -> T) {}
        |let _: (S...) -> Int = \.i
        |let _: (S...) -> Int = \Array.i1
        |let _: (S...) -> Int = \S.i2
        |""".stripMargin) { cpg => ??? }

  }

}
