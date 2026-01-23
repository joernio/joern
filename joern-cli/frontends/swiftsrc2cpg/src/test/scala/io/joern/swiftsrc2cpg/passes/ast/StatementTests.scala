package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class StatementTests extends AstSwiftSrc2CpgSuite {

  "StatementTests" should {

    "testIf" ignore {
      val cpg = code("""
      |if let baz {}
      |if let self = self {}
      |""".stripMargin)
      ???
    }

    "testDoCatch" in {
      val cpg = code("""
      |do {
      |  try foo()
      |} catch {
      |  bar()
      |}
      |do { try foo() }
      |catch where (error as NSError) == NSError() {}
      |""".stripMargin)
      val List(doStructure1, doStructure2) =
        cpg.controlStructure.controlStructureType(ControlStructureTypes.TRY).code("do \\{.*").l

      val List(fooCall1) = doStructure1.astChildren.order(1).isCall.l
      fooCall1.code shouldBe "foo()"
      val List(catchBlock1) = doStructure1.astChildren.isControlStructure.isCatch.l
      catchBlock1.order shouldBe 2
      catchBlock1.astChildren.isCall.codeExact("bar()").size shouldBe 1

      val List(fooCall2) = doStructure2.astChildren.order(1).isCall.l
      fooCall2.code shouldBe "foo()"
      val List(catchBlock2) = doStructure2.astChildren.isControlStructure.isCatch.l
      catchBlock2.order shouldBe 2
      catchBlock2.astChildren.isCall.code.l shouldBe List("(error as NSError) == NSError()")
    }

    "testReturn" ignore {
      val cpg = code("""
      |return actor
      |{ return 0 }
      |return
      |""".stripMargin)
      ???
    }

    "testMissingIfClauseIntroducer" ignore {
      val cpg = code("if _ = 42 {}")
      ???
    }

    "testIfHasSymbol" ignore {
      val cpg = code("""
      |if #_hasSymbol(foo) {}
      |if #_hasSymbol(foo as () -> ()) {}
      |""".stripMargin)
      ???
    }

    "testYield" ignore {
      val cpg = code("""
      |var x: Int {
      |  _read {
      |    yield &x
      |  }
      |}
      |func f() -> Int {
      |  yield 5
      |}
      |""".stripMargin)
      ???
    }

    "testTrailingClosureInIfCondition" ignore {
      val cpg = code("if test { $0 } {}")
      ???
    }

    "testClosureInsideIfCondition" ignore {
      val cpg = code("if true, {x}() {}")
      ???
    }

  }

}
