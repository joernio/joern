package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class StatementTests extends SwiftSrc2CpgSuite {

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

    "testMissingIfClauseIntroducer" in {
      // Invalid Swift (`if _ = 42`); the parser still recovers an IF control structure.
      val cpg          = code("if _ = 42 {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if")
    }

    "testIfHasSymbol" in {
      val cpg = code("""
      |if #_hasSymbol(foo) {}
      |if #_hasSymbol(foo as () -> ()) {}
      |""".stripMargin)
      // `#_hasSymbol` is an availability-style predicate. The IF control structures must still be created.
      val ifs = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifs.code.l shouldBe List("if #_hasSymbol(foo) {}", "if #_hasSymbol(foo as () -> ()) {}")
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

    "testTrailingClosureInIfCondition" in {
      // Edge case the parser disambiguates: `if test { $0 } {}` — `{ $0 }` is a closure
      // argument to `test`, the outer `{}` is the IF body. We just lock in that an IF survives.
      val cpg          = code("if test { $0 } {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if test")
    }

    "testClosureInsideIfCondition" in {
      // Invalid Swift (`if true, {x}() {}`). Parser recovers; assert the IF survives.
      val cpg          = code("if true, {x}() {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if true")
    }

  }

}
