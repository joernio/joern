package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class StatementTests extends SwiftSrc2CpgSuite {

  "StatementTests" should {

    "testIf" in {
      val cpg = code("""
      |if let baz {}
      |if let self = self {}
      |""".stripMargin)
      val ifs = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).code.l
      ifs shouldBe List("if let baz {}", "if let self = self {}")
      cpg.local.name.l should contain allOf ("baz", "self")
      cpg.call.codeExact("let self = self").l should not be empty
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

    "testReturn" in {
      val cpg = code("""
      |return actor
      |{ return 0 }
      |return
      |""".stripMargin)
      // Top-level `return` statements + a closure literal yielding `return 0`. We expect
      // a `<lambda>0` method to be created for the closure body; the bare `{ return 0 }`
      // closure is wrapped via `single_apply`.
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.<lambda>0:()->ANY",
        "Swift.Function<()->ANY>.single_apply:()->ANY"
      )
      val returnCodes = cpg.method.ast.isReturn.code.l
      // The `return actor` line is followed by a closure literal `{ return 0 }` on the next
      // line — Swift parses the trailing closure as the return expression, so the outer
      // return's code spans both lines.
      returnCodes should contain allOf ("return actor\n{ return 0 }", "return 0", "return")
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

    "testYield" in {
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
      // The `_read` accessor on `var x` is materialized as a method named `_read` whose
      // qualifier is the property name `x`, returning Swift.Int. `func f` is a sibling.
      cpg.method.fullName.l should contain allOf (
        "Test0.swift:<global>.x._read:Swift.Int",
        "Test0.swift:<global>.f:()->Swift.Int"
      )
      // `yield &x` lowers to an addressOf operator; the `&x` argument shows up as a call.
      cpg.call.nameExact(Operators.addressOf).code.l should contain("&x")
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
