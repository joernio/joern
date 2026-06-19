package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

class VarLetTupleTests extends SwiftSrc2CpgSuite {

  "VarLetTupleTests" should {

    "testLetTupleBasic" in {
      val cpg = code("""
      |func test() {
      |  let (a, b) = foo()
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // <tmp>0, a, b all live in the method block
      methodBlock.local.nameNot("self").name.sorted shouldBe Seq("<tmp>0", "a", "b")

      // The 3 desugared assignments are wrapped in a block
      val List(varDeclBlock) = methodBlock.astChildren.isBlock.l

      // <tmp>0 = foo()
      val List(tmpAssign) =
        varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("<tmp>0 = foo()").l
      tmpAssign.argument(1).code shouldBe "<tmp>0"
      tmpAssign.argument(2).code shouldBe "foo()"

      // a = <tmp>0.0  — rhs is a field access: <tmp>0.<field>0
      val List(aAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("a = <tmp>0.0").l
      aAssign.argument(1).code shouldBe "a"
      val List(aFieldAccess) = aAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      aFieldAccess.argument(1).code shouldBe "<tmp>0"
      aFieldAccess.argument(2).code shouldBe "0"

      // b = <tmp>0.1  — rhs is a field access: <tmp>0.<field>1
      val List(bAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("b = <tmp>0.1").l
      bAssign.argument(1).code shouldBe "b"
      val List(bFieldAccess) = bAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      bFieldAccess.argument(1).code shouldBe "<tmp>0"
      bFieldAccess.argument(2).code shouldBe "1"
    }

    "testVarTupleBasic" in {
      val cpg = code("""
      |func test() {
      |  var (a, b) = foo()
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      methodBlock.local.nameNot("self").name.sorted shouldBe Seq("<tmp>0", "a", "b")

      val List(varDeclBlock) = methodBlock.astChildren.isBlock.l

      val List(tmpAssign) =
        varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("<tmp>0 = foo()").l
      tmpAssign.argument(1).code shouldBe "<tmp>0"
      tmpAssign.argument(2).code shouldBe "foo()"

      val List(aAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("a = <tmp>0.0").l
      aAssign.argument(1).code shouldBe "a"
      val List(aFieldAccess) = aAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      aFieldAccess.argument(1).code shouldBe "<tmp>0"
      aFieldAccess.argument(2).code shouldBe "0"

      val List(bAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("b = <tmp>0.1").l
      bAssign.argument(1).code shouldBe "b"
      val List(bFieldAccess) = bAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      bFieldAccess.argument(1).code shouldBe "<tmp>0"
      bFieldAccess.argument(2).code shouldBe "1"
    }

    "testLetTupleWithWildcard" in {
      val cpg = code("""
      |func test() {
      |  let (a, _) = foo()
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // Only <tmp>0 and a — no local for the wildcard
      methodBlock.local.nameNot("self").name.sorted shouldBe Seq("<tmp>0", "a")

      val List(varDeclBlock) = methodBlock.astChildren.isBlock.l

      val List(aAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("a = <tmp>0.0").l
      aAssign.argument(1).code shouldBe "a"
      val List(aFieldAccess) = aAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      aFieldAccess.argument(1).code shouldBe "<tmp>0"
      aFieldAccess.argument(2).code shouldBe "0"

      // No assignment for the wildcard element
      varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("_ = <tmp>0.1").l shouldBe empty
    }

    "testLetTupleNested" in {
      val cpg = code("""
      |func test() {
      |  let (a, (b, c)) = foo()
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      methodBlock.local.nameNot("self").name.sorted shouldBe Seq("<tmp>0", "a", "b", "c")

      val List(varDeclBlock) = methodBlock.astChildren.isBlock.l

      // a = <tmp>0.0
      val List(aAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("a = <tmp>0.0").l
      aAssign.argument(1).code shouldBe "a"
      val List(aFieldAccess) = aAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      aFieldAccess.argument(1).code shouldBe "<tmp>0"
      aFieldAccess.argument(2).code shouldBe "0"

      // b = <tmp>0.1.0 — nested field access: (<tmp>0.1).0
      val List(bAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("b = <tmp>0.1.0").l
      bAssign.argument(1).code shouldBe "b"
      val List(bOuterAccess) = bAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      bOuterAccess.code shouldBe "<tmp>0.1.0"
      bOuterAccess.argument(2).code shouldBe "0"
      val List(bInnerAccess) = bOuterAccess.arguments(1).isCall.nameExact(Operators.fieldAccess).l
      bInnerAccess.argument(1).code shouldBe "<tmp>0"
      bInnerAccess.argument(2).code shouldBe "1"

      // c = <tmp>0.1.1
      val List(cAssign) = varDeclBlock.astChildren.isCall.nameExact(Operators.assignment).codeExact("c = <tmp>0.1.1").l
      cAssign.argument(1).code shouldBe "c"
      val List(cOuterAccess) = cAssign.arguments(2).isCall.nameExact(Operators.fieldAccess).l
      cOuterAccess.code shouldBe "<tmp>0.1.1"
      cOuterAccess.argument(2).code shouldBe "1"
      val List(cInnerAccess) = cOuterAccess.arguments(1).isCall.nameExact(Operators.fieldAccess).l
      cInnerAccess.argument(1).code shouldBe "<tmp>0"
      cInnerAccess.argument(2).code shouldBe "1"
    }

    "testLetTupleNoInitializer" in {
      val cpg = code("""
      |func test() {
      |  var (a, b): (Int, String)
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // All 2 leaf locals declared, no tmp, no assignments
      methodBlock.local.name.sorted shouldBe Seq("a", "b")
      methodBlock.ast.isCall.nameExact(Operators.assignment).l shouldBe List.empty
    }

    "testLetTupleNoInitializerNested" in {
      val cpg = code("""
      |func test() {
      |  var (a, (b, c)): (Int, (Bool, String))
      |}
      |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("test").block.l
      // All 3 leaf locals declared, no tmp, no assignments
      methodBlock.local.name.sorted shouldBe Seq("a", "b", "c")
      methodBlock.ast.isCall.nameExact(Operators.assignment).l shouldBe List.empty
    }

  }
}
