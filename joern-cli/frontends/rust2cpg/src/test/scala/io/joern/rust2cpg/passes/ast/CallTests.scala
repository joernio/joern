package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class CallTests extends Rust2CpgSuite(noSysRoot = true) {

  "`let x = foo()`" should {
    val cpg = code("""
        |fn main() {
        | let x = foo();
        |}
        |""".stripMargin)

    "create a local for the binding" in {
      inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
        local.typeFullName shouldBe Defines.Any
        local.code shouldBe "x"
      }
    }

    "have the call as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: Call) :: Nil =>
        rhs.name shouldBe "foo"
        rhs.code shouldBe "foo()"
        rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::foo"
        rhs.typeFullName shouldBe Defines.Any
      }
    }
  }

  "an unresolved fully-qualified call" should {
    val cpg = code("""
        |fn main() {
        | a::b::c();
        |}
        |""".stripMargin)

    "preserve the full path in methodFullName" in {
      inside(cpg.call.name("c").l) { case cCall :: Nil =>
        cCall.code shouldBe "a::b::c()"
        cCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        cCall.argument shouldBe empty
        cCall.methodFullName shouldBe "a::b::c"
      }
    }
  }

  "an unresolved chained method call" should {
    val cpg = code("""
        |fn main() {
        | external().chain().tail();
        |}
        |""".stripMargin)

    "have DynamicCallUnknownFullName for the inner method call" in {
      cpg.call.nameExact("chain").methodFullName.l shouldBe List(Defines.DynamicCallUnknownFullName)
    }

    "have DynamicCallUnknownFullName for the outer method call" in {
      cpg.call.nameExact("tail").methodFullName.l shouldBe List(Defines.DynamicCallUnknownFullName)
    }

    "have an unresolved-namespace methodFullName for the function call" in {
      cpg.call.nameExact("external").methodFullName.l shouldBe List(s"${Defines.UnresolvedNamespace}::external")
    }
  }

  "a call to a function defined in the same file" should {
    val cpg = code("""
        |fn callee() {}
        |fn main() { callee(); }
        |""".stripMargin)

    "have a crate-prefixed methodFullName" in {
      cpg.call.name("callee").methodFullName.l shouldBe List("rust2cpgtest::callee")
    }
  }
}
