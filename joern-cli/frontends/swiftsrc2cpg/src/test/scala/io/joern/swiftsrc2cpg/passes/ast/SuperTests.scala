// This test file has been translated from swift/test/Parse/super.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.semanticcpg.language.*

class SuperTests extends AstSwiftSrc2CpgSuite {

  "SuperTests" should {

    "testSuper2a" in {
      val cpg = code("""
        |class D : B {
        |  override init() {
        |    super.init()
        |    super.init(42)
        |  }
        |}
        |""".stripMargin)
      cpg.typeDecl.nameExact("D").method.isConstructor.block.astChildren.isCall.code.sorted.l shouldBe List(
        "super.init()",
        "super.init(42)"
      )
    }

    "testSuper2c" in {
      val cpg = code("""
        |class D : B {
        |  convenience init(y:Int) {
        |    let _: () -> D = self.init
        |  }
        |}""".stripMargin)
      cpg.typeDecl.nameExact("D").method.isConstructor.block.astChildren.isCall.code.sorted.l shouldBe List(
        "let wildcard_0: () -> D = self.init"
      )
    }

    "testSuper2d" in {
      val cpg = code("""
        |class D : B {
        |  init(z: Int) {
        |    super
        |      .init(x: z)
        |  }
        |}""".stripMargin)
      cpg.typeDecl.nameExact("D").method.isConstructor.block.astChildren.isCall.code.sorted.l shouldBe List(
        "super\n      .init(x: z)"
      )
    }

  }

}
