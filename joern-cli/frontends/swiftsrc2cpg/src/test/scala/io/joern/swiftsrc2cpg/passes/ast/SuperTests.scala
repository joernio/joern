// This test file has been translated from swift/test/Parse/super.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class SuperTests extends AstSwiftSrc2CpgSuite {

  "SuperTests" should {

    "testSuper2a" ignore {
      val cpg = code("""
        |class D : B {
        |  override init() {
        |    super.init()
        |    super.init(42)
        |  }
        |}
        |""".stripMargin)
      ???
    }

    "testSuper2c" ignore {
      val cpg = code("""
        |class D : B {
        |  convenience init(y:Int) {
        |    let _: () -> D = self.init
        |  }
        |}""".stripMargin)
      ???
    }

    "testSuper2d" ignore {
      val cpg = code("""
        |class D : B {
        |  init(z: Int) {
        |    super
        |      .init(x: z)
        |  }
        |}""".stripMargin)
      ???
    }

  }

}
