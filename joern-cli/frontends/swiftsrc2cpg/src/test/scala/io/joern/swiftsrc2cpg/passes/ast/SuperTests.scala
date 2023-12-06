// This test file has been translated from swift/test/Parse/super.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SuperTests extends AbstractPassTest {

  "SuperTests" should {

    "testSuper2a" ignore AstFixture("""
        |class D : B {
        |  override init() {
        |    super.init()
        |    super.init(42)
        |  }
        |}
        |""".stripMargin) { cpg => ??? }

    "testSuper2c" ignore AstFixture("""
        |class D : B {
        |  convenience init(y:Int) {
        |    let _: () -> D = self.init
        |  }
        |}""".stripMargin) { cpg => ??? }

    "testSuper2d" ignore AstFixture("""
        |class D : B {
        |  init(z: Int) {
        |    super
        |      .init(x: z)
        |  }
        |}""".stripMargin) { cpg => ??? }

  }

}
