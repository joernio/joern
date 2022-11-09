package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

class CallLinkerPassTest extends DataFlowCodeToCpgSuite {

  "CallLinkerPass" should {

    "create call edges correctly" in {
      val cpg: Cpg = code("""
        |function sayhi() {
        |  console.log("Hello World!");
        |}
        |
        |sayhi();
        |""".stripMargin)

      inside(cpg.method("sayhi").l) { case List(m) =>
        m.name shouldBe "sayhi"
        m.fullName should endWith(".js::program:sayhi")
      }

      inside(cpg.method("sayhi").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "sayhi()"
        call.methodFullName should endWith(".js::program:sayhi")
      }
    }

  }

}
