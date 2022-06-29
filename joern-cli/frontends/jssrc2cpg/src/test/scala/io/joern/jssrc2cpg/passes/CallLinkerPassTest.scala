package io.joern.jssrc2cpg.passes

import io.shiftleft.semanticcpg.language._

class CallLinkerPassTest extends AbstractPassTest {

  "CallLinkerPass" should {

    "create call edges correctly" in AstFixture("""
        |function sayhi() {
        |  console.log("Hello World!");
        |}
        |
        |sayhi();
        |""".stripMargin) { cpg =>
      inside(cpg.method("sayhi").l) { case List(m) =>
        m.name shouldBe "sayhi"
        m.fullName should endWith(".js::program:sayhi")
      }

      inside(cpg.method("sayhi").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "sayhi()"
      }
    }
  }

}
