package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Inside

class CallLinkerPassTest extends AnyWordSpec with Matchers with Inside {

  "CallLinkerPass" should {

    "create call edges correctly" in {
      val code = """
       |function sayhi() {
       |  console.log("Hello World!");
       |}
       |
       |sayhi();
       |""".stripMargin

      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val file = dir / "code.js"
        file.write(code)
        file.deleteOnExit()
        val cpg = new JsSrc2CpgFrontend().execute(dir.toJava)

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

}
