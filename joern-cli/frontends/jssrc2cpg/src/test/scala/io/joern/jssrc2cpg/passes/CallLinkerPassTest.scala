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

    "link exported anonymous functions across file boundaries correctly" in {
      val cpg: Cpg = code(
        """
          |const bar = require('./bar.js');
          |const baz = require('./baz.js');
          |
          |bar.sayhi();
          |baz.sayhowdy();
          |""".stripMargin,
        "foo.js"
      ).moreCode(
        """
          |module.exports = {
          |  sayhi: function() {            // this will be called anonymous
          |    console.log("Hello World!");
          |  },
          |  saybye: function() {           // this will be called anonymous1
          |    console.log("Good-bye!");
          |  }
          |}
          |""".stripMargin,
        "bar.js"
      ).moreCode(
        """
          |module.exports = {
          |  sayhowdy: function() {       // this will be called anonymous
          |    console.log("Howdy World!");
          |  }
          |}
          |""".stripMargin,
        "baz.js"
      )

      inside(cpg.method.fullNameExact("bar.js::program:anonymous").l) { case List(m) =>
        m.name shouldBe "anonymous"
        m.fullName shouldBe "bar.js::program:anonymous"
      }

      inside(cpg.method.fullNameExact("bar.js::program:anonymous").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "bar.sayhi()"
        call.methodFullName shouldBe "bar.js::program:anonymous"
      }

      inside(cpg.method.fullNameExact("baz.js::program:anonymous").l) { case List(m) =>
        m.name shouldBe "anonymous"
        m.fullName shouldBe "baz.js::program:anonymous"
      }

      inside(cpg.method.fullNameExact("baz.js::program:anonymous").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "baz.sayhowdy()"
        call.methodFullName shouldBe "baz.js::program:anonymous"
      }
    }
  }

}
