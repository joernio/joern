package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.*

class CallLinkerPassTests extends DataFlowCodeToCpgSuite {

  "CallLinkerPass" should {

    "create call edges correctly for methods from classes" in {
      val cpg: Cpg = code("""
        |class Foo {
        |  a() {
        |    this.b();
        |  }
        |
        |  b() {
        |    console.log("b");
        |    new this.bar().c();
        |  }
        |
        |  bar = class Bar {
        |    c() {
        |      console.log("c");
        |    }
        |  }
        |}""".stripMargin)

      inside(cpg.method("b").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "this.b()"
        call.methodFullName should endWith(".js::program:Foo:b")
      }

      inside(cpg.method("c").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "new this.bar().c()"
        call.methodFullName should endWith(".js::program:Foo:Bar:c")
      }
    }

    "create call edges correctly" in {
      val cpg: Cpg = code("""
        |function sayhi() {
        |  console.log("Hello World!");
        |}
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

    "link exported <lambda> functions across file boundaries correctly" in {
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
          |  sayhi: function() {
          |    console.log("Hello World!");
          |  },
          |  saybye: function() {
          |    console.log("Good-bye!");
          |  }
          |}
          |""".stripMargin,
        "bar.js"
      ).moreCode(
        """
          |module.exports = {
          |  sayhowdy: function() {
          |    console.log("Howdy World!");
          |  }
          |}
          |""".stripMargin,
        "baz.js"
      )

      inside(cpg.method.fullNameExact("bar.js::program:sayhi").l) { case List(m) =>
        m.name shouldBe "sayhi"
        m.fullName shouldBe "bar.js::program:sayhi"
      }

      inside(cpg.method.fullNameExact("bar.js::program:sayhi").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "bar.sayhi()"
        call.methodFullName shouldBe "bar.js::program:sayhi"
        inside(call.expressionDown.isIdentifier.l) { case List(receiver: Identifier) =>
          receiver.name shouldBe "bar"
          receiver.typeFullName shouldBe "bar.js::program"
        }
      }

      inside(cpg.method.fullNameExact("baz.js::program:sayhowdy").l) { case List(m) =>
        m.name shouldBe "sayhowdy"
        m.fullName shouldBe "baz.js::program:sayhowdy"
      }

      inside(cpg.method.fullNameExact("baz.js::program:sayhowdy").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "baz.sayhowdy()"
        call.methodFullName shouldBe "baz.js::program:sayhowdy"
        inside(call.expressionDown.isIdentifier.l) { case List(receiver: Identifier) =>
          receiver.name shouldBe "baz"
          receiver.typeFullName shouldBe "baz.js::program"
        }
      }
    }

    "link all calls in a conservative, sound, and flow-insensitive manner" in {
      val cpg: Cpg = code(
        """
          |var barOrBaz = require('./bar.js');
          |barOrBaz = require('./baz.js');
          |
          |barOrBaz.sayhi();
          |""".stripMargin,
        "foo.js"
      ).moreCode(
        """
          |module.exports = {
          |  sayhi: function() {
          |    console.log("Hello World, love BAR");
          |  }
          |}
          |""".stripMargin,
        "bar.js"
      ).moreCode(
        """
          |module.exports = {
          |  sayhi: function() {
          |    console.log("Howdy World, love BAZ");
          |  }
          |}
          |""".stripMargin,
        "baz.js"
      )

      // Because of flow-insensitivity, it could point to either "sayhi"
      inside(cpg.call("sayhi").callee(NoResolve).l) { case List(m1, m2) =>
        m1.fullName shouldBe "bar.js::program:sayhi"
        m2.fullName shouldBe "baz.js::program:sayhi"
      }

      inside(cpg.method.fullNameExact("bar.js::program:sayhi").l) { case List(m) =>
        m.name shouldBe "sayhi"
        m.fullName shouldBe "bar.js::program:sayhi"
      }

      inside(cpg.method.fullNameExact("bar.js::program:sayhi").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "barOrBaz.sayhi()"
        call.methodFullName shouldBe "<unknownFullName>"
        inside(call.expressionDown.isIdentifier.l) { case List(receiver: Identifier) =>
          receiver.name shouldBe "barOrBaz"
          receiver.typeFullName shouldBe "ANY"
        }
      }

      inside(cpg.method.fullNameExact("baz.js::program:sayhi").l) { case List(m) =>
        m.name shouldBe "sayhi"
        m.fullName shouldBe "baz.js::program:sayhi"
      }

      inside(cpg.method.fullNameExact("baz.js::program:sayhi").callIn(NoResolve).l) { case List(call) =>
        call.code shouldBe "barOrBaz.sayhi()"
        call.methodFullName shouldBe "<unknownFullName>"
        inside(call.expressionDown.isIdentifier.l) { case List(receiver: Identifier) =>
          receiver.name shouldBe "barOrBaz"
          receiver.typeFullName shouldBe "ANY"
        }
      }
    }

  }

}
