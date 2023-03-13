package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class TypeRecoveryPassTests extends DataFlowCodeToCpgSuite {

  "literals declared from built-in types" should {
    lazy val cpg = code("""
        |let x = 123;
        |
        |function foo_shadowing() {
        |   let x = "foo";
        |}
        |
        |z = {'a': 123};
        |z = [1, 2, 3];
        |
        |z.push(4)
        |""".stripMargin)

    "resolve 'x' identifier types despite shadowing" in {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("__ecma.String", "__ecma.Number")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("__ecma.String", "__ecma.Number")
    }

    "resolve 'z' types correctly" in {
      // The dictionary/object type is just considered "ANY" which is fine for now
      cpg.identifier("z").typeFullName.toSet.headOption shouldBe Some("__ecma.Array")
    }

    "resolve 'z' identifier call correctly" in {
      val List(zAppend) = cpg.call("push").l
      zAppend.methodFullName shouldBe "__ecma.Array.push"
    }
  }

  "call from a function from an external type" should {

    lazy val cpg = code(
      """
        |import { WebClient } from "slack_sdk";
        |import { SendGridAPIClient } from "sendgrid";
        |
        |const client = new WebClient("WOLOLO");
        |const sg = new SendGridAPIClient("SENGRID_KEY_WOLOLO");
        |
        |function sendSlackMessage(chan, msg) {
        |    client.chatPostMessage(channel=chan, text=msg);
        |}
        |
        |let response = sg.send(message);
        |""".stripMargin, "test1.ts").cpg

    "resolve 'sg' identifier types from import information" in {
      val List(sgAssignment, sgElseWhere) = cpg.identifier("sg").take(2).l
      sgAssignment.typeFullName shouldBe "<export>::sendgrid.js::program:SendGridAPIClient"
      sgElseWhere.typeFullName shouldBe "<export>::sendgrid.js::program:SendGridAPIClient"
    }

    "resolve 'sg' call path from import information" in {
      val List(sendCall) = cpg.call("send").l
      sendCall.methodFullName shouldBe "<export>::sendgrid.js::program:SendGridAPIClient.send"
    }

    "resolve 'client' identifier types from import information" in {
      val List(clientAssignment, clientElseWhere) = cpg.identifier("client").take(2).l
      clientAssignment.typeFullName shouldBe "<export>::slack_sdk.js::program:WebClient"
      clientElseWhere.typeFullName shouldBe "<export>::slack_sdk.js::program:WebClient"
    }

    "resolve 'client' call path from identifier in child scope" in {
      val List(postMessage) = cpg.call("chatPostMessage").l
      postMessage.methodFullName shouldBe "<export>::slack_sdk.js::program:WebClient.chatPostMessage"
    }

    "resolve a dummy 'send' return value from sg.send" in {
      val List(postMessage) = cpg.identifier("response").l
      postMessage.typeFullName shouldBe "<export>::sendgrid.js::program:SendGridAPIClient.send.<returnValue>"
    }

  }

}
