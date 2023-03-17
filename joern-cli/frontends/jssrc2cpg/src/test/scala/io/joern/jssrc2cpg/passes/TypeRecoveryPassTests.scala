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
      val List(xOuterScope, xInnerScope) = cpg.identifier.nameExact("x").l
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
        |""".stripMargin,
      "Test1.ts"
    ).cpg

    "resolve 'sg' identifier types from import information" in {
      val List(sg1, sg2, sg3) = cpg.identifier.nameExact("sg").l
      sg1.typeFullName shouldBe "sendgrid:SendGridAPIClient"
      sg2.typeFullName shouldBe "sendgrid:SendGridAPIClient"
      sg3.typeFullName shouldBe "sendgrid:SendGridAPIClient"
    }

    "resolve 'sg' call path from import information" in {
      val List(sendCall) = cpg.call("send").l
      sendCall.methodFullName shouldBe "sendgrid:SendGridAPIClient.send"
    }

    "resolve 'client' identifier types from import information" in {
      val List(client1, client2, client3) = cpg.identifier.nameExact("client").l
      client1.typeFullName shouldBe "slack_sdk:WebClient"
      client2.typeFullName shouldBe "slack_sdk:WebClient"
      client3.typeFullName shouldBe "slack_sdk:WebClient"
    }

    "resolve 'client' call path from identifier in child scope" in {
      val List(postMessage) = cpg.call("chatPostMessage").l
      postMessage.methodFullName shouldBe "slack_sdk:WebClient.chatPostMessage"
    }

    "resolve a dummy 'send' return value from sg.send" in {
      val List(postMessage) = cpg.identifier("response").l
      postMessage.typeFullName shouldBe "sendgrid:SendGridAPIClient.send.<returnValue>"
    }

  }

  "recovering paths for built-in calls" should {
    lazy val cpg = code("""
        |console.log("Hello world");
        |let x = Math.abs(-1);
        |""".stripMargin).cpg

    "resolve 'print' and 'max' calls" in {
      val Some(printCall) = cpg.call("log").headOption
      printCall.methodFullName shouldBe "__whatwg.console.log"
      val Some(maxCall) = cpg.call("abs").headOption
      maxCall.methodFullName shouldBe "__ecma.Math.abs"
      val Some(x) = cpg.identifier("x").headOption
      // TODO: Ideally we would know the result of `abs` but this can be a future task
      x.typeFullName shouldBe "__ecma.Math.abs.<returnValue>"
    }

  }

  "recovering module members across modules" should {
    lazy val cpg = code(
      """
        |import { SQLAlchemy } from "flask_sqlalchemy";
        |
        |export const x = 1;
        |export const y = "test";
        |export const db = new SQLAlchemy();
        |""".stripMargin,
      "Foo.ts"
    ).moreCode(
      """
        |import { x, y, db } from './Foo';
        |
        |let z = x;
        |z = y;
        |
        |let d = db;
        |
        |d.createTable()
        |
        |db.deleteTable();
        |""".stripMargin,
      "Bar.ts"
    ).cpg

    "resolve 'x' and 'y' locally under foo.py" in {
      val Some(x) = cpg.file.name(".*Foo.*").ast.isIdentifier.nameExact("x").headOption
      x.typeFullName shouldBe "__ecma.Number"
      val Some(y) = cpg.file.name(".*Foo.*").ast.isIdentifier.nameExact("y").headOption
      y.typeFullName shouldBe "__ecma.String"
      val Some(db) = cpg.file.name(".*Foo.*").ast.isIdentifier.nameExact("db").headOption
      db.typeFullName shouldBe "flask_sqlalchemy:SQLAlchemy"
    }

    "resolve 'foo.x' and 'foo.y' field access primitive types correctly" in {
      val List(z1, z2) = cpg.file
        .name(".*Bar.*")
        .ast
        .isIdentifier
        .nameExact("z")
        .l
      z1.typeFullName shouldBe "ANY"
      z1.dynamicTypeHintFullName shouldBe Seq("__ecma.Number", "__ecma.String")
      z2.typeFullName shouldBe "ANY"
      z2.dynamicTypeHintFullName shouldBe Seq("__ecma.Number", "__ecma.String")
    }

    "resolve 'foo.d' field access object types correctly" in {
      val Some(d) = cpg.file
        .name(".*Bar.*")
        .ast
        .isIdentifier
        .nameExact("d")
        .headOption
      d.typeFullName shouldBe "flask_sqlalchemy:SQLAlchemy"
      d.dynamicTypeHintFullName shouldBe Seq()
    }

    "resolve a 'createTable' call indirectly from 'foo.d' field access correctly" in {
      val List(d) = cpg.file
        .name(".*Bar.*")
        .ast
        .isCall
        .name("createTable")
        .l
      d.methodFullName shouldBe "flask_sqlalchemy:SQLAlchemy.createTable"
      d.dynamicTypeHintFullName shouldBe Seq()
      d.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

    "resolve a 'deleteTable' call directly from 'foo.db' field access correctly" in {
      val List(d) = cpg.file
        .name(".*Bar.*")
        .ast
        .isCall
        .name("deleteTable")
        .l
      d.methodFullName shouldBe "flask_sqlalchemy:SQLAlchemy.deleteTable"
      d.dynamicTypeHintFullName shouldBe empty
      d.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

  }

}
