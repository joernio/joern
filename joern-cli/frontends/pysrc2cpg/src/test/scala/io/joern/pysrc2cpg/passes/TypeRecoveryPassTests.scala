package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.semanticcpg.language._

class TypeRecoveryPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "literals declared from built-in types" should {
    lazy val cpg = code("""
        |x = 123
        |
        |def foo_shadowing():
        |   x = "foo"
        |
        |z = {'a': 123}
        |z = [1, 2, 3]
        |z = (1, 2, 3)
        |# This should fail, as tuples are immutable
        |z.append(4)
        |""".stripMargin).cpg

    "resolve 'x' identifier types despite shadowing" in {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("int", "str")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("int", "str")
    }

    "resolve 'y' and 'z' identifier collection types" in {
      val List(zDict, zList, zTuple) = cpg.identifier("z").take(3).l
      zDict.dynamicTypeHintFullName shouldBe Seq("dict", "list", "tuple")
      zList.dynamicTypeHintFullName shouldBe Seq("dict", "list", "tuple")
      zTuple.dynamicTypeHintFullName shouldBe Seq("dict", "list", "tuple")
    }

    "resolve 'z' identifier calls conservatively" in {
      // TODO: These should have callee entries but the method stubs are not present here
      val List(zAppend) = cpg.call("append").l
      zAppend.methodFullName shouldBe Defines.DynamicCallUnknownFallName
      zAppend.dynamicTypeHintFullName shouldBe Seq("dict", "list", "tuple")
    }
  }

  "call from a function from an external type" should {

    lazy val cpg = code("""
        |from slack_sdk import WebClient
        |from sendgrid import SendGridAPIClient
        |
        |client = WebClient(token="WOLOLO")
        |sg = SendGridAPIClient("SENGRID_KEY_WOLOLO")
        |
        |def send_slack_message(chan, msg):
        |    client.chat_postMessage(channel=chan, text=msg)
        |
        |response = sg.send(message)
        |""".stripMargin).cpg

    "resolve 'sg' identifier types from import information" in {
      val List(sgAssignment, sgElseWhere) = cpg.identifier("sg").take(2).l
      sgAssignment.typeFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient"
      sgElseWhere.typeFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient"
    }

    "resolve 'sg' call path from import information" in {
      val List(apiClient) = cpg.call("SendGridAPIClient").l
      apiClient.methodFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient.<init>"
      val List(sendCall) = cpg.call("send").l
      sendCall.methodFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient.send"
    }

    "resolve 'client' identifier types from import information" in {
      val List(clientAssignment, clientElseWhere) = cpg.identifier("client").take(2).l
      clientAssignment.typeFullName shouldBe "slack_sdk.py:<module>.WebClient"
      clientElseWhere.typeFullName shouldBe "slack_sdk.py:<module>.WebClient"
    }

    "resolve 'client' call path from identifier in child scope" in {
      val List(client) = cpg.call("WebClient").l
      client.methodFullName shouldBe "slack_sdk.py:<module>.WebClient.<init>"
      val List(postMessage) = cpg.call("chat_postMessage").l
      postMessage.methodFullName shouldBe "slack_sdk.py:<module>.WebClient.chat_postMessage"
    }

  }

  "type recovery on class members" should {
    lazy val cpg = code("""
        |from flask_sqlalchemy import SQLAlchemy
        |
        |db = SQLAlchemy()
        |
        |class User(db.Model):
        |    __tablename__ = 'users'
        |
        |    id = db.Column(db.Integer, primary_key=True)
        |    firstname = db.Column(db.String)
        |    age = db.Column(db.Integer)
        |    address = db.Column(db.String(120))
        |
        |    def __repr__(self):
        |        return '<Task %r>' % self.id
        |
        |    @property
        |    def serialize(self):
        |        return {
        |            'id': self.id,
        |            'firstname': self.firstname,
        |            'age': self.age,
        |            'address': self.address
        |        }
        |""".stripMargin)

    "resolve 'db' identifier types from import information" in {
      val List(clientAssignment, clientElseWhere) = cpg.identifier("db").take(2).l
      clientAssignment.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy"
      clientElseWhere.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy"
    }

    "resolve the 'SQLAlchemy' constructor in the module" in {
      val Some(client) = cpg.call("SQLAlchemy").headOption
      client.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.<init>"
    }

    "resolve 'User' field types" in {
      val List(id, firstname, age, address) =
        cpg.identifier.nameExact("id", "firstname", "age", "address").takeRight(4).l
      id.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.Column"
      firstname.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.Column"
      age.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.Column"
      address.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.Column"
    }

    "resolve the 'Column' constructor for a class member" in {
      val Some(columnConstructor) = cpg.call("Column").headOption
      columnConstructor.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.Column.<init>"
    }
  }

}
