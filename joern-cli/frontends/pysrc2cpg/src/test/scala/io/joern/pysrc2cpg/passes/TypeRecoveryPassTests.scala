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
      zAppend.dynamicTypeHintFullName shouldBe Seq("dict.append", "list.append", "tuple.append")
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

  "recovering paths for built-in calls" should {
    lazy val cpg = code("""
        |print("Hello world")
        |max(1, 2)
        |
        |from foo import abs
        |
        |x = abs(-1)
        |""".stripMargin)

    "resolve 'print' and 'max' calls" in {
      val Some(printCall) = cpg.call("print").headOption
      printCall.methodFullName shouldBe "builtins.py:<module>.print"
      val Some(maxCall) = cpg.call("max").headOption
      maxCall.methodFullName shouldBe "builtins.py:<module>.max"
    }

    "select the imported abs over the built-in type when call is shadowed" in {
      val Some(absCall) = cpg.call("abs").headOption
      absCall.dynamicTypeHintFullName shouldBe Seq("foo.py:<module>.abs")
    }

  }

  "recovering module members across modules" should {
    lazy val cpg = code(
      """
        |from flask_sqlalchemy import SQLAlchemy
        |
        |x = 1
        |y = "test"
        |db = SQLAlchemy()
        |""".stripMargin,
      "foo.py"
    ).moreCode(
      """
        |import foo
        |
        |z = foo.x
        |z = foo.y
        |
        |d = foo.db
        |
        |d.createTable()
        |
        |foo.db.deleteTable()
        |""".stripMargin,
      "bar.py"
    )

    "resolve 'x' and 'y' locally under foo.py" in {
      val Some(x) = cpg.file.name(".*foo.*").ast.isIdentifier.name("x").headOption
      x.typeFullName shouldBe "int"
      val Some(y) = cpg.file.name(".*foo.*").ast.isIdentifier.name("y").headOption
      y.typeFullName shouldBe "str"
    }

    "resolve 'foo.x' and 'foo.y' field access primitive types correctly" in {
      val List(z1, z2) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("z")
        .l
      z1.typeFullName shouldBe "ANY"
      z1.dynamicTypeHintFullName shouldBe Seq("int", "str")
      z2.typeFullName shouldBe "ANY"
      z2.dynamicTypeHintFullName shouldBe Seq("int", "str")
    }

    "resolve 'foo.d' field access object types correctly" in {
      val Some(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("d")
        .headOption
      d.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy"
      d.dynamicTypeHintFullName shouldBe Seq()
    }

    "resolve a 'createTable' call indirectly from 'foo.d' field access correctly" in {
      val List(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isCall
        .name("createTable")
        .l
      d.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.createTable"
      d.dynamicTypeHintFullName shouldBe Seq()
      d.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

    "resolve a 'deleteTable' call directly from 'foo.db' field access correctly" in {
      val List(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isCall
        .name("deleteTable")
        .l

      d.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.deleteTable"
      d.dynamicTypeHintFullName shouldBe Seq()
      d.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

  }

  "type recovery for a field imported as an individual component" should {
    lazy val cpg = code(
      """import app
        |from flask import jsonify
        |
        |def store():
        |    from app import db
        |    try:
        |        db.create_all()
        |        db.session.add(user)
        |        return jsonify({"success": message})
        |    except Exception as e:
        |        return 'There was an issue adding your task'
        |""".stripMargin,
      "UserController.py"
    ).moreCode(
      """
        |from flask_sqlalchemy import SQLAlchemy
        |
        |db = SQLAlchemy()
        |""".stripMargin,
      "app.py"
    )

    "be determined as a variable reference and have its type recovered correctly" in {
      cpg.identifier("db").map(_.typeFullName).toSet shouldBe Set("flask_sqlalchemy.py:<module>.SQLAlchemy")

      cpg
        .call("add")
        .where(_.parentBlock.ast.isIdentifier.typeFullName("flask_sqlalchemy.py:<module>.SQLAlchemy"))
        .where(_.parentBlock.ast.isFieldIdentifier.canonicalName("session"))
        .headOption
        .map(_.code) shouldBe Some("tmp0.add(user)")
    }

    "provide a dummy type to a member if the member type is not known" in {
      val Some(sessionTmpVar) = cpg.identifier("tmp0").headOption
      sessionTmpVar.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.<member>(session)"

      val Some(addCall) = cpg
        .call("add")
        .headOption
      addCall.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.<member>(session).add"
      addCall.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.<member>(session).add"
      addCall.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

  }

  "assignment from a call to a method inside an imported module" should {
    lazy val cpg = code("""
        |import logging
        |log = logging.getLogger(__name__)
        |log.error("foo")
        |""".stripMargin)

    "provide a dummy type" in {
      val Some(log) = cpg.identifier("log").headOption
      log.typeFullName shouldBe "logging.py:<module>.getLogger.<returnValue>"
      val List(errorCall) = cpg.call("error").l
      errorCall.methodFullName shouldBe "logging.py:<module>.getLogger.<returnValue>.error"
      val List(getLoggerCall) = cpg.call("getLogger").l
      getLoggerCall.methodFullName shouldBe "logging.py:<module>.getLogger"
    }
  }

  "a constructor call from a field access of an externally imported package" should {
    lazy val cpg = code("""
        |import urllib.error
        |import urllib.request
        |
        |req = urllib.request.Request(url=apiUrl, data=dataBytes, method='POST')
        |""".stripMargin)

    "reasonably determine the constructor type" in {
      val Some(tmp0) = cpg.identifier("tmp0").headOption
      tmp0.typeFullName shouldBe "urllib.py:<module>.request"
      val Some(requestCall) = cpg.call("Request").headOption
      requestCall.methodFullName shouldBe "urllib.py:<module>.request.Request.<init>"
    }
  }

  "a method call inherited from a super class should be recovered" should {
    lazy val cpg = code(
      """from pymongo import MongoClient
        |from django.conf import settings
        |
        |
        |class MongoConnection(object):
        |    def __init__(self):
        |        DATABASES = settings.DATABASES
        |        self.client = MongoClient(
        |            host=[DATABASES['MONGO']['HOST']],
        |            username=DATABASES['MONGO']['USERNAME'],
        |            password=DATABASES['MONGO']['PASSWORD'],
        |            authSource=DATABASES['MONGO']['AUTH_DATABASE']
        |        )
        |        self.db = self.client[DATABASES['MONGO']['DATABASE']]
        |        self.collection = None
        |
        |    def get_collection(self, name):
        |        self.collection = self.db[name]
        |""".stripMargin,
      "MongoConnection.py"
    ).moreCode(
      """
        |from MongoConnection import MongoConnection
        |
        |class InstallationsDAO(MongoConnection):
        |    def __init__(self):
        |        super(InstallationsDAO, self).__init__()
        |        self.get_collection("installations")
        |
        |    def getCustomerId(self, installationId):
        |        res = self.collection.find_one({'_id': installationId})
        |        if res is None:
        |            return None
        |        return dict(res).get("customerId", None)
        |""".stripMargin,
      "InstallationDao.py"
    )

    "recover a potential type for `self.collection` using the assignment at `get_collection` as a type hint" in {
      val Some(selfFindFound) = cpg.typeDecl(".*InstallationsDAO.*").ast.isCall.name("find_one").headOption
      selfFindFound.methodFullName shouldBe "pymongo.py:<module>.MongoClient.<init>.<indexAccess>.<indexAccess>.find_one"
    }
  }

}
