package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language._

import java.io.File

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
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("__builtin.int", "__builtin.str")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("__builtin.int", "__builtin.str")
    }

    "resolve 'y' and 'z' identifier collection types" in {
      val List(zDict, zList, zTuple) = cpg.identifier("z").take(3).l
      zDict.dynamicTypeHintFullName shouldBe Seq("__builtin.dict", "__builtin.list", "__builtin.tuple")
      zList.dynamicTypeHintFullName shouldBe Seq("__builtin.dict", "__builtin.list", "__builtin.tuple")
      zTuple.dynamicTypeHintFullName shouldBe Seq("__builtin.dict", "__builtin.list", "__builtin.tuple")
    }

    "resolve 'z' identifier calls conservatively" in {
      // TODO: These should have callee entries but the method stubs are not present here
      val List(zAppend) = cpg.call("append").l
      zAppend.methodFullName shouldBe "__builtin.dict.append"
      zAppend.dynamicTypeHintFullName shouldBe Seq(
        "__builtin.dict.append",
        "__builtin.list.append",
        "__builtin.tuple.append"
      )
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
      sgAssignment.typeFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient.SendGridAPIClient<body>"
      sgElseWhere.typeFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient.SendGridAPIClient<body>"
    }

    "resolve 'sg' call path from import information" in {
      val List(apiClient) = cpg.call("SendGridAPIClient").l
      apiClient.methodFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient.<init>"
      val List(sendCall) = cpg.call("send").l
      sendCall.methodFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient.SendGridAPIClient<body>.send"
    }

    "resolve 'client' identifier types from import information" in {
      val List(clientAssignment, clientElseWhere) = cpg.identifier("client").take(2).l
      clientAssignment.typeFullName shouldBe "slack_sdk.py:<module>.WebClient.WebClient<body>"
      clientElseWhere.typeFullName shouldBe "slack_sdk.py:<module>.WebClient.WebClient<body>"
    }

    "resolve 'client' call path from identifier in child scope" in {
      val List(client) = cpg.call("WebClient").l
      client.methodFullName shouldBe "slack_sdk.py:<module>.WebClient.<init>"
      val List(postMessage) = cpg.call("chat_postMessage").l
      postMessage.methodFullName shouldBe "slack_sdk.py:<module>.WebClient.WebClient<body>.chat_postMessage"
    }

    "resolve a dummy 'send' return value from sg.send" in {
      val List(postMessage) = cpg.identifier("response").l
      postMessage.typeFullName shouldBe "sendgrid.py:<module>.SendGridAPIClient.SendGridAPIClient<body>.send.<returnValue>"
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
        |""".stripMargin).cpg

    "resolve 'db' identifier types from import information" in {
      val List(clientAssignment, clientElseWhere) = cpg.identifier("db").take(2).l
      clientAssignment.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>"
      clientElseWhere.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>"
    }

    "resolve the 'SQLAlchemy' constructor in the module" in {
      val Some(client) = cpg.call("SQLAlchemy").headOption
      client.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.<init>"
    }

    "resolve 'User' field types" in {
      val List(id, firstname, age, address) =
        cpg.identifier.nameExact("id", "firstname", "age", "address").takeRight(4).l
      id.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.Column.Column<body>"
      firstname.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.Column.Column<body>"
      age.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.Column.Column<body>"
      address.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.Column.Column<body>"
    }

    "resolve the 'Column' constructor for a class member" in {
      val Some(columnConstructor) = cpg.call("Column").headOption
      columnConstructor.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.Column.<init>"
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
        |""".stripMargin).cpg

    "resolve 'print' and 'max' calls" in {
      val Some(printCall) = cpg.call("print").headOption
      printCall.methodFullName shouldBe "__builtin.print"
      val Some(maxCall) = cpg.call("max").headOption
      maxCall.methodFullName shouldBe "__builtin.max"
    }

    "conservatively present either option when an imported function uses the same name as a builtin" in {
      val Some(absCall) = cpg.call("abs").headOption
      absCall.dynamicTypeHintFullName shouldBe Seq("foo.py:<module>.abs", "__builtin.abs")
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
    ).cpg

    "resolve 'x' and 'y' locally under foo.py" in {
      val Some(x) = cpg.file.name(".*foo.*").ast.isIdentifier.name("x").headOption
      x.typeFullName shouldBe "__builtin.int"
      val Some(y) = cpg.file.name(".*foo.*").ast.isIdentifier.name("y").headOption
      y.typeFullName shouldBe "__builtin.str"
    }

    "resolve 'foo.x' and 'foo.y' field access primitive types correctly" in {
      val List(z1, z2) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("z")
        .l
      z1.typeFullName shouldBe "ANY"
      z1.dynamicTypeHintFullName shouldBe Seq("__builtin.int", "__builtin.str")
      z2.typeFullName shouldBe "ANY"
      z2.dynamicTypeHintFullName shouldBe Seq("__builtin.int", "__builtin.str")
    }

    "resolve 'foo.d' field access object types correctly" in {
      val Some(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isIdentifier
        .name("d")
        .headOption
      d.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>"
      d.dynamicTypeHintFullName shouldBe Seq()
    }

    "resolve a 'createTable' call indirectly from 'foo.d' field access correctly" in {
      val List(d) = cpg.file
        .name(".*bar.*")
        .ast
        .isCall
        .name("createTable")
        .l
      d.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.createTable"
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

      d.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.deleteTable"
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
        |        return jsonify({"success": True})
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
    ).cpg

    "be determined as a variable reference and have its type recovered correctly" in {
      cpg.identifier("db").map(_.typeFullName).toSet shouldBe Set(
        "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>"
      )

      cpg
        .call("add")
        .where(_.parentBlock.ast.isIdentifier.typeFullName("flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>"))
        .where(_.parentBlock.ast.isFieldIdentifier.canonicalName("session"))
        .headOption
        .map(_.code) shouldBe Some("tmp0.add(user)")
    }

    "provide a dummy type to a member if the member type is not known" in {
      val Some(sessionTmpVar) = cpg.identifier("tmp0").headOption
      sessionTmpVar.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.<member>(session)"

      val Some(addCall) = cpg
        .call("add")
        .headOption
      addCall.typeFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.<member>(session).add"
      addCall.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.<member>(session).add"
      addCall.callee(NoResolve).isExternal.headOption shouldBe Some(true)
    }

  }

  "assignment from a call to a method inside an imported module" should {
    lazy val cpg = code("""
        |import logging
        |log = logging.getLogger(__name__)
        |log.error("foo")
        |""".stripMargin).cpg

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
        |""".stripMargin).cpg

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
    ).moreCode(
      """
        |# dummy file to trigger isExternal = false on methods that are imported from here
        |""".stripMargin,
      "pymongo.py"
    ).cpg

    "recover a potential type for `self.collection` using the assignment at `get_collection` as a type hint" in {
      val Some(selfFindFound) = cpg.typeDecl(".*InstallationsDAO.*").ast.isCall.name("find_one").headOption
      selfFindFound.dynamicTypeHintFullName shouldBe Seq(
        "__builtin.None.find_one",
        "pymongo.py:<module>.MongoClient.<init>.<indexAccess>.<indexAccess>.find_one"
      )
    }

    "correctly determine that, despite being unable to resolve the correct method full name, that it is an internal method" in {
      val Some(selfFindFound) = cpg.typeDecl(".*InstallationsDAO.*").ast.isCall.name("find_one").headOption
      selfFindFound.callee.isExternal.toSeq shouldBe Seq(true, false)
    }
  }

  "a recursive field access based call type" should {
    lazy val cpg = code(
      """
        |from flask import Flask, render_template
        |from flask_pymongo import PyMongo
        |
        |# Instance of Flask
        |app = Flask(__name__)
        |mongo = PyMongo(app)
        |
        |
        |@app.route('/')
        |@app.route("/bikes")
        |def bike():
        |    bikes = mongo.db.bikes.find()
        |    return render_template("index.html", bikes=bikes)
        |""".stripMargin,
      "app.py"
    )

    "manage to create a correct chain of dummy field accesses before the call" in {
      val Some(bikeFind) = cpg.call.name("find").headOption
      bikeFind.methodFullName shouldBe "flask_pymongo.py:<module>.PyMongo.PyMongo<body>.<member>(db).<member>(bikes).find"
    }
  }

  "a call from an import where the import acts as a module" should {
    lazy val cpg = code(
      """import os
        |import datadog
        |
        |# Initialize the Datadog client
        |options = {
        |    'api_key': os.environ.get('DD_API_KEY'),
        |    'app_key': os.environ.get('DD_APP_KEY')
        |}
        |
        |datadog.initialize(**options)
        |""".stripMargin,
      "app.py"
    )

    "recover the import as an identifier and not directly as a call" in {
      val Some(initCall) = cpg.call.name("initialize").headOption
      initCall.methodFullName shouldBe "datadog.py:<module>.initialize"
    }
  }

  "a call made from within a call invocation" should {
    lazy val cpg = code("""
        |from __future__ import unicode_literals
        |
        |from django.db import migrations, models
        |
        |
        |class Migration(migrations.Migration):
        |
        |    dependencies = [
        |        ('music', '0006_auto_20160325_1236'),
        |    ]
        |
        |    operations = [
        |        migrations.AddField(
        |            model_name='album',
        |            name='is_favorite',
        |            field=models.BooleanField(default=False),
        |        ),
        |    ]
        |""".stripMargin)

    "recover its full name successfully" in {
      val Some(addFieldConstructor) = cpg.call.name("AddField").headOption
      addFieldConstructor.methodFullName shouldBe Seq("django", "db.py:<module>.migrations.AddField.<init>").mkString(
        File.separator
      )
      val Some(booleanFieldConstructor) = cpg.call.name("BooleanField").headOption
      booleanFieldConstructor.methodFullName shouldBe Seq("django", "db.py:<module>.models.BooleanField.<init>")
        .mkString(File.separator)
    }
  }

  "a call made via an import from a directory" should {
    lazy val cpg = code("""
        |from data import db_session
        |
        |def foo():
        |   db_sess = db_session.create_session()
        |   x = db_sess.query(foo, bar)
        |""".stripMargin)
      .moreCode(
        """
          |from sqlalchemy.orm import Session
          |
          |def create_session() -> Session:
          |   global __factory
          |   return __factory()
          |""".stripMargin,
        "data/db_session.py"
      )

    "recover its full name successfully" in {
      val List(methodFullName) = cpg.call("query").methodFullName.l
      methodFullName shouldBe "sqlalchemy.orm.Session.query"
    }
  }

  "recover a method ref from a field identifier" should {
    lazy val cpg = code(
      """
        |from django.conf.urls import url
        |
        |from student import views
        |
        |urlpatterns = [
        |    url(r'^addStudent/$', views.add_student)
        |]
        |""".stripMargin,
      "urls.py"
    ).moreCode(
      """
        |def add_student():
        | pass
        |""".stripMargin,
      s"student${File.separator}views.py"
    )

    "recover the method full name related" in {
      val Some(methodRef) = cpg.methodRef.code("views.add_student").headOption
      methodRef.methodFullName shouldBe Seq("student", "views.py:<module>.add_student").mkString(File.separator)
      methodRef.typeFullName shouldBe "<empty>"
    }
  }

  "a type hint on a parameter" should {
    lazy val cpg = code("""
        |import sqlalchemy.orm as orm
        |
        |async def get_user_by_email(email: str, db: orm.Session):
        |   return db.query(user_models.User).filter(user_models.User.email == email).first()
        |""".stripMargin)

    "be sufficient to resolve method full names at calls" in {
      val List(call) = cpg.call("query").l
      call.methodFullName.startsWith("sqlalchemy.orm") shouldBe true
    }
  }

  "recover a member call from a reference to an imported global variable" should {
    lazy val cpg = code(
      """from api import db
        |
        |class UserModel(db.Model):
        |
        |   def save(self):
        |        try:
        |            db.session.add(self)
        |            db.session.commit()
        |        except IntegrityError:
        |            print(f"User with username={self.username} already exist")
        |            db.session.rollback()
        |""".stripMargin,
      Seq("api", "models", "user.py").mkString(File.separator)
    ).moreCode(
      """from flask_sqlalchemy import SQLAlchemy
        |
        |app = Flask(__name__, static_folder=Config.UPLOAD_FOLDER)
        |
        |db = SQLAlchemy(app)
        |""".stripMargin,
      Seq("api", "__init__.py").mkString(File.separator)
    )

    "recover a call to `add`" in {
      val Some(addCall) = cpg.call("add").headOption
      addCall.methodFullName shouldBe "flask_sqlalchemy.py:<module>.SQLAlchemy.SQLAlchemy<body>.<member>(session).add"
    }
  }

  "handle a wrapper function with the same name as an imported function" should {
    lazy val cpg = code("""
        |import requests
        |
        |class Client:
        |    access_token: str = None
        |    def post(self, uuid: str, account_id: str, endpoint: str = "results"):
        |        if not self.access_token:
        |            self.authenticate()
        |
        |        response = requests.post(
        |          url=f"https://{account_id}.rest.marketingcloudapis.com/data/v1/async/{uuid}/{endpoint}",
        |            headers={
        |                "Authorization": self.auth_header(),
        |                "Content-Type": "application/json",
        |            },
        |        )
        |        return response
        |""".stripMargin)

    "recover the child function `post` path correctly via receiver" in {
      val Some(postCallReceiver) = cpg.identifier("requests").headOption
      postCallReceiver.typeFullName shouldBe "requests.py:<module>"
      val Some(postCall) = cpg.call("post").headOption
      postCall.methodFullName shouldBe "requests.py:<module>.post"
    }
  }

  "handle a call from parameter with a type hint" should {
    lazy val cpg = code("""
        |import models.user as user_models
        |import sqlalchemy.orm as orm
        |
        |async def get_user_by_email(email: str, db: orm.Session):
        |    return db.query(user_models.User).filter(user_models.User.email == email).first()
        |""".stripMargin)

    "with the correct identifier and call types" in {
      val Some(postCallReceiver) = cpg.identifier("db").headOption
      postCallReceiver.typeFullName shouldBe "sqlalchemy.orm.Session"
      val Some(postCall) = cpg.call("query").headOption
      postCall.methodFullName shouldBe "sqlalchemy.orm.Session.query"
    }
  }

  "Import statement with method ref sample one" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from student import views
        |
        |urlpatterns = [
        |    url(r'allPage', views.all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code("print('Hello, world!')")
      .moreCode(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("student", "views.py").mkString(File.separator))

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("student", "views.py:<module>.all_page").mkString(File.separator)
    allPageRef.code shouldBe "views.all_page"
  }

  "Import statement with method ref sample two" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from .import views
        |
        |urlpatterns = [
        |    url(r'allPage', views.all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, "urls.py")
      .moreCode(views, "views.py")

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe "views.py:<module>.all_page"
    allPageRef.code shouldBe "views.all_page"
  }

  "Import statement with method ref sample three" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from .import views
        |
        |urlpatterns = [
        |    url(r'allPage', views.all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("controller", "views.py").mkString(File.separator))

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("controller", "views.py:<module>.all_page").mkString(File.separator)
    allPageRef.code shouldBe "views.all_page"
  }

  "Import statement with method ref sample four" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from .views import all_page
        |
        |urlpatterns = [
        |    url(r'allPage', all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("controller", "views.py").mkString(File.separator))

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("controller", "views.py:<module>.all_page").mkString(File.separator)
    allPageRef.code shouldBe "all_page"
  }

  "Import statement with method ref sample five" in {
    val controller =
      """
        |from django.contrib import admin
        |from django.urls import path
        |from django.conf.urls import url
        |from student.views import all_page
        |
        |urlpatterns = [
        |    url(r'allPage', all_page)
        |]
        |""".stripMargin
    val views =
      """
        |def all_page(request):
        |	print("All pages")
        |""".stripMargin
    val cpg = code(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("student", "views.py").mkString(File.separator))

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("student", "views.py:<module>.all_page").mkString(File.separator)
    allPageRef.code shouldBe "all_page"
  }

  "Import statement with method ref sample six" in {
    val controller =
      """
        |from django.urls import path
        |from authy.views import PasswordChange
        |
        |urlpatterns = [
        |   path('changepassword/', PasswordChange, name='change_password')
        |]
        |""".stripMargin
    val views =
      """from django.contrib.auth.decorators import login_required
        |
        |@login_required
        |def PasswordChange(request):
        |    print("All pages")
        |
        |""".stripMargin
    val cpg = code(controller, Seq("controller", "urls.py").mkString(File.separator))
      .moreCode(views, Seq("authy", "views.py").mkString(File.separator))

    val Some(allPageRef) = cpg.call.methodFullName("django.*[.](path|url)").argument.isMethodRef.headOption
    allPageRef.methodFullName shouldBe Seq("authy", "views.py:<module>.PasswordChange").mkString(File.separator)
    allPageRef.code shouldBe "PasswordChange"
  }

}
