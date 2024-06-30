package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.semanticcpg.language.*

class TypeRecoveryPassTests extends DataFlowCodeToCpgSuite {

  "literals declared from built-in types" should {
    val cpg = code("""
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
      cpg.identifier("z").typeFullName.toSet.headOption shouldBe Option("__ecma.Array")
    }

    "resolve 'z' identifier call correctly" in {
      val List(zAppend) = cpg.call("push").l
      zAppend.methodFullName shouldBe "__ecma.Array:push"
    }
  }

  "call from a function from an external type" should {
    val cpg = code(
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
    )

    "resolve correct imports via tag nodes" in {
      val List(a: UnknownMethod, b: UnknownTypeDecl, x: UnknownMethod, y: UnknownTypeDecl) =
        cpg.call.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      a.fullName shouldBe "slack_sdk:WebClient"
      b.fullName shouldBe "slack_sdk:WebClient"
      x.fullName shouldBe "sendgrid:SendGridAPIClient"
      y.fullName shouldBe "sendgrid:SendGridAPIClient"
    }

    "resolve 'sg' identifier types from import information" in {
      val List(sg1, sg2, sg3) = cpg.identifier.nameExact("sg").l
      sg1.typeFullName shouldBe "sendgrid:SendGridAPIClient"
      sg2.typeFullName shouldBe "sendgrid:SendGridAPIClient"
      sg3.typeFullName shouldBe "sendgrid:SendGridAPIClient"
    }

    "resolve 'sg' call path from import information" in {
      val List(sendCall) = cpg.call("send").l
      sendCall.methodFullName shouldBe "sendgrid:SendGridAPIClient:send"
    }

    "resolve 'client' identifier types from import information" in {
      val List(client1, client2, client3) = cpg.identifier.nameExact("client").l
      client1.typeFullName shouldBe "slack_sdk:WebClient"
      client2.typeFullName shouldBe "slack_sdk:WebClient"
      client3.typeFullName shouldBe "slack_sdk:WebClient"
    }

    "resolve 'client' call path from identifier in child scope" in {
      val List(postMessage) = cpg.call("chatPostMessage").l
      postMessage.methodFullName shouldBe "slack_sdk:WebClient:chatPostMessage"
    }

    "resolve a dummy 'send' return value from sg.send" in {
      val List(postMessage) = cpg.identifier("response").l
      postMessage.typeFullName shouldBe "sendgrid:SendGridAPIClient:send:<returnValue>"
    }

  }

  "recovering paths for built-in calls" should {
    val cpg = code("""
        |console.log("Hello world");
        |let x = Math.abs(-1);
        |""".stripMargin)

    "resolve 'print' and 'max' calls" in {
      val List(printCall) = cpg.call("log").l
      printCall.methodFullName shouldBe "__whatwg.console:log"
      val List(maxCall) = cpg.call("abs").l
      maxCall.methodFullName shouldBe "__ecma.Math:abs"
      val List(x) = cpg.identifier("x").l
      // TODO: Ideally we would know the result of `abs` but this can be a future task
      x.typeFullName shouldBe "__ecma.Math:abs:<returnValue>"
    }

  }

  "recovering module members across modules" should {
    val cpg = code(
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
    )

    "resolve correct imports via tag nodes" in {
      val List(a: ResolvedMember, b: ResolvedMember, c: ResolvedMember, d: UnknownMethod, e: UnknownTypeDecl) =
        cpg.call.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      a.basePath shouldBe "Foo.ts::program"
      a.memberName shouldBe "x"
      b.basePath shouldBe "Foo.ts::program"
      b.memberName shouldBe "y"
      c.basePath shouldBe "Foo.ts::program"
      c.memberName shouldBe "db"
      d.fullName shouldBe "flask_sqlalchemy:SQLAlchemy"
      e.fullName shouldBe "flask_sqlalchemy:SQLAlchemy"
    }

    "resolve 'x' and 'y' locally under Foo.ts" in {
      val List(x1, x2) = cpg.file.name(".*Foo.*").ast.isIdentifier.nameExact("x").l
      x1.typeFullName shouldBe "__ecma.Number"
      x2.typeFullName shouldBe "__ecma.Number"
      val List(y1, y2) = cpg.file.name(".*Foo.*").ast.isIdentifier.nameExact("y").l
      y1.typeFullName shouldBe "__ecma.String"
      y2.typeFullName shouldBe "__ecma.String"
      val List(db1, db2) = cpg.file.name(".*Foo.*").ast.isIdentifier.nameExact("db").l
      db1.typeFullName shouldBe "flask_sqlalchemy:SQLAlchemy"
      db2.typeFullName shouldBe "flask_sqlalchemy:SQLAlchemy"
    }

    "resolve 'foo.x' and 'foo.y' field access primitive types correctly" in {
      val List(z1, z2) = cpg.file.name(".*Bar.*").ast.isIdentifier.nameExact("z").l
      z1.typeFullName shouldBe "ANY"
      z1.dynamicTypeHintFullName shouldBe Seq("__ecma.Number", "__ecma.String")
      z2.typeFullName shouldBe "ANY"
      z2.dynamicTypeHintFullName shouldBe Seq("__ecma.Number", "__ecma.String")
    }

    "resolve 'foo.d' field access object types correctly" in {
      val List(d1, d2, d3) = cpg.file.name(".*Bar.*").ast.isIdentifier.nameExact("d").l
      d1.typeFullName shouldBe "flask_sqlalchemy:SQLAlchemy"
      d1.dynamicTypeHintFullName shouldBe Seq()
      d2.typeFullName shouldBe "flask_sqlalchemy:SQLAlchemy"
      d2.dynamicTypeHintFullName shouldBe Seq()
      d3.typeFullName shouldBe "flask_sqlalchemy:SQLAlchemy"
      d3.dynamicTypeHintFullName shouldBe Seq()
    }

    "resolve a 'createTable' call indirectly from 'foo.d' field access correctly" in {
      val List(d) = cpg.file.name(".*Bar.*").ast.isCall.name("createTable").l
      d.methodFullName shouldBe "flask_sqlalchemy:SQLAlchemy:createTable"
      d.dynamicTypeHintFullName shouldBe Seq()
      d.callee(NoResolve).isExternal.headOption shouldBe Option(true)
    }

    "resolve a 'deleteTable' call directly from 'foo.db' field access correctly" in {
      val List(d) = cpg.file
        .name(".*Bar.*")
        .ast
        .isCall
        .name("deleteTable")
        .l
      d.methodFullName shouldBe "flask_sqlalchemy:SQLAlchemy:deleteTable"
      d.dynamicTypeHintFullName shouldBe empty
      d.callee(NoResolve).isExternal.headOption shouldBe Option(true)
    }

  }

  "Importing an anonymous function" should {
    val cpg = code(
      """
        |var refThis = this;
        |
        |exports.getIncrementalInteger = (function() {
        |	var count = 0;
        |	return function() {
        |		count++;
        |		return count;
        |	};
        |})();
        |
        |refThis.getIncrementalInteger();
        |""".stripMargin,
      "util.js"
    ).moreCode(
      """
        |var util = require("./util.js");
        |
        |util.getIncrementalInteger()
        |""".stripMargin,
      "foo.js"
    )

    "resolve correct imports via tag nodes" in {
      val List(x: ResolvedMethod) = cpg.call.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      x.fullName shouldBe "util.js::program:getIncrementalInteger"
    }

    "resolve the method full name off of an aliased 'this'" in {
      val List(x) = cpg.file("util.js").ast.isCall.nameExact("getIncrementalInteger").l
      x.methodFullName shouldBe "util.js::program:getIncrementalInteger"
    }

    "resolve the method full name off of the imported 'util'" in {
      val List(x) = cpg.file("foo.js").ast.isCall.nameExact("getIncrementalInteger").l
      x.methodFullName shouldBe "util.js::program:getIncrementalInteger"
    }

    "resolve the full name of the currying from the closure" in {
      val List(x) = cpg.file("util.js").ast.isCall.nameExact("<lambda>0").lineNumber(4).l
      x.name shouldBe "<lambda>0"
      x.methodFullName shouldBe "util.js::program:<lambda>0"
    }
  }

  "Type obtained via assignment from `require`" should {
    val cpg = code("""
        |const google = require('googleapis');
        |const driveObj = google.drive({ version: 'v3', auth });
        |""".stripMargin)

    "resolve correct imports via tag nodes" in {
      val List(x: UnknownMethod, y: UnknownTypeDecl) =
        cpg.call.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      x.fullName shouldBe "googleapis"
      y.fullName shouldBe "googleapis"
    }

    "be propagated to `methodFullName` of call" in {
      val List(methodFullName) = cpg.call.code("google.drive\\(.*").methodFullName.l
      methodFullName shouldBe "googleapis:drive"
      val List(typeFullName) = cpg.identifier.name("driveObj").typeFullName.l
      typeFullName shouldBe "googleapis:drive:<returnValue>"
    }

  }

  "Type obtained via assignment from `require` to {...}" should {
    val cpg = code("""
        |const { google } = require('googleapis');
        |const driveObj = google.drive({ version: 'v3', auth });
        |""".stripMargin)

    "resolve correct imports via tag nodes" in {
      val List(x: UnknownMethod, y: UnknownTypeDecl, z: UnknownMethod) =
        cpg.call.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      x.fullName shouldBe "googleapis"
      y.fullName shouldBe "googleapis"
      z.fullName shouldBe "googleapis"
    }

    "be propagated to `methodFullName` of call" in {
      val List(methodFullName) = cpg.call.code("google.drive\\(.*").methodFullName.l
      methodFullName shouldBe "googleapis:drive"
      val List(typeFullName) = cpg.identifier.name("driveObj").typeFullName.l
      typeFullName shouldBe "googleapis:drive:<returnValue>"
    }
  }

  "Type obtained via field access from 'require' derived identifier" should {
    val cpg = code("""
        |import google from 'googleapis';
        |export const authObj = new google.auth.GoogleAuth({
        |  keyFile: 'path/to/your/credentials.json',
        |  scopes: ['https://www.googleapis.com/auth/drive'],
        |});
        |""".stripMargin)

    "be propagated to `methodFullName` of call" in {
      val List(constructor) = cpg.call.code("new google.auth.GoogleAuth\\(.*").l
      constructor.methodFullName shouldBe "googleapis:google:<member>(auth):GoogleAuth:<init>"
      val List(authObj1, authObj2) = cpg.identifier.name("authObj").l
      authObj1.typeFullName shouldBe "googleapis:google:<member>(auth):GoogleAuth"
      authObj2.typeFullName shouldBe "googleapis:google:<member>(auth):GoogleAuth"
    }
  }

  "Type casts of an identifier and call receiver" should {
    val cpg = code("""
        |let imgScr: string = <string>this.imageElement;
        |this.imageElement = new HTMLImageElement();
        |(<HTMLImageElement>this.imageElement).src = imgScr;
        |""".stripMargin)

    "succeed in propagating type cast identifiers" in {
      val List(imgSrc1, imgSrc2) = cpg.identifier("imgScr").l
      imgSrc1.typeFullName shouldBe "__ecma.String"
      imgSrc2.typeFullName shouldBe "__ecma.String"
      val List(tmp1, tmp2, tmp3) = cpg.identifier("_tmp_0").l
      tmp1.typeFullName shouldBe "__ecma.HTMLImageElement"
      tmp2.typeFullName shouldBe "__ecma.HTMLImageElement"
      tmp3.typeFullName shouldBe "__ecma.HTMLImageElement"
    }
  }

  "Type hints for method parameters and returns" should {
    val cpg = code("""
        |import google from 'googleapis';
        |
        |function foo(a: google.More, b: google.Money): google.Problems {
        | a.bar();
        | b.baz();
        |}
        |""".stripMargin)

    "be propagated within the method full name reflecting the import `googleapis`" in {
      val List(bar) = cpg.call("bar").l
      bar.methodFullName shouldBe "googleapis:google:More:bar"
      val List(baz) = cpg.call("baz").l
      baz.methodFullName shouldBe "googleapis:google:Money:baz"
      val List(foo) = cpg.method("foo").methodReturn.l
      foo.typeFullName shouldBe "googleapis:google:Problems"
    }
  }

  "Recovered values that are returned in methods" should {
    val cpg = code(
      """
        |const axios = require("axios");
        |
        |exports.literalFunction = function() { return 2; };
        |
        |const axiosInstance = axios.create({
        |  baseURL: 'https://api.example.com',
        |  timeout: 5000,
        |  headers: {  'Content-Type': 'application/json' }
        |});
        |
        |exports.get = (url: string, config?: any) => {
        |  return axiosInstance.get(url, config);
        |};
        |
        |""".stripMargin,
      "foo.js"
    ).moreCode(
      """
        |const foo = require("./foo");
        |
        |const x = foo.literalFunction();
        |const y = foo.get();
        |""".stripMargin,
      "bar.js"
    )

    "resolve correct imports via tag nodes" in {
      val List(a: ResolvedTypeDecl, b: ResolvedMethod, c: ResolvedMethod, d: UnknownMethod, e: UnknownTypeDecl) =
        cpg.call.where(_.referencedImports).tag._toEvaluatedImport.toList: @unchecked
      a.fullName shouldBe "foo.js::program"
      b.fullName shouldBe "foo.js::program:literalFunction"
      c.fullName shouldBe "foo.js::program:get"
      d.fullName shouldBe "axios"
      e.fullName shouldBe "axios"

    }

    "propagate literal types to the method return" in {
      val List(literalMethod) = cpg.method.nameExact("literalFunction").l
      literalMethod.methodReturn.typeFullName shouldBe "__ecma.Number"
      val List(x) = cpg.identifier("x").l
      x.typeFullName shouldBe "__ecma.Number"
      val List(literalCall) = cpg.call.nameExact("literalFunction").l
      literalCall.typeFullName shouldBe "__ecma.Number"
    }

    "propagate complex types to the method return" in {
      val List(getMethod) = cpg.method.nameExact("get").lineNumber(12).l
      getMethod.methodReturn.typeFullName shouldBe "axios:create:<returnValue>:get:<returnValue>"
      val List(y) = cpg.identifier("y").l
      y.typeFullName shouldBe "axios:create:<returnValue>:get:<returnValue>"
      val List(getCall) = cpg.call.nameExact("get").lineNumber(5).l
      getCall.typeFullName shouldBe "axios:create:<returnValue>:get:<returnValue>"
    }
  }

  "Temporary variables inserted to produce a three-address code structure" should {
    val cpg = code(
      """
        |import { HttpClient } from '@angular/common/http';
        |
        |@Injectable({
        |  providedIn: 'root',
        |})
        |export class SharedService {
        |  private http: HttpClient = new HttpClient();
        |  saveUserFeedback(payload) {
        |    return this.http.post('https://google.com', payload);
        |  }
        |}
        |""".stripMargin,
      "foo.ts"
    )

    "have their calls from a field access structure successfully recovered" in {
      cpg.identifier("_tmp_2").typeFullName.headOption shouldBe Option("@angular/common/http:HttpClient")
      cpg.call("post").methodFullName.headOption shouldBe Option("@angular/common/http:HttpClient:post")
    }

  }

  "Members initialized from constructors where the parameter has a type hint" should {

    val cpg = code(
      """
        |import { HttpClient } from '@angular/common/http';
        |
        |@Injectable({
        |  providedIn: 'root',
        |})
        |export class SharedService {
        |  constructor(private http: HttpClient) {
        |     this.http = http;
        |  }
        |  saveUserFeedback(payload) {
        |    return this.http.post('https://google.com', payload);
        |  }
        |}
        |""".stripMargin,
      "foo.ts"
    )

    "have the type hint recovered and successfully propagated" in {
      val m = cpg.method.fullNameExact("foo.ts::program:SharedService:<init>").head
      m.parameter.nameExact("http").typeFullName.headOption shouldBe Option("@angular/common/http:HttpClient")
      cpg.call("post").methodFullName.headOption shouldBe Option("@angular/common/http:HttpClient:post")
    }

  }

  "resolve a function full name called as a constructor" in {
    val cpg = code("""
        |var Print = function(str) {
        |	console.log(str);
        |}
        |
        |new Print("Hello")
        |""".stripMargin)

    cpg.call.nameExact(Defines.OperatorsNew).methodFullName.head shouldBe "Test0.js::program:Print"
  }

  "A function assigned to a member should have it's full name resolved" in {
    val cpg = code("""
        |var foo = {};
        |
        |foo.bar = {};
        |
        |foo.bar.evaluator = function evaluator (src) {
        |    eval(src);
        |};
        |
        |foo.bar.getGlobals = function getGlobals (src) {
        |    "use strict";
        |    var original = Object.keys(global);
        |    foo.bar.evaluator(src);
        |};
        |""".stripMargin)

    cpg.call("evaluator").methodFullName.head shouldBe "Test0.js::program:evaluator"
  }

}
