package io.joern.jssrc2cpg.types

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class TSTypesTests extends AstJsSrc2CpgSuite {

  "have correct dynamicTypeHint for this without proper surrounding type" in {
    val cpg     = code("exports.isAuthorized = function() { this.publicKey }").withConfig(Config().withTsTypes(true))
    val List(t) = cpg.identifier("this").l
    t.typeFullName shouldBe Defines.Any
    t.dynamicTypeHintFullName shouldBe List("Test0.js::program")
  }

  "have correct types for this with proper surrounding type" in {
    val cpg = code("""
      |class Foo {
      |  publicKey: string = ""
      |  isAuthorized() { return () => { return this.publicKey } }
      |}
      |""".stripMargin).withConfig(Config().withTsTypes(true))
    val List(t) = cpg.identifier("this").l
    t.dynamicTypeHintFullName shouldBe List("Test0.js::program:Foo")
  }

  "have correct types for empty method with rest parameter" in {
    val cpg          = code("function method(x, ...args) {}").withConfig(Config().withTsTypes(true))
    val List(method) = cpg.method.nameExact("method").l
    method.methodReturn.typeFullName shouldBe Defines.Any

    val List(t, x, args) = method.parameter.l
    t.index shouldBe 0
    t.name shouldBe "this"
    t.typeFullName shouldBe Defines.Any
    x.index shouldBe 1
    x.name shouldBe "x"
    x.typeFullName shouldBe Defines.Any
    args.index shouldBe 2
    args.name shouldBe "args"
    args.code shouldBe "...args"
    args.isVariadic shouldBe true
    args.typeFullName shouldBe Defines.Any
  }

  "have return types for arrow functions" in {
    val cpg       = code("const foo = () => 42;").withConfig(Config().withTsTypes(true))
    val List(foo) = cpg.identifier("foo").l
    foo.typeFullName shouldBe Defines.Any
    foo.possibleTypes shouldBe Seq(s"() => ${Defines.Number}")
    val List(ret) = cpg.method("<lambda>0").methodReturn.l
    ret.typeFullName shouldBe Defines.Number
  }

  "have correct types for empty method" in {
    val cpg          = code("function method(x) {}").withConfig(Config().withTsTypes(true))
    val List(method) = cpg.method.nameExact("method").l
    method.methodReturn.typeFullName shouldBe Defines.Any
  }

  "have types for identifiers with type inference" in {
    val cpg = code("""
      |let x = "test";
      |var y = x;
      |""".stripMargin).withConfig(Config().withTsTypes(true))
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe Defines.String
  }

  "have types for identifiers from class" in {
    val cpg = code("""
      |class Foo {};
      |var y = new Foo();
      |""".stripMargin).withConfig(Config().withTsTypes(true))
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe Defines.Any
    y.possibleTypes shouldBe Seq("Foo")
  }

  "have types for parameters" in {
    val cpg = code(
      """
      |class Foo {};
      |let y = new Foo();
      |function bar(p1: number, p2: string) {
      |  return y;
      |}
      |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    val List(y1, y2) = cpg.identifier("y").l
    y1.typeFullName shouldBe Defines.Any
    y1.possibleTypes shouldBe Seq("Foo")
    y2.typeFullName shouldBe Defines.Any
    y2.possibleTypes shouldBe Seq("Foo")
    val List(p1) = cpg.parameter("p1").l
    p1.typeFullName shouldBe Defines.Number
    val List(p2) = cpg.parameter("p2").l
    p2.typeFullName shouldBe Defines.String
    val List(barRet) = cpg.method("bar").methodReturn.l
    barRet.typeFullName shouldBe Defines.Any
    barRet.possibleTypes shouldBe Seq("Foo")
    cpg.typ.name.sorted.l shouldBe List(
      ":program",
      io.joern.x2cpg.Defines.ConstructorMethodName,
      "ANY",
      "Foo",
      "Foo",
      "Number",
      "String",
      "bar"
    ).sorted
    cpg.typ.fullName.sorted.l shouldBe List(
      "ANY",
      "Foo",
      "__ecma.Number",
      "__ecma.String",
      "Test0.ts::program",
      "Test0.ts::program:Foo",
      "Test0.ts::program:Foo:<init>",
      "Test0.ts::program:bar"
    ).sorted
  }

  "have correct types for variables" in {
    val cpg = code(
      """
     |var x: string = "";
     |var y: Foo = null;
     |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    inside(cpg.identifier.l) { case List(idX, idY) =>
      idX.name shouldBe "x"
      idX.code shouldBe "x"
      idX.typeFullName shouldBe Defines.String
      idY.name shouldBe "y"
      idY.code shouldBe "y"
      idY.typeFullName shouldBe Defines.Any
      idY.possibleTypes shouldBe Seq("Foo")
    }
  }

  "have correct types for TS intrinsics" in {
    val cpg = code(
      """
     |type NickName = "user2069"
     |type ModifiedNickName = Uppercase<NickName>
     |var x: ModifiedNickName = "";
     |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    inside(cpg.identifier.l) { case List(idX) =>
      idX.name shouldBe "x"
      idX.code shouldBe "x"
      idX.typeFullName shouldBe Defines.String // we can actually follow type intrinsics
    }
  }

  "have correct types for TS function parameters" in {
    val cpg = code(
      """
     |function foo(a: string, b: Foo) {}
     |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    inside(cpg.method("foo").parameter.l) { case List(_, paramA, paramB) =>
      paramA.name shouldBe "a"
      paramA.code shouldBe "a: string"
      paramA.typeFullName shouldBe Defines.String
      paramB.name shouldBe "b"
      paramB.code shouldBe "b: Foo"
      paramB.typeFullName shouldBe Defines.Any
      paramB.possibleTypes shouldBe Seq("Foo")
    }
  }

  "have correct types for type alias" in {
    val cpg = code(
      """
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |type Alias = ObjectFoo
      |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    inside(cpg.typeDecl("ObjectFoo").l) { case List(objFoo) =>
      objFoo.fullName shouldBe "Test0.ts::program:ObjectFoo"
      objFoo.aliasTypeFullName shouldBe Option("Test0.ts::program:Alias")
      objFoo.code shouldBe "type ObjectFoo = {\n  property: string,\n  method(): number,\n}"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "Test0.ts::program:Alias"
      alias.code shouldBe "type Alias = ObjectFoo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias from class" in {
    val cpg = code(
      """
     |class Foo {}
     |type Alias = Foo
     |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    inside(cpg.typeDecl("Foo").l) { case List(foo) =>
      foo.fullName shouldBe "Test0.ts::program:Foo"
      foo.aliasTypeFullName shouldBe Option("Test0.ts::program:Alias")
      foo.code shouldBe "class Foo"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "Test0.ts::program:Alias"
      alias.code shouldBe "type Alias = Foo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias declared first" in {
    val cpg = code(
      """
      |type Alias = ObjectFoo
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    inside(cpg.typeDecl("ObjectFoo").l) { case List(objFoo) =>
      objFoo.fullName shouldBe "Test0.ts::program:ObjectFoo"
      objFoo.aliasTypeFullName shouldBe Option("Test0.ts::program:Alias")
      objFoo.code shouldBe "type ObjectFoo = {\n  property: string,\n  method(): number,\n}"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "Test0.ts::program:Alias"
      alias.code shouldBe "type Alias = ObjectFoo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias from class defined first" in {
    val cpg = code(
      """
     |type Alias = Foo
     |class Foo {}
     |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    inside(cpg.typeDecl("Foo").l) { case List(foo) =>
      foo.fullName shouldBe "Test0.ts::program:Foo"
      foo.aliasTypeFullName shouldBe Option("Test0.ts::program:Alias")
      foo.code shouldBe "class Foo"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "Test0.ts::program:Alias"
      alias.code shouldBe "type Alias = Foo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias with builtin type" in {
    val cpg = code(
      """
      |type Alias = string
      |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    cpg.typeDecl("string").l shouldBe empty
    cpg.typ.fullName(Defines.String).size shouldBe 1
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "Test0.ts::program:Alias"
      alias.code shouldBe "type Alias = string"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for casts" in {
    val cpg = code(
      """
      |const x = "foo" as string;
      |var y = 1 as int;
      |let z = true as boolean;
      |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    cpg.assignment.code.l shouldBe List("const x = \"foo\" as string", "var y = 1 as int", "let z = true as boolean")
    inside(cpg.call(Operators.cast).l) { case List(callX, callY, callZ) =>
      callX.argument(1).code shouldBe "string"
      callX.argument(2).code shouldBe "\"foo\""
      callY.argument(1).code shouldBe "int"
      callY.argument(2).code shouldBe "1"
      callZ.argument(1).code shouldBe "boolean"
      callZ.argument(2).code shouldBe "true"
    }
    cpg.local("x").typeFullName.l shouldBe List(Defines.String)
    cpg.identifier("x").typeFullName.l shouldBe List(Defines.String)
    cpg.local("y").typeFullName.l shouldBe List(Defines.Number)
    cpg.identifier("y").typeFullName.l shouldBe List(Defines.Number)
    cpg.local("z").typeFullName.l shouldBe List(Defines.Boolean)
    cpg.identifier("z").typeFullName.l shouldBe List(Defines.Boolean)
  }

  "have correct types when type is being used multiple times" in {
    val cpg = code(
      """
      |import { Response, Request, NextFunction } from "express";
      |import { UserDocument } from "../models/User";
      |
      |type CustomResponse = {
      |    render: (arg0: string) => void;
      |}
      |
      |export const getApiA = (req: Request) => {
      |    const user = req.user as UserDocument;
      |};
      |
      |function getApiB(res: Response): void {
      |    res.render("api/index", {
      |        title: "API Examples"
      |    });
      |}
      |
      |function getApiC(res: CustomResponse): void {
      |    res.render("api/index");
      |}
      |
      |function getFoo(req: Request, res: Response): void {
      |    const user = req.user as UserDocument;
      |    const token = user.tokens.find((token: any) => token.kind === "foo");
      |    res.render("api/foo", {
      |        title: "foo API",
      |        profile: "Test"
      |    });
      |};
      |""".stripMargin,
      "Test0.ts"
    ).withConfig(Config().withTsTypes(true))
    cpg.typ.name.l should contain allElementsOf List(
      ":program",
      "getApiB",
      "getApiC",
      "<lambda>0",
      "getFoo",
      "<lambda>1",
      "CustomResponse",
      "Request",
      "Response",
      "UserDocument"
    )
  }

  "have correct types for cross file import" in {
    val cpg = code(
      """
       |export class Foo {
       |  bar() { return "bar"; }
       |}
       |""".stripMargin,
      "Foo.ts"
    ).moreCode(
      """
       |import * as deps from "./Foo";
       |
       |var x = new deps.Foo().bar();
       |""".stripMargin,
      "index.ts"
    ).withConfig(Config().withTsTypes(true))
    val List(x) = cpg.identifier("x").l
    x.typeFullName shouldBe Defines.String
  }

  "have correct types various array types" in {
    val cpg = code(
      """
        |function foo(a: string[], b: Bar[], c: (() => string)[], d: { x: string, y: number }[]) {}
        |""".stripMargin,
      "Main.ts"
    ).withConfig(Config().withTsTypes(true))
    cpg.method.nameExact("foo").parameter.indexNot(0).typeFullName.sorted.distinct.l shouldBe List(Defines.Array)
  }

}
