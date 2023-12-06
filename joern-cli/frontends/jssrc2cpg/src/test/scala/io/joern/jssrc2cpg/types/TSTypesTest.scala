package io.joern.jssrc2cpg.types

import io.joern.jssrc2cpg.passes.AbstractPassTest
import io.joern.jssrc2cpg.passes.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class TSTypesTest extends AbstractPassTest {

  "have correct dynamicTypeHint for this without proper surrounding type" in AstFixture(
    "exports.isAuthorized = function() { this.publicKey }",
    tsTypes = true
  ) { cpg =>
    val List(t) = cpg.identifier("this").l
    t.typeFullName shouldBe Defines.Any
    t.dynamicTypeHintFullName shouldBe List("code.js::program")
  }

  "have correct types for this with proper surrounding type" in AstFixture(
    """
      |class Foo {
      |  publicKey: string = ""
      |  isAuthorized() { return () => { return this.publicKey } }
      |}
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    val List(t) = cpg.identifier("this").l
    t.dynamicTypeHintFullName shouldBe List("code.js::program:Foo")
  }

  "have correct types for empty method with rest parameter" in AstFixture(
    "function method(x, ...args) {}",
    tsTypes = true
  ) { cpg =>
    val List(method) = cpg.method.nameExact("method").l
    method.methodReturn.typeFullName shouldBe "void"

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

  "have return types for arrow functions" in AstFixture("const foo = () => 42;", tsTypes = true) { cpg =>
    val List(foo) = cpg.identifier("foo").l
    foo.typeFullName shouldBe s"() => ${Defines.Number}"
    val List(ret) = cpg.method("<lambda>0").methodReturn.l
    ret.typeFullName shouldBe Defines.Number
  }

  "have correct types for empty method" in AstFixture("function method(x) {}", tsTypes = true) { cpg =>
    val List(method) = cpg.method.nameExact("method").l
    method.methodReturn.typeFullName shouldBe "void"
  }

  "have types for identifiers with type inference" in AstFixture(
    """
      |let x = "test";
      |var y = x;
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe Defines.String
  }

  "have types for identifiers from class" in AstFixture(
    """
      |class Foo {};
      |var y = new Foo();
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe "Foo"
  }

  "have types for parameters" in TsAstFixture(
    """
      |class Foo {};
      |let y = new Foo();
      |function bar(p1: number, p2: string) {
      |  return y;
      |}
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    val List(y1, y2) = cpg.identifier("y").l
    y1.typeFullName shouldBe "Foo"
    y2.typeFullName shouldBe "Foo"
    val List(p1) = cpg.parameter("p1").l
    p1.typeFullName shouldBe Defines.Number
    val List(p2) = cpg.parameter("p2").l
    p2.typeFullName shouldBe Defines.String
    val List(barRet) = cpg.method("bar").methodReturn.l
    barRet.typeFullName shouldBe "Foo"
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
      "code.ts::program",
      "code.ts::program:Foo",
      "code.ts::program:Foo:<init>",
      "code.ts::program:bar"
    ).sorted
  }

  "have correct types for variables" in TsAstFixture(
    """
     |var x: string = "";
     |var y: Foo = null;
     |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    inside(cpg.identifier.l) { case List(x, y) =>
      x.name shouldBe "x"
      x.code shouldBe "x"
      x.typeFullName shouldBe Defines.String
      y.name shouldBe "y"
      y.code shouldBe "y"
      y.typeFullName shouldBe "Foo"
    }
  }

  "have correct types for TS intrinsics" in TsAstFixture(
    """
     |type NickName = "user2069"
     |type ModifiedNickName = Uppercase<NickName>
     |var x: ModifiedNickName = "";
     |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    inside(cpg.identifier.l) { case List(x) =>
      x.name shouldBe "x"
      x.code shouldBe "x"
      x.typeFullName shouldBe Defines.String // we can actually follow type intrinsics
    }
  }

  "have correct types for TS function parameters" in TsAstFixture(
    """
     |function foo(a: string, b: Foo) {}
     |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    inside(cpg.method("foo").parameter.l) { case List(_, a, b) =>
      a.name shouldBe "a"
      a.code shouldBe "a: string"
      a.typeFullName shouldBe Defines.String
      b.name shouldBe "b"
      b.code shouldBe "b: Foo"
      b.typeFullName shouldBe "Foo"
    }
  }

  "have correct types for type alias" in TsAstFixture(
    """
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |type Alias = ObjectFoo
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    inside(cpg.typeDecl("ObjectFoo").l) { case List(objFoo) =>
      objFoo.fullName shouldBe "code.ts::program:ObjectFoo"
      objFoo.aliasTypeFullName shouldBe Option("code.ts::program:Alias")
      objFoo.code shouldBe "type ObjectFoo = {\n  property: string,\n  method(): number,\n}"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = ObjectFoo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias from class" in TsAstFixture(
    """
     |class Foo {}
     |type Alias = Foo
     |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    inside(cpg.typeDecl("Foo").l) { case List(foo) =>
      foo.fullName shouldBe "code.ts::program:Foo"
      foo.aliasTypeFullName shouldBe Option("code.ts::program:Alias")
      foo.code shouldBe "class Foo"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = Foo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias declared first" in TsAstFixture(
    """
      |type Alias = ObjectFoo
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    inside(cpg.typeDecl("ObjectFoo").l) { case List(objFoo) =>
      objFoo.fullName shouldBe "code.ts::program:ObjectFoo"
      objFoo.aliasTypeFullName shouldBe Option("code.ts::program:Alias")
      objFoo.code shouldBe "type ObjectFoo = {\n  property: string,\n  method(): number,\n}"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = ObjectFoo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias from class defined first" in TsAstFixture(
    """
     |type Alias = Foo
     |class Foo {}
     |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    inside(cpg.typeDecl("Foo").l) { case List(foo) =>
      foo.fullName shouldBe "code.ts::program:Foo"
      foo.aliasTypeFullName shouldBe Option("code.ts::program:Alias")
      foo.code shouldBe "class Foo"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = Foo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias with builtin type" in TsAstFixture(
    """
      |type Alias = string
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
    cpg.typeDecl("string").l shouldBe empty
    cpg.typ.fullName(Defines.String).size shouldBe 1
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = string"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for casts" in TsAstFixture(
    """
      |const x = "foo" as string;
      |var y = 1 as int;
      |let z = true as boolean;
      |""".stripMargin,
    tsTypes = true
  ) { cpg =>
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
    cpg.local("y").typeFullName.l shouldBe List("int")
    cpg.identifier("y").typeFullName.l shouldBe List("int")
    cpg.local("z").typeFullName.l shouldBe List(Defines.Boolean)
    cpg.identifier("z").typeFullName.l shouldBe List(Defines.Boolean)
  }

  "have correct types when type is being used multiple times" in TsAstFixture(
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
    tsTypes = true
  ) { cpg =>
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

  "have correct types for cross file import" in TsAstFixture.files(
    """
      |export class Foo {
      |  bar() { return "bar"; }
      |}
      |""".stripMargin,
    "Foo.ts",
    """
      |import * as deps from "./Foo";
      |
      |var x = new deps.Foo().bar();
      |""".stripMargin,
    "index.ts",
    tsTypes = true
  ) { cpg =>
    val List(x) = cpg.identifier("x").l
    x.typeFullName shouldBe Defines.String
  }

}
