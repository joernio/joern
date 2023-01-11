package io.joern.jssrc2cpg.types

import io.joern.jssrc2cpg.passes.AbstractPassTest
import io.joern.jssrc2cpg.passes.Defines
import io.shiftleft.semanticcpg.language._

class TSTypesTest extends AbstractPassTest {

  "have types for identifiers with type inference" in AstFixture("""
      |let x = "test";
      |var y = x;
      |""".stripMargin) { cpg =>
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe Defines.STRING
  }

  "have types for identifiers from class" in AstFixture("""
      |class Foo {};
      |var y = new Foo();
      |""".stripMargin) { cpg =>
    val List(y) = cpg.identifier("y").l
    y.typeFullName shouldBe "Foo"
  }

  "have types for parameters" in TsAstFixture("""
      |class Foo {};
      |let y = new Foo();
      |function bar(p1: number, p2: string) {
      |  return y;
      |}
      |""".stripMargin) { cpg =>
    val List(y1, y2) = cpg.identifier("y").l
    y1.typeFullName shouldBe "Foo"
    y2.typeFullName shouldBe "Foo"
    val List(p1) = cpg.parameter("p1").l
    p1.typeFullName shouldBe Defines.NUMBER
    val List(p2) = cpg.parameter("p2").l
    p2.typeFullName shouldBe Defines.STRING
    val List(barRet) = cpg.method("bar").methodReturn.l
    barRet.typeFullName shouldBe s"(p1: ${Defines.NUMBER}, p2: ${Defines.STRING}) => Foo"
    cpg.typ.name.sorted.l shouldBe (List(
      ":program",
      io.joern.x2cpg.Defines.ConstructorMethodName,
      "Foo",
      "bar"
    ) ++ Defines.JSTYPES).sorted
  }

  "have correct types for variables" in TsAstFixture("""
     |var x: string = "";
     |var y: Foo = null;
     |""".stripMargin) { cpg =>
    inside(cpg.identifier.l) { case List(x, y) =>
      x.name shouldBe "x"
      x.code shouldBe "x"
      x.typeFullName shouldBe Defines.STRING
      y.name shouldBe "y"
      y.code shouldBe "y"
      y.typeFullName shouldBe "Foo"
    }
  }

  "have correct types for TS intrinsics" in TsAstFixture("""
     |type NickName = "user2069"
     |type ModifiedNickName = Uppercase<NickName>
     |var x: ModifiedNickName = "";
     |""".stripMargin) { cpg =>
    inside(cpg.identifier.l) { case List(x) =>
      x.name shouldBe "x"
      x.code shouldBe "x"
      x.typeFullName shouldBe "\"USER2069\"" // we can actually follow type intrinsics
    }
  }

  "have correct types for TS function parameters" in TsAstFixture("""
     |function foo(a: string, b: Foo) {}
     |""".stripMargin) { cpg =>
    inside(cpg.method("foo").parameter.l) { case List(_, a, b) =>
      a.name shouldBe "a"
      a.code shouldBe "a: string"
      a.typeFullName shouldBe Defines.STRING
      b.name shouldBe "b"
      b.code shouldBe "b: Foo"
      b.typeFullName shouldBe "Foo"
    }
  }

  "have correct types for type alias" in TsAstFixture("""
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |type Alias = ObjectFoo
      |""".stripMargin) { cpg =>
    inside(cpg.typeDecl("ObjectFoo").l) { case List(objFoo) =>
      objFoo.fullName shouldBe "code.ts::program:ObjectFoo"
      objFoo.aliasTypeFullName shouldBe Some("code.ts::program:Alias")
      objFoo.code shouldBe "type ObjectFoo = {\n  property: string,\n  method(): number,\n}"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = ObjectFoo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias from class" in TsAstFixture("""
     |class Foo {}
     |type Alias = Foo
     |""".stripMargin) { cpg =>
    inside(cpg.typeDecl("Foo").l) { case List(foo) =>
      foo.fullName shouldBe "code.ts::program:Foo"
      foo.aliasTypeFullName shouldBe Some("code.ts::program:Alias")
      foo.code shouldBe "class Foo"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = Foo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias declared first" in TsAstFixture("""
      |type Alias = ObjectFoo
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |""".stripMargin) { cpg =>
    inside(cpg.typeDecl("ObjectFoo").l) { case List(objFoo) =>
      objFoo.fullName shouldBe "code.ts::program:ObjectFoo"
      objFoo.aliasTypeFullName shouldBe Some("code.ts::program:Alias")
      objFoo.code shouldBe "type ObjectFoo = {\n  property: string,\n  method(): number,\n}"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = ObjectFoo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias from class defined first" in TsAstFixture("""
     |type Alias = Foo
     |class Foo {}
     |""".stripMargin) { cpg =>
    inside(cpg.typeDecl("Foo").l) { case List(foo) =>
      foo.fullName shouldBe "code.ts::program:Foo"
      foo.aliasTypeFullName shouldBe Some("code.ts::program:Alias")
      foo.code shouldBe "class Foo"
    }
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = Foo"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for type alias with builtin type" in TsAstFixture("""
      |type Alias = string
      |""".stripMargin) { cpg =>
    cpg.typeDecl("string").l shouldBe empty
    cpg.typeDecl(Defines.STRING).size shouldBe 1
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = string"
      alias.aliasTypeFullName shouldBe empty
    }
  }

  "have correct types for cross file import" in TsAstFixture(
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
    "index.ts"
  ) { cpg =>
    val List(x) = cpg.identifier("x").l
    x.typeFullName shouldBe Defines.STRING
  }

}
