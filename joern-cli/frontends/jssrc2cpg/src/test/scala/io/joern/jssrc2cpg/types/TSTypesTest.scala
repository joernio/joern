package io.joern.jssrc2cpg.types

import io.joern.jssrc2cpg.passes.AbstractPassTest
import io.joern.jssrc2cpg.passes.Defines
import io.shiftleft.semanticcpg.language._

class TSTypesTest extends AbstractPassTest {
  "have correct types for variables" in AstFixture(
    """
     |var x: string = ""
     |var y: Foo = null
     |""".stripMargin,
    "code.ts"
  ) { cpg =>
    inside(cpg.identifier.l) { case List(x, y) =>
      x.name shouldBe "x"
      x.code shouldBe "x"
      x.typeFullName shouldBe Defines.STRING.label
      y.name shouldBe "y"
      y.code shouldBe "y"
      y.typeFullName shouldBe "Foo"
    }
  }

  "have correct types for TS intrinsics" in AstFixture(
    """
     |type NickName = "user2069"
     |type ModifiedNickName = Uppercase<NickName>
     |var x: ModifiedNickName = ""
     |""".stripMargin,
    "code.ts"
  ) { cpg =>
    inside(cpg.identifier.l) { case List(x) =>
      x.name shouldBe "x"
      x.code shouldBe "x"
      x.typeFullName shouldBe "ModifiedNickName"
    }
  }

  "have correct types for TS function parameters" in AstFixture(
    """
     |function foo(a: string, b: Foo) {}
     |""".stripMargin,
    "code.ts"
  ) { cpg =>
    inside(cpg.method("foo").parameter.l) { case List(_, a, b) =>
      a.name shouldBe "a"
      a.code shouldBe "a: string"
      a.typeFullName shouldBe Defines.STRING.label
      b.name shouldBe "b"
      b.code shouldBe "b: Foo"
      b.typeFullName shouldBe "Foo"
    }
  }

  "have correct types for type alias" in AstFixture(
    """
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |type Alias = ObjectFoo
      |""".stripMargin,
    "code.ts"
  ) { cpg =>
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

  "have correct types for type alias from class" in AstFixture(
    """
     |class Foo {}
     |type Alias = Foo
     |""".stripMargin,
    "code.ts"
  ) { cpg =>
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

  "have correct types for type alias declared first" in AstFixture(
    """
      |type Alias = ObjectFoo
      |type ObjectFoo = {
      |  property: string,
      |  method(): number,
      |}
      |""".stripMargin,
    "code.ts"
  ) { cpg =>
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

  "have correct types for type alias from class defined first" in AstFixture(
    """
     |type Alias = Foo
     |class Foo {}
     |""".stripMargin,
    "code.ts"
  ) { cpg =>
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

  "have correct types for type alias with builtin type" in AstFixture(
    """
      |type Alias = string
      |""".stripMargin,
    "code.ts"
  ) { cpg =>
    cpg.typeDecl("string").l shouldBe empty
    cpg.typeDecl(Defines.STRING.label).size shouldBe 1
    inside(cpg.typeDecl("Alias").l) { case List(alias) =>
      alias.fullName shouldBe "code.ts::program:Alias"
      alias.code shouldBe "type Alias = string"
      alias.aliasTypeFullName shouldBe empty
    }
  }

}
