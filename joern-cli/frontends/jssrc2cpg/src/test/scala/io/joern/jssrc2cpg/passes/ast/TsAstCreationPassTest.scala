package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.AbstractPassTest
import io.joern.jssrc2cpg.passes.Defines
import io.shiftleft.semanticcpg.language._

class TsAstCreationPassTest extends AbstractPassTest {

  "AST generation for simple TS constructs" should {

    "create methods for const exports" in TsAstFixture(
      "export const getApiA = (req: Request) => { const user = req.user as UserDocument; }"
    ) { cpg =>
      cpg.method.name.sorted.l shouldBe List(":program", "anonymous")
      cpg.assignment.code.l shouldBe List(
        "const user = req.user as UserDocument",
        "const getApiA = (req: Request) => { const user = req.user as UserDocument; }",
        "exports.getApiA = getApiA"
      )
      inside(cpg.method.name("anonymous").l) { case List(anon) =>
        anon.fullName shouldBe "code.ts::program:anonymous"
        anon.ast.isIdentifier.name.l shouldBe List("user", "req")
      }
    }

    "have correct structure for import assignments" in TsAstFixture("""
        |import fs = require('fs');
        |import models = require('../models/index');
        |""".stripMargin) { cpg =>
      cpg.assignment.code.l shouldBe List("var fs = require(\"fs\")", "var models = require(\"../models/index\")")
      cpg.local.code.l shouldBe List("fs", "models")
      val List(fsDep, modelsDep) = cpg.dependency.l
      fsDep.name shouldBe "fs"
      fsDep.dependencyGroupId shouldBe Option("fs")
      modelsDep.name shouldBe "models"
      modelsDep.dependencyGroupId shouldBe Option("../models/index")

      val List(fs, models) = cpg.imports.l
      fs.code shouldBe "import fs = require('fs')"
      fs.importedEntity shouldBe Option("fs")
      fs.importedAs shouldBe Option("fs")
      models.code shouldBe "import models = require('../models/index')"
      models.importedEntity shouldBe Option("../models/index")
      models.importedAs shouldBe Option("models")
    }

    "have correct structure for declared functions" in TsAstFixture("declare function foo(arg: string): string") {
      cpg =>
        val List(func) = cpg.method("foo").l
        func.code shouldBe "declare function foo(arg: string): string"
        func.name shouldBe "foo"
        func.fullName shouldBe "code.ts::program:foo"
        val List(_, arg) = cpg.method("foo").parameter.l
        arg.name shouldBe "arg"
        arg.typeFullName shouldBe Defines.String
        arg.code shouldBe "arg: string"
        arg.index shouldBe 1
        val List(parentTypeDecl) = cpg.typeDecl.name(":program").l
        parentTypeDecl.bindsOut.flatMap(_.refOut).l should contain(func)
    }

  }

  "AST generation for TS enums" should {

    "have correct structure for simple enum" in TsAstFixture("""
        |enum Direction {
        |  Up = 1,
        |  Down,
        |  Left,
        |  Right,
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl("Direction").l) { case List(direction) =>
        direction.name shouldBe "Direction"
        direction.code shouldBe "enum Direction"
        direction.fullName shouldBe "code.ts::program:Direction"
        direction.filename shouldBe "code.ts"
        direction.file.name.head shouldBe "code.ts"
        inside(direction.method.name(io.joern.x2cpg.Defines.StaticInitMethodName).l) { case List(init) =>
          init.block.astChildren.isCall.code.head shouldBe "Up = 1"
        }
        inside(cpg.typeDecl("Direction").member.l) { case List(up, down, left, right) =>
          up.name shouldBe "Up"
          up.code shouldBe "Up = 1"
          down.name shouldBe "Down"
          down.code shouldBe "Down"
          left.name shouldBe "Left"
          left.code shouldBe "Left"
          right.name shouldBe "Right"
          right.code shouldBe "Right"
        }
      }
    }

  }

  "AST generation for TS classes" should {

    "have correct structure for simple classes" in TsAstFixture("""
        |class Greeter {
        |  greeting: string;
        |  greet() {
        |    return "Hello, " + this.greeting;
        |  }
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "class Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        val constructor = greeter.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).head
        greeter.method.isConstructor.head shouldBe constructor
        constructor.fullName shouldBe s"code.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting, greet) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
          greet.name shouldBe "greet"
          greet.code should (
            startWith("greet() {") and endWith("}")
          )
        }
      }
    }

    "have correct structure for declared classes with empty constructor" in TsAstFixture("""
        |declare class Greeter {
        |  greeting: string;
        |  constructor(arg: string);
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "class Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        val constructor = greeter.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).head
        constructor.fullName shouldBe s"code.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
        greeter.method.isConstructor.head shouldBe constructor
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
        }
      }
    }

    "have correct modifier" in TsAstFixture("""
        |abstract class Greeter {
        |  static a: string;
        |  private b: string;
        |  public c: string;
        |  protected d: string;
        |  #e: string; // also private
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl.name("Greeter.*").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        cpg.typeDecl.isAbstract.head shouldBe greeter
        greeter.member.isStatic.head shouldBe greeter.member.name("a").head
        greeter.member.isPrivate.l shouldBe greeter.member.name("b", "e").l
        greeter.member.isPublic.head shouldBe greeter.member.name("c").head
        greeter.member.isProtected.head shouldBe greeter.member.name("d").head
      }
    }

    "have correct structure for empty interfaces" in TsAstFixture("""
        |interface A {};
        |interface B {};
        |""".stripMargin) { cpg =>
      cpg.method.fullName.sorted.l shouldBe List(
        "code.ts::program",
        s"code.ts::program:A:${io.joern.x2cpg.Defines.ConstructorMethodName}",
        s"code.ts::program:B:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      )
    }

    "have correct structure for simple interfaces" in TsAstFixture("""
        |interface Greeter {
        |  greeting: string;
        |  name?: string;
        |  [propName: string]: any;
        |  "foo": string;
        |  (source: string, subString: string): boolean;
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "interface Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting, name, propName, foo, func) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
          name.name shouldBe "name"
          name.code shouldBe "name?: string;"
          propName.name shouldBe "propName"
          propName.code shouldBe "[propName: string]: any;"
          foo.name shouldBe "foo"
          foo.code shouldBe "\"foo\": string;"
          func.name shouldBe "anonymous"
          func.code shouldBe "(source: string, subString: string): boolean;"
          func.dynamicTypeHintFullName.head shouldBe "code.ts::program:Greeter:anonymous"
        }
        inside(cpg.typeDecl("Greeter").method.l) { case List(constructor, anon) =>
          constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          constructor.fullName shouldBe s"code.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
          constructor.code shouldBe "new: Greeter"
          greeter.method.isConstructor.head shouldBe constructor
          anon.name shouldBe "anonymous"
          anon.fullName shouldBe "code.ts::program:Greeter:anonymous"
          anon.code shouldBe "(source: string, subString: string): boolean;"
          anon.parameter.name.l shouldBe List("this", "source", "subString")
          anon.parameter.code.l shouldBe List("this", "source: string", "subString: string")
        }
      }
    }

    "have correct structure for interface constructor" in TsAstFixture("""
       |interface Greeter {
       |  new (param: string) : Greeter
       |}
       |""".stripMargin) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "interface Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        inside(cpg.typeDecl("Greeter").method.l) { case List(constructor) =>
          constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          constructor.fullName shouldBe s"code.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
          constructor.code shouldBe "new (param: string) : Greeter"
          constructor.parameter.name.l shouldBe List("this", "param")
          constructor.parameter.code.l shouldBe List("this", "param: string")
          greeter.method.isConstructor.head shouldBe constructor
        }
      }
    }

    "have correct structure for simple namespace" in TsAstFixture("""
       |namespace A {
       |  class Foo {};
       |}
       |""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock("A").l) { case List(a) =>
        a.code should startWith("namespace A")
        a.fullName shouldBe "code.ts::program:A"
        a.typeDecl.name("Foo").head.fullName shouldBe "code.ts::program:A:Foo"
      }
    }

    "have correct structure for nested namespaces" in TsAstFixture("""
        |namespace A {
        |  namespace B {
        |    namespace C {
        |      class Foo {};
        |    }
        |  }
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock("A").l) { case List(a) =>
        a.code should startWith("namespace A")
        a.fullName shouldBe "code.ts::program:A"
        a.astChildren.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(b) =>
        b.code should startWith("namespace B")
        b.fullName shouldBe "code.ts::program:A:B"
        b.astChildren.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(c) =>
        c.code should startWith("namespace C")
        c.fullName shouldBe "code.ts::program:A:B:C"
        c.typeDecl.name("Foo").head.fullName shouldBe "code.ts::program:A:B:C:Foo"
      }
    }

    "have correct structure for nested namespaces with path" in TsAstFixture("""
         |namespace A.B.C {
         |  class Foo {};
         |}
         |""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock("A").l) { case List(a) =>
        a.code should startWith("namespace A")
        a.fullName shouldBe "code.ts::program:A"
        a.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(b) =>
        b.code should startWith("B.C")
        b.fullName shouldBe "code.ts::program:A:B"
        b.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(c) =>
        c.code should startWith("C")
        c.fullName shouldBe "code.ts::program:A:B:C"
        c.typeDecl.name("Foo").head.fullName shouldBe "code.ts::program:A:B:C:Foo"
      }
    }

  }

}
