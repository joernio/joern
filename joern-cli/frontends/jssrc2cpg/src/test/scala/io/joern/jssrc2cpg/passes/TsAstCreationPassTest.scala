package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.Import
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.shiftleft.semanticcpg.language._

class TsAstCreationPassTest extends AbstractPassTest {

  "AST generation for simple TS constructs" should {

    "have correct structure for casts" in AstFixture(
      """
        | const x = "foo" as string;
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
      inside(cpg.call(Operators.cast).l) { case List(call) =>
        call.argument(1).head.code shouldBe "string"
        call.argument(2).head.code shouldBe "\"foo\""
      }
    }

    "have correct structure for import assignments" in AstFixture(
      """
        |import fs = require('fs')
        |import models = require('../models/index')
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
      def fsDep = getDependencies(cpg).filter(PropertyNames.NAME, "fs")
      fsDep.checkNodeCount(1)
      fsDep.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "fs")

      def modelsDep = getDependencies(cpg).filter(PropertyNames.NAME, "models")
      modelsDep.checkNodeCount(1)
      modelsDep.checkProperty(PropertyNames.DEPENDENCY_GROUP_ID, "../models/index")

      val List(fs: Import, models: Import) = getImports(cpg).l
      fs.code shouldBe "import fs = require('fs')"
      fs.importedEntity shouldBe Some("fs")
      fs.importedAs shouldBe Some("fs")
      models.code shouldBe "import models = require('../models/index')"
      models.importedEntity shouldBe Some("../models/index")
      models.importedAs shouldBe Some("models")
    }

    "have correct structure for declared functions" in AstFixture(
      """
        |declare function foo(arg: string): string
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
      val List(func: Method) = cpg.method("foo").l
      func.code shouldBe "declare function foo(arg: string): string"
      func.name shouldBe "foo"
      func.fullName shouldBe "code.ts::program:foo"
      val List(_, arg: MethodParameterIn) = cpg.method("foo").parameter.l
      arg.name shouldBe "arg"
      arg.typeFullName shouldBe Defines.STRING.label
      arg.code shouldBe "arg: string"
      arg.index shouldBe 1
      val List(parentTypeDecl) = cpg.typeDecl.name(":program").l
      parentTypeDecl.bindsOut.expandRef().l should contain(func)
    }

  }

  "AST generation for TS enums" should {

    "have correct structure for simple enum" in AstFixture(
      """
        |enum Direction {
        |  Up = 1,
        |  Down,
        |  Left,
        |  Right,
        |}
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
      inside(cpg.typeDecl("Direction").l) { case List(direction) =>
        direction.name shouldBe "Direction"
        direction.code shouldBe "enum Direction"
        direction.fullName shouldBe "code.ts::program:Direction"
        direction.filename shouldBe "code.ts"
        direction.file.name.head shouldBe "code.ts"
        inside(direction.method.name("<sinit>").l) { case List(init) =>
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

    "have correct structure for simple classes" in AstFixture(
      """
        |class Greeter {
        |  greeting: string;
        |  greet() {
        |    return "Hello, " + this.greeting;
        |  }
        |}
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "class Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        val constructor = greeter.method.name("Greeter<constructor>").head
        greeter.method.isConstructor.head shouldBe constructor
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

    "have correct structure for declared classes with empty constructor" in AstFixture(
      """
        |declare class Greeter {
        |  greeting: string;
        |  constructor(arg: string);
        |}
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "class Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        val constructor = greeter.method.name("Greeter<constructor>").head
        greeter.method.isConstructor.head shouldBe constructor
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
        }
      }
    }

    "have correct modifier" in AstFixture(
      """
        |abstract class Greeter {
        |  static a: string;
        |  private b: string;
        |  public c: string;
        |  protected d: string;
        |}
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
      inside(cpg.typeDecl.name("Greeter.*").l) { case List(greeter, greeterMeta) =>
        greeterMeta.name shouldBe "Greeter<meta>"
        greeter.name shouldBe "Greeter"
        cpg.typeDecl.isAbstract.head shouldBe greeter
        greeterMeta.member.isStatic.head shouldBe greeterMeta.member.name("a").head
        greeter.member.isPrivate.head shouldBe greeter.member.name("b").head
        greeter.member.isPublic.head shouldBe greeter.member.name("c").head
        greeter.member.isProtected.head shouldBe greeter.member.name("d").head
      }
    }

    "have correct structure for simple interfaces" in AstFixture(
      """
        |interface Greeter {
        |  greeting: string;
        |  name?: string;
        |  [propName: string]: any;
        |  "foo": string;
        |  (source: string, subString: string): boolean;
        |}
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
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
          constructor.name shouldBe "Greeter<constructor>"
          constructor.fullName shouldBe "code.ts::program:Greeter<constructor>"
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

    "have correct structure for interface constructor" in AstFixture(
      """
       |interface Greeter {
       |  new (param: string) : Greeter
       |}
       |""".stripMargin,
      "code.ts"
    ) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "interface Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        inside(cpg.typeDecl("Greeter").method.l) { case List(constructor) =>
          constructor.name shouldBe "Greeter<constructor>"
          constructor.fullName shouldBe "code.ts::program:Greeter<constructor>"
          constructor.code shouldBe "new (param: string) : Greeter"
          constructor.parameter.name.l shouldBe List("this", "param")
          constructor.parameter.code.l shouldBe List("this", "param: string")
          greeter.method.isConstructor.head shouldBe constructor
        }
      }
    }

    "have correct structure for simple namespace" in AstFixture(
      """
       |namespace A {
       |  class Foo {};
       |}
       |""".stripMargin,
      "code.ts"
    ) { cpg =>
      inside(cpg.namespaceBlock("A").l) { case List(a) =>
        a.code should startWith("namespace A")
        a.fullName shouldBe "code.ts::program:A"
        a.typeDecl.name("Foo").head.fullName shouldBe "code.ts::program:A:Foo"
      }
    }

    "have correct structure for nested namespaces" in AstFixture(
      """
        |namespace A {
        |  namespace B {
        |    namespace C {
        |      class Foo {};
        |    }
        |  }
        |}
        |""".stripMargin,
      "code.ts"
    ) { cpg =>
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

    "have correct structure for nested namespaces with path" in AstFixture(
      """
         |namespace A.B.C {
         |  class Foo {};
         |}
         |""".stripMargin,
      "code.ts"
    ) { cpg =>
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
