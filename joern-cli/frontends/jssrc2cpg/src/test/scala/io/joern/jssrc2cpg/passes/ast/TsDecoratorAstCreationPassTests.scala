package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.semanticcpg.language.*

class TsDecoratorAstCreationPassTests extends AstJsSrc2CpgSuite(".ts") {

  "AST generation for TS decorator" should {

    "create annotations correctly for methods" in {
      val cpg = code("""
        |class Greeter {
        |  @a(false)
        |  @b(foo)
        |  @c(foo=false)
        |  @d()
        |  greet() {
        |    return "Hello";
        |  }
        |}""".stripMargin)
      inside(cpg.typeDecl.name("Greeter").method.name("greet").annotation.l) {
        case List(annotationA, annotationB, annotationC, annotationD) =>
          annotationA.code shouldBe "@a(false)"
          annotationA.name shouldBe "a"
          annotationA.fullName shouldBe "a"
          val List(paramAssignA) = annotationA.parameterAssign.l
          paramAssignA.code shouldBe "false"
          paramAssignA.order shouldBe 1
          val List(paramA) = paramAssignA.parameter.l
          paramA.code shouldBe "value"
          paramA.order shouldBe 1
          val List(paramValueA) = paramAssignA.value.l
          paramValueA.code shouldBe "false"
          paramValueA.order shouldBe 2
          paramValueA.argumentIndex shouldBe 2

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"
          val List(paramAssignB) = annotationB.parameterAssign.l
          paramAssignB.code shouldBe "foo"
          paramAssignB.order shouldBe 1
          val List(paramB) = paramAssignB.parameter.l
          paramB.code shouldBe "value"
          paramB.order shouldBe 1
          val List(paramValueB) = paramAssignB.value.l
          paramValueB.code shouldBe "foo"
          paramValueB.order shouldBe 2
          paramValueB.argumentIndex shouldBe 2

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"
          val List(paramAssignC) = annotationC.parameterAssign.l
          paramAssignC.code shouldBe "foo=false"
          paramAssignC.order shouldBe 1
          val List(paramC) = paramAssignC.parameter.l
          paramC.code shouldBe "foo"
          paramC.order shouldBe 1
          val List(paramValueC) = paramAssignC.value.l
          paramValueC.code shouldBe "false"
          paramValueC.order shouldBe 2
          paramValueC.argumentIndex shouldBe 2

          annotationD.code shouldBe "@d()"
          annotationD.name shouldBe "d"
          annotationD.fullName shouldBe "d"
          annotationD.parameterAssign.l shouldBe empty
      }
    }

    "create annotations correctly for method parameter" in {
      val cpg = code("""
        |class Greeter {
        |  greet(@c(foo=false) x: number) {
        |    return "Hello";
        |  }
        |}""".stripMargin)
      inside(cpg.typeDecl.name("Greeter").method.name("greet").parameter.name("x").annotation.l) { case List(c) =>
        c.code shouldBe "@c(foo=false)"
        c.name shouldBe "c"
        c.fullName shouldBe "c"
        val List(paramAssignC) = c.parameterAssign.l
        paramAssignC.code shouldBe "foo=false"
        paramAssignC.order shouldBe 1
        val List(paramC) = paramAssignC.parameter.l
        paramC.code shouldBe "foo"
        paramC.order shouldBe 1
        val List(paramValueC) = paramAssignC.value.l
        paramValueC.code shouldBe "false"
        paramValueC.order shouldBe 2
        paramValueC.argumentIndex shouldBe 2
      }
    }

    "create annotations with full names correctly" in {
      val cpg = code("""
        |class Foo {
        |  foo(@a.b.c(foo=false) x: number) {
        |    return "Hello";
        |  }
        |  bar(@a.b.c x: number) {
        |    return "Hello";
        |  }
        |}""".stripMargin)
      inside(cpg.typeDecl.name("Foo").method.name("foo").parameter.name("x").annotation.l) { case List(c) =>
        c.code shouldBe "@a.b.c(foo=false)"
        c.name shouldBe "c"
        c.fullName shouldBe "a.b.c"
      }
      inside(cpg.typeDecl.name("Foo").method.name("bar").parameter.name("x").annotation.l) { case List(c) =>
        c.code shouldBe "@a.b.c"
        c.name shouldBe "c"
        c.fullName shouldBe "a.b.c"
      }
    }

    "create annotations correctly for classes" in {
      val cpg = code("""
        |@a(false)
        |@b(foo)
        |@c(foo=false)
        |@d()
        |class Greeter {}
        |""".stripMargin)
      inside(cpg.typeDecl.name("Greeter").annotation.l) {
        case List(annotationA, annotationB, annotationC, annotationD) =>
          annotationA.code shouldBe "@a(false)"
          annotationA.name shouldBe "a"
          annotationA.fullName shouldBe "a"
          val List(paramAssignA) = annotationA.parameterAssign.l
          paramAssignA.code shouldBe "false"
          paramAssignA.order shouldBe 1
          val List(paramA) = paramAssignA.parameter.l
          paramA.code shouldBe "value"
          paramA.order shouldBe 1
          val List(paramValueA) = paramAssignA.value.l
          paramValueA.code shouldBe "false"
          paramValueA.order shouldBe 2
          paramValueA.argumentIndex shouldBe 2

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"
          val List(paramAssignB) = annotationB.parameterAssign.l
          paramAssignB.code shouldBe "foo"
          paramAssignB.order shouldBe 1
          val List(paramB) = paramAssignB.parameter.l
          paramB.code shouldBe "value"
          paramB.order shouldBe 1
          val List(paramValueB) = paramAssignB.value.l
          paramValueB.code shouldBe "foo"
          paramValueB.order shouldBe 2
          paramValueB.argumentIndex shouldBe 2

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"
          val List(paramAssignC) = annotationC.parameterAssign.l
          paramAssignC.code shouldBe "foo=false"
          paramAssignC.order shouldBe 1
          val List(paramC) = paramAssignC.parameter.l
          paramC.code shouldBe "foo"
          paramC.order shouldBe 1
          val List(paramValueC) = paramAssignC.value.l
          paramValueC.code shouldBe "false"
          paramValueC.order shouldBe 2
          paramValueC.argumentIndex shouldBe 2

          annotationD.code shouldBe "@d()"
          annotationD.name shouldBe "d"
          annotationD.fullName shouldBe "d"
          annotationD.parameterAssign.l shouldBe empty
      }
    }

    "create annotations correctly for class members" in {
      val cpg = code("""
        |class Greeter {
        |  @a(false)
        |  @b(foo)
        |  @c(foo=false)
        |  @d()
        |  greeting: string;
        |}
        |""".stripMargin)
      inside(cpg.typeDecl.name("Greeter").member.name("greeting").annotation.l) {
        case List(annotationA, annotationB, annotationC, annotationD) =>
          annotationA.code shouldBe "@a(false)"
          annotationA.name shouldBe "a"
          annotationA.fullName shouldBe "a"
          val List(paramAssignA) = annotationA.parameterAssign.l
          paramAssignA.code shouldBe "false"
          paramAssignA.order shouldBe 1
          val List(paramA) = paramAssignA.parameter.l
          paramA.code shouldBe "value"
          paramA.order shouldBe 1
          val List(paramValueA) = paramAssignA.value.l
          paramValueA.code shouldBe "false"
          paramValueA.order shouldBe 2
          paramValueA.argumentIndex shouldBe 2

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"
          val List(paramAssignB) = annotationB.parameterAssign.l
          paramAssignB.code shouldBe "foo"
          paramAssignB.order shouldBe 1
          val List(paramB) = paramAssignB.parameter.l
          paramB.code shouldBe "value"
          paramB.order shouldBe 1
          val List(paramValueB) = paramAssignB.value.l
          paramValueB.code shouldBe "foo"
          paramValueB.order shouldBe 2
          paramValueB.argumentIndex shouldBe 2

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"
          val List(paramAssignC) = annotationC.parameterAssign.l
          paramAssignC.code shouldBe "foo=false"
          paramAssignC.order shouldBe 1
          val List(paramC) = paramAssignC.parameter.l
          paramC.code shouldBe "foo"
          paramC.order shouldBe 1
          val List(paramValueC) = paramAssignC.value.l
          paramValueC.code shouldBe "false"
          paramValueC.order shouldBe 2
          paramValueC.argumentIndex shouldBe 2

          annotationD.code shouldBe "@d()"
          annotationD.name shouldBe "d"
          annotationD.fullName shouldBe "d"
          annotationD.parameterAssign.l shouldBe empty
      }
    }

    "create annotations with literals correctly for class members" in {
      val cpg = code("""
        |class Foo {
        |  @a('lit')
        |  public x: number;
        |}
        |""".stripMargin)
      inside(cpg.typeDecl.name("Foo").member.name("x").annotation.l) { case List(annotationA) =>
        annotationA.code shouldBe "@a('lit')"
        annotationA.name shouldBe "a"
        annotationA.fullName shouldBe "a"
        val List(assign) = annotationA.parameterAssign.l
        assign.code shouldBe "'lit'"
        assign.order shouldBe 1
        val List(paramA) = assign.parameter.l
        paramA.code shouldBe "value"
        paramA.order shouldBe 1
        val List(lit) = assign.value.isLiteral.l
        lit.code shouldBe "\"lit\""
        lit.order shouldBe 2
        lit.argumentIndex shouldBe 2
        lit.parentExpression.code.head shouldBe "@a('lit')"
      }
    }

    "create annotations correctly for class accessors" in {
      val cpg = code("""
        |class Foo {
        |  private _x: number;
        |
        |  @a(false)
        |  @b(foo)
        |  @c(foo=false)
        |  @d()
        |  get x() {
        |    return this._x;
        |  }
        |}""".stripMargin)
      inside(cpg.typeDecl.name("Foo").method.name("x").annotation.l) {
        case List(annotationA, annotationB, annotationC, annotationD) =>
          annotationA.code shouldBe "@a(false)"
          annotationA.name shouldBe "a"
          annotationA.fullName shouldBe "a"
          val List(paramAssignA) = annotationA.parameterAssign.l
          paramAssignA.code shouldBe "false"
          paramAssignA.order shouldBe 1
          val List(paramA) = paramAssignA.parameter.l
          paramA.code shouldBe "value"
          paramA.order shouldBe 1
          val List(paramValueA) = paramAssignA.value.l
          paramValueA.code shouldBe "false"
          paramValueA.order shouldBe 2
          paramValueA.argumentIndex shouldBe 2

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"
          val List(paramAssignB) = annotationB.parameterAssign.l
          paramAssignB.code shouldBe "foo"
          paramAssignB.order shouldBe 1
          val List(paramB) = paramAssignB.parameter.l
          paramB.code shouldBe "value"
          paramB.order shouldBe 1
          val List(paramValueB) = paramAssignB.value.l
          paramValueB.code shouldBe "foo"
          paramValueB.order shouldBe 2
          paramValueB.argumentIndex shouldBe 2

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"
          val List(paramAssignC) = annotationC.parameterAssign.l
          paramAssignC.code shouldBe "foo=false"
          paramAssignC.order shouldBe 1
          val List(paramC) = paramAssignC.parameter.l
          paramC.code shouldBe "foo"
          paramC.order shouldBe 1
          val List(paramValueC) = paramAssignC.value.l
          paramValueC.code shouldBe "false"
          paramValueC.order shouldBe 2
          paramValueC.argumentIndex shouldBe 2

          annotationD.code shouldBe "@d()"
          annotationD.name shouldBe "d"
          annotationD.fullName shouldBe "d"
          annotationD.parameterAssign.l shouldBe empty
      }
    }

    "create methods for const exports" in {
      val cpg = code("export const getApiA = (req: Request) => { const user = req.user as UserDocument; }")
      cpg.method.name.sorted.l shouldBe List(":program", "<lambda>0")
      cpg.assignment.code.l shouldBe List(
        "const user = req.user as UserDocument",
        "const getApiA = (req: Request) => { const user = req.user as UserDocument; }",
        "exports.getApiA = getApiA"
      )
      inside(cpg.method.name("<lambda>0").l) { case List(anon) =>
        anon.fullName shouldBe "Test0.ts::program:<lambda>0"
        anon.ast.isIdentifier.name.l shouldBe List("user", "req")
      }
    }

    "have correct structure for import assignments" in {
      val cpg = code("""
        |import fs = require('fs');
        |import models = require('../models/index');
        |""".stripMargin)
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

    "have correct structure for declared functions" in {
      val cpg        = code("declare function foo(arg: string): string")
      val List(func) = cpg.method("foo").l
      func.code shouldBe "declare function foo(arg: string): string"
      func.name shouldBe "foo"
      func.fullName shouldBe "Test0.ts::program:foo"
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

    "have correct structure for simple enum" in {
      val cpg = code("""
        |enum Direction {
        |  Up = 1,
        |  Down,
        |  Left,
        |  Right,
        |}
        |""".stripMargin)
      inside(cpg.typeDecl("Direction").l) { case List(direction) =>
        direction.name shouldBe "Direction"
        direction.code shouldBe "enum Direction"
        direction.fullName shouldBe "Test0.ts::program:Direction"
        direction.filename shouldBe "Test0.ts"
        direction.file.name.head shouldBe "Test0.ts"
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

    "have correct structure for simple classes" in {
      val cpg = code("""
        |class Greeter {
        |  greeting: string;
        |  greet() {
        |    return "Hello, " + this.greeting;
        |  }
        |}
        |""".stripMargin)
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "class Greeter"
        greeter.fullName shouldBe "Test0.ts::program:Greeter"
        greeter.filename shouldBe "Test0.ts"
        greeter.file.name.head shouldBe "Test0.ts"
        val constructor = greeter.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).head
        greeter.method.isConstructor.head shouldBe constructor
        constructor.fullName shouldBe s"Test0.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting, greet) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
          greet.name shouldBe "greet"
          greet.dynamicTypeHintFullName shouldBe Seq("Test0.ts::program:Greeter:greet")
        }
      }
    }

    "have correct structure for declared classes with empty constructor" in {
      val cpg = code("""
        |declare class Greeter {
        |  greeting: string;
        |  constructor(arg: string);
        |}
        |""".stripMargin)
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "class Greeter"
        greeter.fullName shouldBe "Test0.ts::program:Greeter"
        greeter.filename shouldBe "Test0.ts"
        greeter.file.name.head shouldBe "Test0.ts"
        val constructor = greeter.method.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).head
        constructor.fullName shouldBe s"Test0.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
        greeter.method.isConstructor.head shouldBe constructor
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
        }
      }
    }

    "have correct modifier" in {
      val cpg = code("""
        |abstract class Greeter {
        |  static a: string;
        |  private b: string;
        |  public c: string;
        |  protected d: string;
        |  #e: string; // also private
        |}
        |""".stripMargin)
      inside(cpg.typeDecl.name("Greeter.*").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        cpg.typeDecl.isAbstract.head shouldBe greeter
        greeter.member.isStatic.head shouldBe greeter.member.name("a").head
        greeter.member.isPrivate.l shouldBe greeter.member.name("b", "e").l
        greeter.member.isPublic.head shouldBe greeter.member.name("c").head
        greeter.member.isProtected.head shouldBe greeter.member.name("d").head
      }
    }

    "have correct structure for empty interfaces" in {
      val cpg = code("""
        |interface A {};
        |interface B {};
        |""".stripMargin)
      cpg.method.fullName.sorted.l shouldBe List(
        "Test0.ts::program",
        s"Test0.ts::program:A:${io.joern.x2cpg.Defines.ConstructorMethodName}",
        s"Test0.ts::program:B:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      )
    }

    "have correct structure for simple interfaces" in {
      val cpg = code("""
        |interface Greeter {
        |  greeting: string;
        |  name?: string;
        |  [propName: string]: any;
        |  "foo": string;
        |  (source: string, subString: string): boolean;
        |}
        |""".stripMargin)
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "interface Greeter"
        greeter.fullName shouldBe "Test0.ts::program:Greeter"
        greeter.filename shouldBe "Test0.ts"
        greeter.file.name.head shouldBe "Test0.ts"
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting, name, propName, foo, anon) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
          name.name shouldBe "name"
          name.code shouldBe "name?: string;"
          propName.name shouldBe "propName"
          propName.code shouldBe "[propName: string]: any;"
          foo.name shouldBe "foo"
          foo.code shouldBe "\"foo\": string;"
          anon.name shouldBe "<lambda>0"
          anon.dynamicTypeHintFullName shouldBe Seq("Test0.ts::program:Greeter:<lambda>0")
          anon.code shouldBe "(source: string, subString: string): boolean;"
        }
        inside(cpg.typeDecl("Greeter").method.l) { case List(constructor, anon) =>
          constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          constructor.fullName shouldBe s"Test0.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
          constructor.code shouldBe "new: Greeter"
          greeter.method.isConstructor.head shouldBe constructor
          anon.name shouldBe "<lambda>0"
          anon.fullName shouldBe "Test0.ts::program:Greeter:<lambda>0"
          anon.code shouldBe "(source: string, subString: string): boolean;"
          anon.parameter.name.l shouldBe List("this", "source", "subString")
          anon.parameter.code.l shouldBe List("this", "source: string", "subString: string")
        }
      }
    }

    "have correct structure for interface constructor" in {
      val cpg = code("""
       |interface Greeter {
       |  new (param: string) : Greeter
       |}
       |""".stripMargin)
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "interface Greeter"
        greeter.fullName shouldBe "Test0.ts::program:Greeter"
        greeter.filename shouldBe "Test0.ts"
        greeter.file.name.head shouldBe "Test0.ts"
        inside(cpg.typeDecl("Greeter").method.l) { case List(constructor) =>
          constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          constructor.fullName shouldBe s"Test0.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
          constructor.code shouldBe "new (param: string) : Greeter"
          constructor.parameter.name.l shouldBe List("this", "param")
          constructor.parameter.code.l shouldBe List("this", "param: string")
          greeter.method.isConstructor.head shouldBe constructor
        }
      }
    }

    "have correct structure for simple namespace" in {
      val cpg = code("""
       |namespace A {
       |  class Foo {};
       |}
       |""".stripMargin)
      inside(cpg.namespaceBlock("A").l) { case List(namespaceA) =>
        namespaceA.code should startWith("namespace A")
        namespaceA.fullName shouldBe "Test0.ts::program:A"
        namespaceA.typeDecl.name("Foo").head.fullName shouldBe "Test0.ts::program:A:Foo"
      }
    }

    "have correct structure for nested namespaces" in {
      val cpg = code("""
        |namespace A {
        |  namespace B {
        |    namespace C {
        |      class Foo {};
        |    }
        |  }
        |}
        |""".stripMargin)
      inside(cpg.namespaceBlock("A").l) { case List(namespaceA) =>
        namespaceA.code should startWith("namespace A")
        namespaceA.fullName shouldBe "Test0.ts::program:A"
        namespaceA.astChildren.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(namespaceB) =>
        namespaceB.code should startWith("namespace B")
        namespaceB.fullName shouldBe "Test0.ts::program:A:B"
        namespaceB.astChildren.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(namespaceC) =>
        namespaceC.code should startWith("namespace C")
        namespaceC.fullName shouldBe "Test0.ts::program:A:B:C"
        namespaceC.typeDecl.name("Foo").head.fullName shouldBe "Test0.ts::program:A:B:C:Foo"
      }
    }

    "have correct structure for nested namespaces with path" in {
      val cpg = code("""
         |namespace A.B.C {
         |  class Foo {};
         |}
         |""".stripMargin)
      inside(cpg.namespaceBlock("A").l) { case List(namespaceA) =>
        namespaceA.code should startWith("namespace A")
        namespaceA.fullName shouldBe "Test0.ts::program:A"
        namespaceA.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(b) =>
        b.code should startWith("B.C")
        b.fullName shouldBe "Test0.ts::program:A:B"
        b.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(c) =>
        c.code should startWith("C")
        c.fullName shouldBe "Test0.ts::program:A:B:C"
        c.typeDecl.name("Foo").head.fullName shouldBe "Test0.ts::program:A:B:C:Foo"
      }
    }

  }

}
