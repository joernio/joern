package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.*

class TsClassesAstCreationPassTests extends AstJsSrc2CpgSuite(".ts") {

  "AST generation for TS classes" should {

    "have correct structure for constructor parameter assignment" in {
      val cpg = code("""
        |class D {
        |  readonly noWiden = 1
        |  constructor(readonly widen = 2) {
        |    this.noWiden = 5;
        |    this.widen = 6;
        |  }
        |}
        |new D(7);
        |""".stripMargin)
      cpg.typeDecl.nameExact("D").method.isConstructor.parameter.name.l shouldBe List("this", "widen")
    }

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

    "have correct structure for classes with abstract functions" in {
      val cpg = code("""
        |export abstract class Foo {
        |    x: number;
        |    y: number;
        |    public abstract foo(): void;
        |    public abstract bar(): void;
        |}
        |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").modifier.modifierType.l shouldBe List(ModifierTypes.ABSTRACT)
      inside(cpg.typeDecl.nameExact("Foo").member.l) { case List(x, y, foo, bar) =>
        x.name shouldBe "x"
        x.code shouldBe "x: number;"
        x.typeFullName shouldBe Defines.Number
        y.name shouldBe "y"
        y.code shouldBe "y: number;"
        y.typeFullName shouldBe Defines.Number
        foo.name shouldBe "foo"
        foo.code shouldBe "public abstract foo(): void;"
        foo.dynamicTypeHintFullName shouldBe Seq("Test0.ts::program:Foo:foo")
        bar.name shouldBe "bar"
        bar.code shouldBe "public abstract bar(): void;"
        bar.dynamicTypeHintFullName shouldBe Seq("Test0.ts::program:Foo:bar")
      }
      inside(cpg.typeDecl.nameExact("Foo").method.l) { case List(constructor, foo, bar) =>
        constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        constructor.fullName shouldBe s"Test0.ts::program:Foo:${io.joern.x2cpg.Defines.ConstructorMethodName}"
        constructor.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.CONSTRUCTOR)
        foo.name shouldBe "foo"
        foo.fullName shouldBe "Test0.ts::program:Foo:foo"
        foo.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.ABSTRACT, ModifierTypes.PUBLIC)
        bar.name shouldBe "bar"
        bar.fullName shouldBe "Test0.ts::program:Foo:bar"
        bar.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.ABSTRACT, ModifierTypes.PUBLIC)
      }
    }

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
        |  toString(): string;
        |}
        |""".stripMargin)
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "interface Greeter"
        greeter.fullName shouldBe "Test0.ts::program:Greeter"
        greeter.filename shouldBe "Test0.ts"
        greeter.file.name.head shouldBe "Test0.ts"
        inside(cpg.typeDecl("Greeter").member.l) { case List(init, greeting, name, propName, foo, anon, toString) =>
          init.name shouldBe "<init>"
          init.typeFullName shouldBe "Test0.ts::program:Greeter"
          init.dynamicTypeHintFullName shouldBe List("Test0.ts::program:Greeter:<init>")
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
          toString.name shouldBe "toString"
          toString.code shouldBe "toString(): string;"
        }
        inside(cpg.typeDecl("Greeter").method.l) { case List(constructor, anon, toString) =>
          constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          constructor.fullName shouldBe s"Test0.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
          constructor.code shouldBe "new: Greeter"
          greeter.method.isConstructor.head shouldBe constructor
          anon.name shouldBe "<lambda>0"
          anon.fullName shouldBe "Test0.ts::program:Greeter:<lambda>0"
          anon.code shouldBe "(source: string, subString: string): boolean;"
          anon.parameter.name.l shouldBe List("this", "source", "subString")
          anon.parameter.code.l shouldBe List("this", "source: string", "subString: string")
          toString.name shouldBe "toString"
          toString.code shouldBe "toString(): string;"
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
      inside(cpg.namespaceBlock("A").l) { case List(namespaceBlockA) =>
        namespaceBlockA.code should startWith("namespace A")
        namespaceBlockA.fullName shouldBe "Test0.ts::program:A"
        namespaceBlockA.typeDecl.name("Foo").head.fullName shouldBe "Test0.ts::program:A:Foo"
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
      inside(cpg.namespaceBlock("A").l) { case List(namespaceBlockA) =>
        namespaceBlockA.code should startWith("namespace A")
        namespaceBlockA.fullName shouldBe "Test0.ts::program:A"
        namespaceBlockA.astChildren.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(namespaceBlockB) =>
        namespaceBlockB.code should startWith("namespace B")
        namespaceBlockB.fullName shouldBe "Test0.ts::program:A:B"
        namespaceBlockB.astChildren.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(namespaceBlockC) =>
        namespaceBlockC.code should startWith("namespace C")
        namespaceBlockC.fullName shouldBe "Test0.ts::program:A:B:C"
        namespaceBlockC.typeDecl.name("Foo").head.fullName shouldBe "Test0.ts::program:A:B:C:Foo"
      }
    }

    "have correct structure for nested namespaces with path" in {
      val cpg = code("""
         |namespace A.B.C {
         |  class Foo {};
         |}
         |""".stripMargin)
      inside(cpg.namespaceBlock("A").l) { case List(namespaceBlockA) =>
        namespaceBlockA.code should startWith("namespace A")
        namespaceBlockA.fullName shouldBe "Test0.ts::program:A"
        namespaceBlockA.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(namespaceBlockB) =>
        namespaceBlockB.code should startWith("B.C")
        namespaceBlockB.fullName shouldBe "Test0.ts::program:A:B"
        namespaceBlockB.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(namespaceBlockC) =>
        namespaceBlockC.code should startWith("C")
        namespaceBlockC.fullName shouldBe "Test0.ts::program:A:B:C"
        namespaceBlockC.typeDecl.name("Foo").head.fullName shouldBe "Test0.ts::program:A:B:C:Foo"
      }
    }

    "AST generation for dynamically exported and defined class" in {
      val cpg = code("""
        |export type User = {
        |    email: string;
        |    organizationIds: string[];
        |    username: string;
        |    name: string;
        |    gender: string;
        |}
        |""".stripMargin)
      val List(userType) = cpg.typeDecl.name("User").l
      userType.member.name.l shouldBe List("email", "organizationIds", "username", "name", "gender")
      userType.member.typeFullName.toSet shouldBe Set(Defines.String, Defines.Array)
    }

    "AST generation for dynamically defined type in a parameter" in {
      val cpg = code("""
        |class Test {
        |    run(credentials: { username: string; password: string; }): string {
        |        console.log(credentials);
        |        return ``;
        |    }
        |}
        |""".stripMargin)
      val List(credentialsType) = cpg.typeDecl.nameExact("<anon-class>0").l
      credentialsType.fullName shouldBe "Test0.ts::program:Test:run:<anon-class>0"
      credentialsType.member.name.l shouldBe List("username", "password")
      credentialsType.member.typeFullName.toSet shouldBe Set("__ecma.String")
      val List(credentialsParam) = cpg.parameter.nameExact("credentials").l
      credentialsParam.typeFullName shouldBe "Test0.ts::program:Test:run:<anon-class>0"
      // should not produce dangling nodes that are meant to be inside procedures
      cpg.all.collectAll[CfgNode].whereNot(_._astIn).size shouldBe 0
      cpg.identifier.count(_.refsTo.size > 1) shouldBe 0
      cpg.identifier.whereNot(_.refsTo).size shouldBe 0
      // should not produce assignment calls directly under typedecls
      cpg.call.assignment.astParent.isTypeDecl shouldBe empty
    }

    "AST generation for destructured type in a parameter" in {
      val cpg = code("""
        |function apiCall({ username, password }) {
        |    log(`${username}: ${password}`);
        |}
        |""".stripMargin)
      val List(credentialsType) = cpg.typeDecl.nameExact("<anon-class>0").l
      credentialsType.fullName shouldBe "Test0.ts::program:apiCall:<anon-class>0"
      credentialsType.member.name.l shouldBe List("username", "password")
      credentialsType.member.typeFullName.toSet shouldBe Set(Defines.Any)
      val List(credentialsParam) = cpg.parameter.nameExact("param1_0").l
      credentialsParam.typeFullName shouldBe "Test0.ts::program:apiCall:<anon-class>0"
      // should not produce dangling nodes that are meant to be inside procedures
      cpg.all.collectAll[CfgNode].whereNot(_._astIn).size shouldBe 0
      cpg.identifier.count(_.refsTo.size > 1) shouldBe 0
      cpg.identifier.whereNot(_.refsTo).size shouldBe 0
      // should not produce assignment calls directly under typedecls
      cpg.call.assignment.astParent.isTypeDecl shouldBe empty
    }

  }

}
