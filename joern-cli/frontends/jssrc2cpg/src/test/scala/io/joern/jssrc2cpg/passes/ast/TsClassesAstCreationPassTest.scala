package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.{AbstractPassTest, Defines}
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Declaration, Identifier}
import io.shiftleft.semanticcpg.language.*

class TsClassesAstCreationPassTest extends AbstractPassTest {

  "AST generation for TS classes" should {

    "have correct structure for constructor parameter assignment" in TsAstFixture("""
        |class D {
        |  readonly noWiden = 1
        |  constructor(readonly widen = 2) {
        |    this.noWiden = 5;
        |    this.widen = 6;
        |  }
        |}
        |new D(7);
        |""".stripMargin) { cpg =>
      cpg.typeDecl.nameExact("D").method.isConstructor.parameter.name.l shouldBe List("this", "widen")
    }

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

    "have correct structure for classes with abstract functions" in TsAstFixture("""
        |export abstract class Foo {
        |    x: number;
        |    y: number;
        |    public abstract foo(): void;
        |    public abstract bar(): void;
        |}
        |""".stripMargin) { cpg =>
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
        foo.dynamicTypeHintFullName shouldBe Seq("code.ts::program:Foo:foo")
        bar.name shouldBe "bar"
        bar.code shouldBe "public abstract bar(): void;"
        bar.dynamicTypeHintFullName shouldBe Seq("code.ts::program:Foo:bar")
      }
      inside(cpg.typeDecl.nameExact("Foo").method.l) { case List(constructor, foo, bar) =>
        constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
        constructor.fullName shouldBe s"code.ts::program:Foo:${io.joern.x2cpg.Defines.ConstructorMethodName}"
        constructor.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.CONSTRUCTOR)
        foo.name shouldBe "foo"
        foo.fullName shouldBe "code.ts::program:Foo:foo"
        foo.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.ABSTRACT, ModifierTypes.PUBLIC)
        bar.name shouldBe "bar"
        bar.fullName shouldBe "code.ts::program:Foo:bar"
        bar.modifier.modifierType.l shouldBe List(ModifierTypes.VIRTUAL, ModifierTypes.ABSTRACT, ModifierTypes.PUBLIC)
      }
    }

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
          greet.dynamicTypeHintFullName shouldBe Seq("code.ts::program:Greeter:greet")
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
        |  toString(): string;
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.typeDecl("Greeter").l) { case List(greeter) =>
        greeter.name shouldBe "Greeter"
        greeter.code shouldBe "interface Greeter"
        greeter.fullName shouldBe "code.ts::program:Greeter"
        greeter.filename shouldBe "code.ts"
        greeter.file.name.head shouldBe "code.ts"
        inside(cpg.typeDecl("Greeter").member.l) { case List(greeting, name, propName, foo, anon, toString) =>
          greeting.name shouldBe "greeting"
          greeting.code shouldBe "greeting: string;"
          name.name shouldBe "name"
          name.code shouldBe "name?: string;"
          propName.name shouldBe "propName"
          propName.code shouldBe "[propName: string]: any;"
          foo.name shouldBe "foo"
          foo.code shouldBe "\"foo\": string;"
          anon.name shouldBe "<lambda>0"
          anon.dynamicTypeHintFullName shouldBe Seq("code.ts::program:Greeter:<lambda>0")
          anon.code shouldBe "(source: string, subString: string): boolean;"
          toString.name shouldBe "toString"
          toString.code shouldBe "toString(): string;"
        }
        inside(cpg.typeDecl("Greeter").method.l) { case List(constructor, anon, toString) =>
          constructor.name shouldBe io.joern.x2cpg.Defines.ConstructorMethodName
          constructor.fullName shouldBe s"code.ts::program:Greeter:${io.joern.x2cpg.Defines.ConstructorMethodName}"
          constructor.code shouldBe "new: Greeter"
          greeter.method.isConstructor.head shouldBe constructor
          anon.name shouldBe "<lambda>0"
          anon.fullName shouldBe "code.ts::program:Greeter:<lambda>0"
          anon.code shouldBe "(source: string, subString: string): boolean;"
          anon.parameter.name.l shouldBe List("this", "source", "subString")
          anon.parameter.code.l shouldBe List("this", "source: string", "subString: string")
          toString.name shouldBe "toString"
          toString.code shouldBe "toString(): string;"
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
      inside(cpg.namespaceBlock("A").l) { case List(namespaceBlockA) =>
        namespaceBlockA.code should startWith("namespace A")
        namespaceBlockA.fullName shouldBe "code.ts::program:A"
        namespaceBlockA.typeDecl.name("Foo").head.fullName shouldBe "code.ts::program:A:Foo"
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
      inside(cpg.namespaceBlock("A").l) { case List(namespaceBlockA) =>
        namespaceBlockA.code should startWith("namespace A")
        namespaceBlockA.fullName shouldBe "code.ts::program:A"
        namespaceBlockA.astChildren.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(namespaceBlockB) =>
        namespaceBlockB.code should startWith("namespace B")
        namespaceBlockB.fullName shouldBe "code.ts::program:A:B"
        namespaceBlockB.astChildren.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(namespaceBlockC) =>
        namespaceBlockC.code should startWith("namespace C")
        namespaceBlockC.fullName shouldBe "code.ts::program:A:B:C"
        namespaceBlockC.typeDecl.name("Foo").head.fullName shouldBe "code.ts::program:A:B:C:Foo"
      }
    }

    "have correct structure for nested namespaces with path" in TsAstFixture("""
         |namespace A.B.C {
         |  class Foo {};
         |}
         |""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock("A").l) { case List(namespaceBlockA) =>
        namespaceBlockA.code should startWith("namespace A")
        namespaceBlockA.fullName shouldBe "code.ts::program:A"
        namespaceBlockA.astChildren.isNamespaceBlock.name("B").head shouldBe cpg.namespaceBlock("B").head
      }
      inside(cpg.namespaceBlock("B").l) { case List(namespaceBlockB) =>
        namespaceBlockB.code should startWith("B.C")
        namespaceBlockB.fullName shouldBe "code.ts::program:A:B"
        namespaceBlockB.astChildren.isNamespaceBlock.name("C").head shouldBe cpg.namespaceBlock("C").head
      }
      inside(cpg.namespaceBlock("C").l) { case List(namespaceBlockC) =>
        namespaceBlockC.code should startWith("C")
        namespaceBlockC.fullName shouldBe "code.ts::program:A:B:C"
        namespaceBlockC.typeDecl.name("Foo").head.fullName shouldBe "code.ts::program:A:B:C:Foo"
      }
    }

    "AST generation for dynamically exported and defined class" in TsAstFixture("""
        |export type User = {
        |    email: string;
        |    organizationIds: string[];
        |    username: string;
        |    name: string;
        |    gender: string;
        |}
        |""".stripMargin) { cpg =>
      val List(userType) = cpg.typeDecl.name("User").l
      userType.member.name.l shouldBe List("email", "organizationIds", "username", "name", "gender")
      userType.member.typeFullName.toSet shouldBe Set("__ecma.String", "string[]")
    }

    "AST generation for dynamically defined type in a parameter" in TsAstFixture("""
        |class Test {
        |    run(credentials: { username: string; password: string; }): string {
        |        console.log(credentials);
        |        return ``;
        |    }
        |}
        |""".stripMargin) { cpg =>
      val List(credentialsType) = cpg.typeDecl.nameExact("_anon_cdecl").l
      credentialsType.fullName shouldBe "code.ts::program:Test:run:_anon_cdecl"
      credentialsType.member.name.l shouldBe List("username", "password")
      credentialsType.member.typeFullName.toSet shouldBe Set("__ecma.String")
      val List(credentialsParam) = cpg.parameter.nameExact("credentials").l
      credentialsParam.typeFullName shouldBe "code.ts::program:Test:run:_anon_cdecl"
      // should not produce dangling nodes that are meant to be inside procedures
      cpg.all.collectAll[CfgNode].whereNot(_._astIn).size shouldBe 0
      cpg.identifier.count(_.refsTo.size > 1) shouldBe 0
      cpg.identifier.whereNot(_.refsTo).size shouldBe 0
    }

    "AST generation for destructured type in a parameter" in TsAstFixture("""
        |function apiCall({ username, password }) {
        |    log(`${username}: ${password}`);
        |}
        |""".stripMargin) { cpg =>
      val List(credentialsType) = cpg.typeDecl.nameExact("_anon_cdecl").l
      credentialsType.fullName shouldBe "code.ts::program:apiCall:_anon_cdecl"
      credentialsType.member.name.l shouldBe List("username", "password")
      credentialsType.member.typeFullName.toSet shouldBe Set(Defines.Any)
      val List(credentialsParam) = cpg.parameter.nameExact("param1_0").l
      credentialsParam.typeFullName shouldBe "code.ts::program:apiCall:_anon_cdecl"
      // should not produce dangling nodes that are meant to be inside procedures
      cpg.all.collectAll[CfgNode].whereNot(_._astIn).size shouldBe 0
      cpg.identifier.count(_.refsTo.size > 1) shouldBe 0
      cpg.identifier.whereNot(_.refsTo).size shouldBe 0
    }

  }

}
