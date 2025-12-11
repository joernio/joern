package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.passes.base.ContainsEdgePass
import io.shiftleft.codepropertygraph.generated.nodes.{Method, TypeDecl, _containsIn}
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
      cpg.method.nameExact("greet").ast.isCall.code.l shouldBe List(
        "a(false)",
        "b(foo)",
        "c(foo=false)",
        "foo=false",
        "d()"
      )
      inside(cpg.typeDecl.name("Greeter").method.name("greet").annotation.l) {
        case List(annotationA, annotationB, annotationC, annotationD) =>
          annotationA.code shouldBe "@a(false)"
          annotationA.name shouldBe "a"
          annotationA.fullName shouldBe "a"

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"

          annotationD.code shouldBe "@d()"
          annotationD.name shouldBe "d"
          annotationD.fullName shouldBe "d"
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
      cpg.method.nameExact("<init>").ast.isCall.code.l shouldBe List(
        "a(false)",
        "b(foo)",
        "c(foo=false)",
        "foo=false",
        "d()"
      )
      inside(cpg.typeDecl.name("Greeter").annotation.l) {
        case List(annotationA, annotationB, annotationC, annotationD) =>
          annotationA.code shouldBe "@a(false)"
          annotationA.name shouldBe "a"
          annotationA.fullName shouldBe "a"

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"

          annotationD.code shouldBe "@d()"
          annotationD.name shouldBe "d"
          annotationD.fullName shouldBe "d"
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

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"

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
      cpg.method.nameExact("x").ast.isCall.code.l shouldBe List(
        "a(false)",
        "b(foo)",
        "c(foo=false)",
        "foo=false",
        "d()",
        "this._x"
      )
      inside(cpg.typeDecl.name("Foo").method.name("x").annotation.l) {
        case List(annotationA, annotationB, annotationC, annotationD) =>
          annotationA.code shouldBe "@a(false)"
          annotationA.name shouldBe "a"
          annotationA.fullName shouldBe "a"

          annotationB.code shouldBe "@b(foo)"
          annotationB.name shouldBe "b"
          annotationB.fullName shouldBe "b"

          annotationC.code shouldBe "@c(foo=false)"
          annotationC.name shouldBe "c"
          annotationC.fullName shouldBe "c"

          annotationD.code shouldBe "@d()"
          annotationD.name shouldBe "d"
          annotationD.fullName shouldBe "d"
          annotationD.parameterAssign.l shouldBe empty
      }
    }

    "create annotations correctly with lambda function as annotation argument" in {
      val cpg = code("""
          |import { NgModule } from '@angular/core';
          |
          |@NgModule(() => { })
          |export class MyClass { }
          |""".stripMargin)
      ContainsEdgePass(cpg).createAndApply()
      cpg.methodRef.where(_._containsIn.collectAll[TypeDecl]) shouldBe empty
      cpg.methodRef._containsIn.collectAll[Method].fullName.l shouldBe List(
        "Test0.ts::program:MyClass:<init>",
        "Test0.ts::program"
      )
      cpg.method.nameExact("<init>").ast.isCall.code.l shouldBe List("NgModule(() => { })")
    }
  }

}
