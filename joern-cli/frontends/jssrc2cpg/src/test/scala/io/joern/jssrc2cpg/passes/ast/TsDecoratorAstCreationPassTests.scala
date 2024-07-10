package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
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
  }

}
