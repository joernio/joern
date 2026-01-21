package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.AstJsSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.passes.base.ContainsEdgePass
import io.shiftleft.codepropertygraph.generated.Operators
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
      cpg.method.nameExact("greet").ast.isCall shouldBe empty
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
      cpg.call.nameExact("__decorate").ast.isCall.code.l shouldBe List(
        "__decorate([a(false),b(foo),c(foo=false),d()], Greeter)",
        "_tmp_0 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        "_tmp_0.push(a(false))",
        "_tmp_0.push",
        "a(false)",
        "_tmp_0.push(b(foo))",
        "_tmp_0.push",
        "b(foo)",
        "_tmp_0.push(c(foo=false))",
        "_tmp_0.push",
        "c(foo=false)",
        "foo=false",
        "_tmp_0.push(d())",
        "_tmp_0.push",
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
      cpg.method.nameExact("x").ast.isCall.code.l shouldBe List("this._x")
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

      val List(constructorRef, annotationLambdaRef) = cpg.methodRef.l
      constructorRef.methodFullName shouldBe "Test0.ts::program:MyClass:<init>"
      constructorRef._containsIn.collectAll[Method].fullName.l shouldBe List("Test0.ts::program")
      annotationLambdaRef.methodFullName shouldBe "Test0.ts::program:<lambda>0"
      annotationLambdaRef._containsIn.collectAll[Method].fullName.l shouldBe List("Test0.ts::program")

      val List(decoratorAssignment) = cpg.call.codeExact("MyClass = __decorate([NgModule(() => { })], MyClass)").l
      val List(myClassRef)          = decoratorAssignment.arguments(1).isIdentifier.l
      myClassRef.name shouldBe "MyClass"
      myClassRef.dynamicTypeHintFullName.l shouldBe List("Test0.ts::program:MyClass:<init>")
      val List(decoratorCall) = decoratorAssignment.arguments(2).isCall.l
      val List(rec)           = decoratorCall.receiver.isIdentifier.l
      rec.name shouldBe "__decorate"
      rec.code shouldBe "__decorate"

      val List(myClassRef2) = decoratorCall.arguments(2).isIdentifier.l
      myClassRef2.name shouldBe "MyClass"
      myClassRef2.dynamicTypeHintFullName.l shouldBe List("Test0.ts::program:MyClass:<init>")

      val List(decoratorExpr) = decoratorCall.arguments(1).ast.isCall.code("NgModule.*").l
      decoratorExpr.code shouldBe "NgModule(() => { })"

      decoratorCall.arguments(1).ast.isCall.code.l shouldBe List(
        "_tmp_0 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        "_tmp_0.push(NgModule(() => { }))",
        "_tmp_0.push",
        "NgModule(() => { })"
      )
    }

    "create annotations correctly for class properties" in {
      val cpg = code("""
          |class Foo {
          |  @format("a, %s")
          |  a: string;
          |
          |  @format("b, %s")
          |  @validate("isString")
          |  b: string;
          |}
          |""".stripMargin)
      val List(reqAAnnotation) = cpg.member.nameExact("a").annotation.l
      reqAAnnotation.code shouldBe "@format(\"a, %s\")"
      reqAAnnotation.name shouldBe "format"
      val List(bFormatAnnotation, bValidateAnnotation) = cpg.member.nameExact("b").annotation.l
      bFormatAnnotation.code shouldBe "@format(\"b, %s\")"
      bFormatAnnotation.name shouldBe "format"
      bValidateAnnotation.code shouldBe "@validate(\"isString\")"
      bValidateAnnotation.name shouldBe "validate"

      val List(decorateACall, decorateBCall) = cpg.call.name("__decorate").l
      decorateACall.code shouldBe """__decorate([format("a, %s")], Foo.prototype, 'a', void 0)"""
      decorateBCall.code shouldBe """__decorate([format("b, %s"),validate("isString")], Foo.prototype, 'b', void 0)"""

      val List(recA) = decorateACall.receiver.isIdentifier.l
      recA.name shouldBe "__decorate"
      recA.code shouldBe "__decorate"

      val List(recB) = decorateBCall.receiver.isIdentifier.l
      recB.name shouldBe "__decorate"
      recB.code shouldBe "__decorate"

      // arg2: Foo.prototype
      val List(aPrototypeExpr) = decorateACall.arguments(2).isCall.l
      aPrototypeExpr.code shouldBe "Foo.prototype"
      aPrototypeExpr.name shouldBe Operators.fieldAccess
      val List(aPrototypeBase) = aPrototypeExpr.arguments(1).isIdentifier.l
      aPrototypeBase.name shouldBe "Foo"
      val List(aPrototypeMember) = aPrototypeExpr.arguments(2).isFieldIdentifier.l
      aPrototypeMember.code.shouldBe("prototype")

      val List(bPrototypeExpr) = decorateBCall.arguments(2).isCall.l
      bPrototypeExpr.code shouldBe "Foo.prototype"
      bPrototypeExpr.name shouldBe Operators.fieldAccess
      val List(bPrototypeBase) = bPrototypeExpr.arguments(1).isIdentifier.l
      bPrototypeBase.name shouldBe "Foo"
      val List(bPrototypeMember) = bPrototypeExpr.arguments(2).isFieldIdentifier.l
      bPrototypeMember.code.shouldBe("prototype")

      // arg3: property name
      decorateACall.arguments(3).isLiteral.code.loneElement shouldBe "'a'"
      decorateBCall.arguments(3).isLiteral.code.loneElement shouldBe "'b'"

      // arg4: decorator return/descriptor placeholder
      decorateACall.arguments(4).isCall.name.loneElement shouldBe "<operator>.void"
      decorateBCall.arguments(4).isCall.name.loneElement shouldBe "<operator>.void"

      // decorator-array lowering under arg1
      decorateACall.arguments(1).ast.isCall.code.l shouldBe List(
        "_tmp_0 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        """_tmp_0.push(format("a, %s"))""",
        "_tmp_0.push",
        """format("a, %s")"""
      )

      decorateBCall.arguments(1).ast.isCall.code.l shouldBe List(
        "_tmp_1 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        """_tmp_1.push(format("b, %s"))""",
        "_tmp_1.push",
        """format("b, %s")""",
        """_tmp_1.push(validate("isString"))""",
        "_tmp_1.push",
        """validate("isString")"""
      )
    }

    "create annotations correctly for class methods" in {
      val cpg = code("""
          |class Foo {
          |  @Get("argA")
          |  reqA(@Req("reqAParam") request: Request): string {
          |    return foo();
          |  }
          |  @Get("argB")
          |  @Put("argC")
          |  reqB(@Req("reqBParam1") request1: Request, @Req("reqBParam2") request2: Request): number {
          |    return bar();
          |  }
          |}
          |""".stripMargin)
      val List(reqAAnnotation) = cpg.method.nameExact("reqA").annotation.l
      reqAAnnotation.code shouldBe "@Get(\"argA\")"
      reqAAnnotation.name shouldBe "Get"
      val List(reqBAnnotationGet, reqBAnnotationPut) = cpg.method.nameExact("reqB").annotation.l
      reqBAnnotationGet.code shouldBe "@Get(\"argB\")"
      reqBAnnotationGet.name shouldBe "Get"
      reqBAnnotationPut.code shouldBe "@Put(\"argC\")"
      reqBAnnotationPut.name shouldBe "Put"

      val List(decorateReqACall, decorateReqBCall) = cpg.call.name("__decorate").l
      decorateReqACall.code shouldBe """__decorate([Get("argA"), __param(0, Req("reqAParam")), __metadata("design:type", Function), __metadata("design:paramtypes", [Object]), __metadata("design:type", String)], Foo.prototype, 'reqA', null)"""
      decorateReqBCall.code shouldBe """__decorate([Get("argB"),Put("argC"), __param(0, Req("reqBParam1")),__param(1, Req("reqBParam2")), __metadata("design:type", Function), __metadata("design:paramtypes", [Object,Object]), __metadata("design:type", Number)], Foo.prototype, 'reqB', null)"""

      val List(recReqA) = decorateReqACall.receiver.isIdentifier.l
      recReqA.name shouldBe "__decorate"
      recReqA.code shouldBe "__decorate"

      val List(recReqB) = decorateReqBCall.receiver.isIdentifier.l
      recReqB.name shouldBe "__decorate"
      recReqB.code shouldBe "__decorate"

      // arg2: Foo.prototype
      val List(reqAPrototypeExpr) = decorateReqACall.arguments(2).isCall.l
      reqAPrototypeExpr.code shouldBe "Foo.prototype"
      reqAPrototypeExpr.name shouldBe Operators.fieldAccess
      val List(reqAPrototypeBase) = reqAPrototypeExpr.arguments(1).isIdentifier.l
      reqAPrototypeBase.name shouldBe "Foo"
      val List(reqAPrototypeMember) = reqAPrototypeExpr.arguments(2).isFieldIdentifier.l
      reqAPrototypeMember.code.shouldBe("prototype")

      val List(reqBPrototypeExpr) = decorateReqBCall.arguments(2).isCall.l
      reqBPrototypeExpr.code shouldBe "Foo.prototype"
      reqBPrototypeExpr.name shouldBe Operators.fieldAccess
      val List(reqBPrototypeBase) = reqBPrototypeExpr.arguments(1).isIdentifier.l
      reqBPrototypeBase.name shouldBe "Foo"
      val List(reqBPrototypeMember) = reqBPrototypeExpr.arguments(2).isFieldIdentifier.l
      reqBPrototypeMember.code.shouldBe("prototype")

      // arg3: method name
      decorateReqACall.arguments(3).isLiteral.code.loneElement shouldBe "'reqA'"
      decorateReqACall.arguments(3).isLiteral.typeFullName.loneElement shouldBe Defines.String
      decorateReqBCall.arguments(3).isLiteral.code.loneElement shouldBe "'reqB'"
      decorateReqBCall.arguments(3).isLiteral.typeFullName.loneElement shouldBe Defines.String

      // arg4: null descriptor
      decorateReqACall.arguments(4).isLiteral.code.loneElement shouldBe "null"
      decorateReqACall.arguments(4).isLiteral.typeFullName.loneElement shouldBe Defines.Null
      decorateReqBCall.arguments(4).isLiteral.code.loneElement shouldBe "null"
      decorateReqBCall.arguments(4).isLiteral.typeFullName.loneElement shouldBe Defines.Null

      // decorator-array lowering under arg1
      decorateReqACall.arguments(1).ast.isCall.code.l shouldBe List(
        "_tmp_1 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        """_tmp_1.push(Get("argA"))""",
        "_tmp_1.push",
        """Get("argA")""",
        """_tmp_1.push(__param(0, Req("reqAParam")))""",
        "_tmp_1.push",
        """__param(0, Req("reqAParam"))""",
        """Req("reqAParam")""",
        """_tmp_1.push(__metadata("design:type", Function))""",
        "_tmp_1.push",
        """__metadata("design:type", Function)""",
        """_tmp_1.push(__metadata("design:paramtypes", [Object]))""",
        "_tmp_1.push",
        """__metadata("design:paramtypes", [Object])""",
        "_tmp_0 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        "_tmp_0.push(Object)",
        "_tmp_0.push",
        """_tmp_1.push(__metadata("design:type", String))""",
        "_tmp_1.push",
        """__metadata("design:type", String)"""
      )

      decorateReqBCall.arguments(1).ast.isCall.code.l shouldBe List(
        "_tmp_3 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        """_tmp_3.push(Get("argB"))""",
        "_tmp_3.push",
        """Get("argB")""",
        """_tmp_3.push(Put("argC"))""",
        "_tmp_3.push",
        """Put("argC")""",
        """_tmp_3.push(__param(0, Req("reqBParam1")))""",
        "_tmp_3.push",
        """__param(0, Req("reqBParam1"))""",
        """Req("reqBParam1")""",
        """_tmp_3.push(__param(1, Req("reqBParam2")))""",
        "_tmp_3.push",
        """__param(1, Req("reqBParam2"))""",
        """Req("reqBParam2")""",
        """_tmp_3.push(__metadata("design:type", Function))""",
        "_tmp_3.push",
        """__metadata("design:type", Function)""",
        """_tmp_3.push(__metadata("design:paramtypes", [Object,Object]))""",
        "_tmp_3.push",
        """__metadata("design:paramtypes", [Object,Object])""",
        "_tmp_2 = __ecma.Array.factory()",
        "__ecma.Array.factory()",
        "_tmp_2.push(Object)",
        "_tmp_2.push",
        "_tmp_2.push(Object)",
        "_tmp_2.push",
        """_tmp_3.push(__metadata("design:type", Number))""",
        "_tmp_3.push",
        """__metadata("design:type", Number)"""
      )
    }
  }

}
