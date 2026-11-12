package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

/** A namespace is lowered to an object that has the exported members of the namespace as properties. */
class TsNamespacesAstCreationPassTests extends JsSrc2CpgSuite(".ts") {

  "AST generation for TS namespaces" should {

    "create the object of a namespace with a path and assign the exported members to it" in {
      val cpg = code("""
          |namespace A.B.C {
          |  export class Foo {}
          |  export function f() {}
          |  export const x = 1, y = 2;
          |}
          |""".stripMargin)
      cpg.assignment.code.l shouldBe List(
        "A = { B: { C: {} } }",
        "_tmp_2.B = { C: {} }",
        "_tmp_1.C = {}",
        "A.B.C.f = f",
        "A.B.C.Foo = class Foo",
        "A.B.C.x = 1",
        "A.B.C.y = 2"
      )
      cpg.assignment.code("A = .*").target.isIdentifier.refsTo.l shouldBe cpg.local.nameExact("A").l
    }

    "keep the full names of the members of a namespace" in {
      val cpg = code("""
          |namespace A.B.C {
          |  export class Foo { run() {} }
          |  export function f() {}
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("Test0.ts::program:A:B:C:Foo")
      cpg.method.name("run", "f", "<init>").fullName.sorted.l shouldBe
        List("Test0.ts::program:A:B:C:Foo:<init>", "Test0.ts::program:A:B:C:Foo:run", "Test0.ts::program:A:B:C:f")
    }

    "not declare the exported members as variables" in {
      val cpg = code("""
          |namespace A {
          |  export class Foo {}
          |  export function f() {}
          |  export const x = 1;
          |}
          |""".stripMargin)
      cpg.local.name.sorted.l shouldBe List("A", "_tmp_0")
      cpg.identifier.nameExact("Foo", "f", "x").l shouldBe empty
    }

    "not export the exported members of a namespace from the file" in {
      val cpg = code("""
          |namespace A {
          |  export class Foo {}
          |}
          |""".stripMargin)
      cpg.identifier.nameExact("exports").l shouldBe empty
    }

    "not assign the members that are not exported to the object of the namespace" in {
      val cpg = code("""
          |namespace A {
          |  const hidden = 1;
          |  function helper() {}
          |  export const visible = 2;
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf (
        "A.visible = 2",
        "_private_0.hidden = 1",
        "_private_0.helper = helper"
      )
      cpg.assignment.target.code.l should not contain "A.hidden"
      cpg.assignment.target.code.l should not contain "A.helper"
    }

    "not declare the members that are not exported as variables" in {
      val cpg = code("""
          |namespace A {
          |  class Hidden {}
          |  function helper() {}
          |  const value = 1;
          |  export const visible = 2;
          |}
          |""".stripMargin)
      cpg.local.name.sorted.l shouldBe List("A", "_private_0", "_tmp_0", "_tmp_1")
      cpg.identifier.nameExact("Hidden", "helper", "value").l shouldBe empty
    }

    "lower the reference to a member that is not exported to the property access of the private object" in {
      val cpg = code("""
          |namespace A {
          |  function helper() { return 1; }
          |  const value = helper();
          |  export function f() { return helper() + value; }
          |}
          |""".stripMargin)
      cpg.method.nameExact("f").call.nameExact("helper").receiver.code.l shouldBe List("_private_0.helper")
      cpg.method.nameExact("f").call.nameExact(Operators.addition).argument.code.l shouldBe
        List("helper()", "_private_0.value")
      cpg.assignment.code("_private_0.value = .*").source.isCall.receiver.code.l shouldBe
        List("_private_0.helper")
    }

    "not make a member that is not exported visible outside of the namespace" in {
      val cpg = code("""
          |namespace A {
          |  class Hidden {}
          |  export const visible = 2;
          |}
          |const h = new Hidden();
          |""".stripMargin)
      cpg.assignment.code.l should contain("_private_0.Hidden = class Hidden")
      cpg.assignment.target.isIdentifier.nameExact("Hidden").l shouldBe empty
      cpg.call.codeExact("new Hidden()").receiver.isIdentifier.name.l shouldBe List("Hidden")
    }

    "lower the reference to an exported member inside of the namespace to the property access" in {
      val cpg = code("""
          |namespace A {
          |  export function f() { return g() + x; }
          |  export function g() { return 1; }
          |  export const x = 2;
          |  export const y = x + 1;
          |}
          |""".stripMargin)
      cpg.method.nameExact("f").call.nameExact("g").code.l shouldBe List("g()")
      cpg.method.nameExact("f").call.nameExact("g").receiver.code.l shouldBe List("A.g")
      cpg.method.nameExact("f").call.nameExact(Operators.addition).argument.code.l shouldBe List("g()", "A.x")
      cpg.assignment.code("A.y = .*").source.isCall.argument.code.l shouldBe List("A.x", "1")
    }

    "not lower the reference to a variable that shadows an exported member" in {
      val cpg = code("""
          |namespace A {
          |  export function f(x) { return x; }
          |  export const x = 1;
          |}
          |""".stripMargin)
      cpg.method.nameExact("f").ast.isReturn.astChildren.code.l shouldBe List("x")
      cpg.method.nameExact("f").ast.isReturn.astChildren.isIdentifier.name.l shouldBe List("x")
    }

    "lower the reference to an exported member from a namespace with a path" in {
      val cpg = code("""
          |namespace A.B {
          |  export class Foo {}
          |  export function create() { return new Foo(); }
          |}
          |""".stripMargin)
      cpg.method.nameExact("create").call.nameExact("<operator>.new").receiver.code.l shouldBe List("A.B.Foo")
    }

    "lower an exported nested namespace to a property of the object of its namespace" in {
      val cpg = code("""
          |namespace A {
          |  export namespace B {
          |    export class Foo {}
          |  }
          |  export function f() { return B.Foo; }
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf ("A = {}", "A.B = {}", "A.B.Foo = class Foo", "A.f = f")
      cpg.local.filter(_.closureBindingId.isEmpty).name.sorted.l shouldBe List("A", "_tmp_0", "_tmp_1")
      cpg.method.nameExact("f").ast.isReturn.astChildren.code.l shouldBe List("A.B.Foo")
    }

    "lower a nested namespace that is not exported to a property of the private object" in {
      val cpg = code("""
          |namespace A {
          |  namespace B {
          |    export const x = 1;
          |  }
          |  export const y = B.x;
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf ("_private_0.B = {}", "_private_0.B.x = 1", "A.y = _private_0.B.x")
      cpg.local.nameExact("B").l shouldBe empty
    }

    "extend an existing class instead of replacing it" in {
      val cpg = code("""
          |class Foo {}
          |namespace Foo {
          |  export const x = 1;
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain("Foo.x = 1")
      cpg.assignment.code("Foo = .*").l.size shouldBe 1
      cpg.local.nameExact("Foo").size shouldBe 1
    }

    "extend the object of an earlier namespace with the same name" in {
      val cpg = code("""
          |namespace A {
          |  export const x = 1;
          |}
          |namespace A {
          |  export const y = 2;
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf ("A = {}", "A.x = 1", "A.y = 2")
      cpg.assignment.code("A = .*").size shouldBe 1
      cpg.local.nameExact("A").size shouldBe 1
    }

    "extend the object of an earlier namespace with the nested namespaces of a path" in {
      val cpg = code("""
          |namespace A {
          |  export const x = 1;
          |}
          |namespace A.B {
          |  export const y = 2;
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf ("A = {}", "A.B = {}", "A.B.y = 2")
      cpg.assignment.code("A = .*").size shouldBe 1
    }

    "assign an enum as type reference to the property" in {
      val cpg = code("""
          |namespace A {
          |  export enum E { X }
          |  enum Hidden { Y }
          |  export const x = E.X;
          |  export const y = Hidden.Y;
          |}
          |""".stripMargin)
      cpg.typeDecl.nameExact("E").fullName.l shouldBe List("Test0.ts::program:A:E")
      cpg.assignment.code.l should contain allOf ("A.E = enum E", "_private_0.Hidden = enum Hidden")
      cpg.assignment.code.l should contain allOf ("A.x = A.E.X", "A.y = _private_0.Hidden.Y")
      cpg.identifier.nameExact("E", "Hidden").l shouldBe empty
    }

    "assign a decorated class to the property" in {
      val cpg = code("""
          |namespace A {
          |  @dec() export class D {}
          |  @dec() class Hidden {}
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf (
        "A.D = Test0.ts::program:A:D:<init>",
        "_private_0.Hidden = Test0.ts::program:A:Hidden:<init>"
      )
      cpg.call.nameExact("__decorate").code.l shouldBe
        List("__decorate([dec()], A.D)", "__decorate([dec()], _private_0.Hidden)")
      cpg.identifier.nameExact("D", "Hidden").l shouldBe empty
    }

    "assign the decorated members of a class to the property" in {
      val cpg = code("""
          |namespace A {
          |  export class D {
          |    @dec() prop = 1;
          |    @dec() method() {}
          |  }
          |}
          |""".stripMargin)
      cpg.call.nameExact("__decorate").argument(2).code.l shouldBe List("A.D.prototype", "A.D.prototype")
    }

    "assign an import assignment to the property" in {
      val cpg = code("""
          |namespace A {
          |  export import fs = require('fs');
          |  import path = require('path');
          |  export const f = fs;
          |  export const p = path;
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf (
        "A.fs = require(\"fs\")",
        "_private_0.path = require(\"path\")",
        "A.f = A.fs",
        "A.p = _private_0.path"
      )
      cpg.local.nameExact("fs", "path").l shouldBe empty
      cpg.imports.importedEntity.l shouldBe List("fs", "path")
      cpg.imports.importedAs.l shouldBe List("A.fs", "_private_0.path")
    }

    "declare an exported variable with a pattern as usual and assign it to the property" in {
      val cpg = code("""
          |namespace A {
          |  export const { p, q } = lib;
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf ("p = _tmp_1.p", "A.p = p", "A.q = q")
    }

    "assign the exported function declaration as method reference" in {
      val cpg = code("""
          |namespace A {
          |  export function f() {}
          |}
          |""".stripMargin)
      val List(assignment) = cpg.assignment.code("A.f = .*").l
      assignment.source.start.isMethodRef.referencedMethod.fullName.l shouldBe List("Test0.ts::program:A:f")
    }

    "assign the exported class as type reference" in {
      val cpg = code("""
          |namespace A {
          |  export class Foo {}
          |}
          |""".stripMargin)
      val List(assignment) = cpg.assignment.code("A.Foo = .*").l
      assignment.source.start.isTypeRef.typeFullName.l shouldBe List("Test0.ts::program:A:Foo:<init>")
      assignment.target.start.isCall.name.l shouldBe List(Operators.fieldAccess)
    }

    "have the location of the declaration for the generated code" in {
      val cpg                    = code("namespace A {\n  export const x = 1;\n}")
      val List(objectAssignment) = cpg.assignment.code("A = .*").l
      objectAssignment.lineNumber shouldBe Some(1)
      objectAssignment.columnNumber shouldBe Some(0)
      val List(memberAssignment) = cpg.assignment.code("A.x = .*").l
      memberAssignment.lineNumber shouldBe Some(2)
    }

    "not create an object for a namespace that does not export values" in {
      val cpg = code("""
          |namespace A {
          |  export interface I {}
          |  export type T = string;
          |  const hidden = 1;
          |}
          |""".stripMargin)
      cpg.assignment.code.l shouldBe List("_private_0 = {}", "_private_0.hidden = 1")
      cpg.local.nameExact("A").l shouldBe empty
    }

    "support overloaded exported functions" in {
      val cpg = code("""
          |namespace A {
          |  export function over(a: string): void;
          |  export function over(a: number): void;
          |  export function over(a: any): void {}
          |  export const x = over(1);
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain("A.x = over(1)")
      cpg.method.fullName("Test0.ts::program:A:over.*").parameter.name.distinct.l shouldBe List("this", "a")
    }

    "not create an object for ambient namespaces" in {
      val cpg = code("""
          |declare namespace A {
          |  export function f(): void;
          |  export namespace B {
          |    export const x: number;
          |  }
          |}
          |""".stripMargin)
      cpg.assignment.code.l shouldBe empty
      cpg.local.nameExact("A").l shouldBe empty
    }

    "not create an object for ambient module declarations" in {
      val cpg = code("""
          |declare module "foo" {
          |  export function f(): void;
          |  namespace N {
          |    export const x: number;
          |  }
          |}
          |declare global {
          |  export function g(): void;
          |}
          |""".stripMargin)
      cpg.assignment.code.l shouldBe empty
    }

    "export an exported namespace with a path from the file by its first name" in {
      val cpg = code("""
          |export namespace A.B {
          |  export class Foo {}
          |}
          |""".stripMargin)
      cpg.assignment.code.l should contain allOf ("A = { B: {} }", "A.B.Foo = class Foo", "exports.A = A")
      cpg.assignment.code.l should not contain "exports.A.B = A.B"
    }
  }

}
