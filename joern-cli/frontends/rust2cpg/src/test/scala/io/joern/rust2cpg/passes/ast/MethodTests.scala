package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class MethodTests extends Rust2CpgSuite(noSysRoot = true) {

  "a top-level fn" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg     = code("fn main() {}")

    "have a crate-prefixed fullName" in {
      cpg.method.name("main").fullName.l shouldBe List("rust2cpgtest::main")
    }

    "be parented by the fake global method" in {
      inside(cpg.method.name("main").l) { case main :: Nil =>
        main.astParentType shouldBe NodeTypes.METHOD
        main.astParentFullName shouldBe s"$libPath:rust2cpgtest::$globalNamespaceName"
      }
    }

    "have no modifiers" in {
      cpg.method.name("main").modifier shouldBe empty
    }
  }

  "a fn with a single parameter and a tail expression" should {
    val cpg = code("""
        |fn id(x: i32) -> i32 {
        | x
        |}
        |""".stripMargin)

    "have the parameter at index 1 with its declared type" in {
      inside(cpg.method.name("id").parameter.sortBy(_.order).l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "x"
        param.index shouldBe 1
        param.typeFullName shouldBe "i32"
      }
    }

    "lower the tail expression into a RETURN" in {
      inside(cpg.method.name("id").block.astChildren.l) { case (ret: Return) :: Nil =>
        ret.code shouldBe "x"

        inside(ret.astChildren.l) { case (ident: Identifier) :: Nil =>
          ident.name shouldBe "x"
          ident.code shouldBe "x"
          ident.typeFullName shouldBe "i32"
        }
      }
    }

    "have correct REF edges for the parameter" in {
      cpg.method.name("id").parameter.referencingIdentifiers.lineNumber.l shouldBe List(3)
    }
  }

  "parameter shadowed by let" should {
    val cpg = code("""
        |fn f(x: i32) {
        | let y = x;
        | let x = 2;
        | let z = x;
        |}
        |""".stripMargin)

    "have correct REF edges for the parameter" in {
      cpg.method.name("f").parameter.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3)
    }

    "have correct REF edges for the local" in {
      cpg.method.name("f").local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(4, 5)
    }
  }

  "a fn with multiple parameters" should {
    val cpg = code("fn foo(p1: i32, p2: i64, p3: f32) {}")

    "preserve their order and declared types" in {
      inside(cpg.method.name("foo").parameter.sortBy(_.order).l) { case p1 :: p2 :: p3 :: Nil =>
        p1.name shouldBe "p1"
        p1.index shouldBe 1
        p1.typeFullName shouldBe "i32"

        p2.name shouldBe "p2"
        p2.index shouldBe 2
        p2.typeFullName shouldBe "i64"

        p3.name shouldBe "p3"
        p3.index shouldBe 3
        p3.typeFullName shouldBe "f32"
      }
    }
  }

  "tuple pattern parameter" should {
    val cpg = code("""
        |fn f((a, b): (i32, bool)) {
        | let c = a;
        |}
        |""".stripMargin)

    "have correct parameter" in {
      inside(cpg.method.nameExact("f").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "<tmp>0"
        param.code shouldBe "(a, b): (i32, bool)"
        param.index shouldBe 1
        param.typeFullName shouldBe "(i32, bool)"
      }
    }

    "have correct locals" in {
      inside(cpg.method.nameExact("f").block.local.l) { case aLocal :: bLocal :: cLocal :: Nil =>
        aLocal.name shouldBe "a"
        aLocal.typeFullName shouldBe "i32"
        bLocal.name shouldBe "b"
        bLocal.typeFullName shouldBe "bool"
        cLocal.name shouldBe "c"
        cLocal.typeFullName shouldBe "i32"
      }
    }

    "have correct assignments" in {
      inside(cpg.method.nameExact("f").block.astChildren.isCall.l) { case aAssign :: bAssign :: cAssign :: Nil =>
        aAssign.code shouldBe "a = <tmp>0.0"
        bAssign.code shouldBe "b = <tmp>0.1"
        cAssign.code shouldBe "let c = a;"
      }
    }
  }

  "record pattern parameter" should {
    val cpg = code("""
        |struct Point { x: i32, y: bool }
        |fn f(Point { x, y }: Point) {}
        |""".stripMargin)

    "have correct parameter" in {
      inside(cpg.method.nameExact("f").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "<tmp>0"
        param.code shouldBe "Point { x, y }: Point"
        param.index shouldBe 1
        param.typeFullName shouldBe "rust2cpgtest::Point"
      }
    }

    "have correct locals" in {
      inside(cpg.method.nameExact("f").block.local.l) { case xLocal :: yLocal :: Nil =>
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"

        yLocal.name shouldBe "y"
        yLocal.typeFullName shouldBe "bool"
      }
    }

    "have correct assignments" in {
      inside(cpg.method.nameExact("f").block.astChildren.isCall.l) { case xAssign :: yAssign :: Nil =>
        xAssign.code shouldBe "x = <tmp>0.x"
        yAssign.code shouldBe "y = <tmp>0.y"
      }
    }
  }

  "@ pattern parameter" should {
    val cpg = code("""
        |fn f(p @ (a, b): (i32, bool)) {}
        |""".stripMargin)

    "have correct parameter" in {
      inside(cpg.method.name("f").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "p"
        param.code shouldBe "p @ (a, b): (i32, bool)"
        param.typeFullName shouldBe "(i32, bool)"
      }
    }

    "have correct assignments" in {
      inside(cpg.method.name("f").block.astChildren.isCall.l) { case aAssign :: bAssign :: Nil =>
        aAssign.code shouldBe "a = p.0"
        bAssign.code shouldBe "b = p.1"
      }
    }
  }

  "reference pattern parameter" should {
    val cpg = code("""
        |fn f(&x: &i32) {}
        |""".stripMargin)

    "have correct parameter" in {
      inside(cpg.method.nameExact("f").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "<tmp>0"
        param.code shouldBe "&x: &i32"
        param.index shouldBe 1
        param.typeFullName shouldBe "&i32"
      }
    }

    "have correct locals" in {
      inside(cpg.method.nameExact("f").block.local.l) { case xLocal :: Nil =>
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
      }
    }

    "have correct assignments" in {
      inside(cpg.method.nameExact("f").block.astChildren.isCall.l) { case xAssign :: Nil =>
        xAssign.code shouldBe "x = *<tmp>0"
        inside(xAssign.argument.sortBy(_.argumentIndex).l) { case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "i32"
          rhs.methodFullName shouldBe Operators.indirection
          rhs.code shouldBe "*<tmp>0"
          rhs.typeFullName shouldBe "i32"
        }
      }
    }
  }

  "ref parameter" should {
    val cpg = code("""
        |fn f(ref x: i32) {}
        |""".stripMargin)

    "have correct parameter" in {
      inside(cpg.method.nameExact("f").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "<tmp>0"
        param.code shouldBe "ref x: i32"
        param.index shouldBe 1
        param.typeFullName shouldBe "i32"
      }
    }

    "have correct locals" in {
      inside(cpg.method.nameExact("f").block.local.l) { case xLocal :: Nil =>
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "&i32"
      }
    }

    "have correct assignments" in {
      inside(cpg.method.nameExact("f").block.astChildren.isCall.l) { case xAssign :: Nil =>
        xAssign.code shouldBe "x = &<tmp>0"
        inside(xAssign.argument.sortBy(_.argumentIndex).l) { case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "&i32"
          rhs.methodFullName shouldBe Operators.addressOf
          rhs.code shouldBe "&<tmp>0"
          rhs.typeFullName shouldBe "&i32"
        }
      }
    }
  }

  "ref mut parameter" should {
    val cpg = code("""
        |fn f(ref mut x: i32) {}
        |""".stripMargin)

    "have correct parameter" in {
      inside(cpg.method.nameExact("f").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "<tmp>0"
        param.code shouldBe "ref mut x: i32"
        param.index shouldBe 1
        param.typeFullName shouldBe "i32"
      }
    }

    "have correct locals" in {
      inside(cpg.method.nameExact("f").block.local.l) { case xLocal :: Nil =>
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "&mut i32"
      }
    }

    "have correct assignments" in {
      inside(cpg.method.nameExact("f").block.astChildren.isCall.l) { case xAssign :: Nil =>
        xAssign.code shouldBe "x = &<tmp>0"
        inside(xAssign.argument.sortBy(_.argumentIndex).l) { case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "&mut i32"
          rhs.methodFullName shouldBe Operators.addressOf
          rhs.code shouldBe "&<tmp>0"
          rhs.typeFullName shouldBe "&mut i32"
        }
      }
    }
  }

  "wildcard parameter" should {
    val cpg = code("fn f(_: i32) {}")

    "have correct parameter" in {
      inside(cpg.method.nameExact("f").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "<tmp>0"
        param.code shouldBe "_: i32"
        param.index shouldBe 1
        param.typeFullName shouldBe "i32"
      }
    }

    "have no assignments" in {
      cpg.method.nameExact("f").block.astChildren shouldBe empty
    }
  }

  "generic fn" should {
    val cpg = code("""
        |fn id<T>(x: T) -> T {
        | x
        |}
        |fn main() {
        | id(1);
        |}
        |""".stripMargin)

    "have generic parameters in the fullName" in {
      cpg.method.name("id").fullName.l shouldBe List("rust2cpgtest::id<T>")
    }

    "have the same fullName as the one at the call site" in {
      cpg.call.name("id").methodFullName.l shouldBe List("rust2cpgtest::id<T>")
    }
  }

  "a nested fn" should {
    val cpg = code("""
        |fn outer() {
        |    fn inner() {}
        |    inner();
        |}
        |""".stripMargin)

    "have its enclosing method's fullName as prefix" in {
      cpg.method.name("inner").fullName.l shouldBe List("rust2cpgtest::outer::inner")
    }

    "be parented by its enclosing method" in {
      inside(cpg.method.name("inner").l) { case (inner: Method) :: Nil =>
        inner.astParentType shouldBe NodeTypes.METHOD
        inner.astParentFullName shouldBe "rust2cpgtest::outer"
      }
    }

    "have correct methodFullName" in {
      inside(cpg.call.nameExact("inner").l) { case call :: Nil =>
        call.methodFullName shouldBe "rust2cpgtest::outer::inner"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        call.argument shouldBe empty
      }
    }
  }

  "same-named nested fns in both branches of an if" should {
    val cpg = code("""
        |fn outer(c: bool) {
        |  if c {
        |    fn inner() -> i32 { 1 }
        |    inner();
        |  } else {
        |    fn inner() -> i64 { 2 }
        |    inner();
        |  }
        |}
        |""".stripMargin)

    "have correct fullNames" in {
      inside(cpg.method.nameExact("inner").sortBy(_.lineNumber).l) { case thenInner :: elseInner :: Nil =>
        thenInner.fullName shouldBe "rust2cpgtest::outer::inner#1"
        thenInner.methodReturn.typeFullName shouldBe "i32"

        elseInner.fullName shouldBe "rust2cpgtest::outer::inner#2"
        elseInner.methodReturn.typeFullName shouldBe "i64"
      }
    }

    "have correct methodFullName" in {
      inside(cpg.call.nameExact("inner").sortBy(_.lineNumber).l) { case thenCall :: elseCall :: Nil =>
        thenCall.methodFullName shouldBe "rust2cpgtest::outer::inner#1"
        elseCall.methodFullName shouldBe "rust2cpgtest::outer::inner#2"
      }
    }
  }
}
