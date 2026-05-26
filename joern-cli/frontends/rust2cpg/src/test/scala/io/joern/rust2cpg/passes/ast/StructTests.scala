package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class StructTests extends Rust2CpgSuite(noSysRoot = true) {

  "a top-level named-field struct" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg     = code("struct Foo { x: i32, y: i32 }")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("rust2cpgtest::Foo")
    }

    "be parented by the fake global method" in {
      inside(cpg.typeDecl.nameExact("Foo").l) { case foo :: Nil =>
        foo.astParentType shouldBe NodeTypes.METHOD
        foo.astParentFullName shouldBe s"$libPath:rust2cpgtest::$globalNamespaceName"
      }
    }

    "have one MEMBER per field" in {
      inside(cpg.typeDecl.nameExact("Foo").member.l) { case x :: y :: Nil =>
        x.name shouldBe "x"
        x.code shouldBe "x: i32"
        x.typeFullName shouldBe "i32"
        y.name shouldBe "y"
        y.code shouldBe "y: i32"
        y.typeFullName shouldBe "i32"
      }
    }
  }

  "a top-level tuple struct" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg     = code("struct Pair(i32, bool);")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Pair").fullName.l shouldBe List("rust2cpgtest::Pair")
    }

    "be parented by the fake global method" in {
      inside(cpg.typeDecl.nameExact("Pair").l) { case pair :: Nil =>
        pair.astParentType shouldBe NodeTypes.METHOD
        pair.astParentFullName shouldBe s"$libPath:rust2cpgtest::$globalNamespaceName"
      }
    }

    "have one MEMBER per field, named by positional index" in {
      inside(cpg.typeDecl.nameExact("Pair").member.l) { case zero :: one :: Nil =>
        zero.name shouldBe "0"
        zero.code shouldBe "i32"
        zero.typeFullName shouldBe "i32"
        one.name shouldBe "1"
        one.code shouldBe "bool"
        one.typeFullName shouldBe "bool"
      }
    }
  }

  "a top-level tuple struct with one field" should {
    val cpg = code("struct Wrapper(i32);")

    "have a single MEMBER named `0`" in {
      inside(cpg.typeDecl.nameExact("Wrapper").member.l) { case zero :: Nil =>
        zero.name shouldBe "0"
        zero.code shouldBe "i32"
        zero.typeFullName shouldBe "i32"
      }
    }
  }

  "a top-level tuple struct with no fields" should {
    val cpg = code("struct Empty();")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Empty").fullName.l shouldBe List("rust2cpgtest::Empty")
    }

    "have no members" in {
      cpg.typeDecl.nameExact("Empty").member shouldBe empty
    }
  }

  "a top-level unit struct" should {
    val cpg = code("struct Bar;")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Bar").fullName.l shouldBe List("rust2cpgtest::Bar")
    }

    "have no members" in {
      cpg.typeDecl.nameExact("Bar").member shouldBe empty
    }
  }

  "a struct inside an inline module" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg = code("""
        |mod m {
        |  struct Foo { x: i32 }
        |}
        |""".stripMargin)

    "compose its fullName with the module" in {
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("rust2cpgtest::m::Foo")
    }

    "be parented by the module's namespace block" in {
      inside(cpg.typeDecl.nameExact("Foo").l) { case foo :: Nil =>
        foo.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
        foo.astParentFullName shouldBe s"$libPath:rust2cpgtest::m"
      }
    }
  }

  "a struct inside a nested inline module" should {
    val cpg = code("""
        |mod a {
        |  mod b {
        |    struct C;
        |  }
        |}
        |""".stripMargin)

    "compose its fullName with both modules" in {
      cpg.typeDecl.nameExact("C").fullName.l shouldBe List("rust2cpgtest::a::b::C")
    }
  }

  "a struct inside a function body" should {
    val cpg = code("""
        |fn outer() {
        |  struct Inner { value: i32 }
        |}
        |""".stripMargin)

    "have its enclosing method's fullName as prefix" in {
      cpg.typeDecl.nameExact("Inner").fullName.l shouldBe List("rust2cpgtest::outer::Inner")
    }

    "be parented by its enclosing method" in {
      inside(cpg.typeDecl.nameExact("Inner").l) { case inner :: Nil =>
        inner.astParentType shouldBe NodeTypes.METHOD
        inner.astParentFullName shouldBe "rust2cpgtest::outer"
      }
    }
  }

  "a named-field struct field access" should {
    val cpg = code("""
        |struct Point {
        | x: i32,
        |}
        |
        |fn foo(point: Point) -> i32 {
        | point.x
        |}
        |""".stripMargin)

    "lower to a fieldAccess call" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
        fieldAccess.code shouldBe "point.x"
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fieldAccess.typeFullName shouldBe "i32"
      }
    }

    "have the lhs as the first argument" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).argument(1).l) { case (base: Identifier) :: Nil =>
        base.code shouldBe "point"
        base.argumentIndex shouldBe 1
        base.typeFullName shouldBe "rust2cpgtest::Point"
      }
    }

    "have the field as the second argument" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).argument(2).l) { case (field: FieldIdentifier) :: Nil =>
        field.code shouldBe "x"
        field.canonicalName shouldBe "x"
        field.argumentIndex shouldBe 2
      }
    }
  }

  "a tuple-struct positional field access" should {
    val cpg = code("""
        |struct Pair(i32, bool);
        |
        |fn foo(pair: Pair) -> i32 {
        | pair.0
        |}
        |""".stripMargin)

    "lower to a fieldAccess call" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).l) { case fieldAccess :: Nil =>
        fieldAccess.code shouldBe "pair.0"
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        fieldAccess.typeFullName shouldBe "i32"
      }
    }

    "have the lhs as the first argument" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).argument(1).l) { case (base: Identifier) :: Nil =>
        base.code shouldBe "pair"
        base.argumentIndex shouldBe 1
        base.typeFullName shouldBe "rust2cpgtest::Pair"
      }
    }

    "have the positional index as the second argument" in {
      inside(cpg.call.nameExact(Operators.fieldAccess).argument(2).l) { case (field: FieldIdentifier) :: Nil =>
        field.code shouldBe "0"
        field.canonicalName shouldBe "0"
        field.argumentIndex shouldBe 2
      }
    }
  }
}
