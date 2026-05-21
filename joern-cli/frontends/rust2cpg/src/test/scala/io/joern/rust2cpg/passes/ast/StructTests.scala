package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.NodeTypes
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
}
