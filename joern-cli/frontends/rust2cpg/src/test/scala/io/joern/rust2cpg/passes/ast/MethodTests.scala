package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.NodeTypes
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
        // TODO: update once typeFullNames are recorded.
        //  ident.typeFullName shouldBe "i32"
        }
      }
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

  "a nested fn" should {
    val cpg = code("""
        |fn outer() {
        |    fn inner() {}
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
  }
}
