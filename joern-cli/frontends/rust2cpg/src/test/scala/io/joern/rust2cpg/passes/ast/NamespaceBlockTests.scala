package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class NamespaceBlockTests extends Rust2CpgSuite(noSysRoot = true) {

  "the crate namespace block" should {
    val mainPath = (Paths.get("src") / "main.rs").toString
    val cpg      = code("", mainPath)

    "use the crate name as name" in {
      cpg.namespaceBlock.filenameExact(mainPath).name.l shouldBe List("rust2cpgtest")
    }

    "have a file-prefixed fullName" in {
      cpg.namespaceBlock.filenameExact(mainPath).fullName.l shouldBe List(s"$mainPath:rust2cpgtest")
    }
  }

  "a submodule's namespace block" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val fooPath = (Paths.get("src") / "foo.rs").toString
    val cpg     = code("mod foo;", libPath).moreCode("fn bar() {}", fooPath)

    "prefix name with the crate name" in {
      cpg.namespaceBlock.filenameExact(fooPath).name.l shouldBe List("rust2cpgtest::foo")
    }

    "prefix fullName with the file and crate name" in {
      cpg.namespaceBlock.filenameExact(fooPath).fullName.l shouldBe List(s"$fooPath:rust2cpgtest::foo")
    }
  }

  "the fake global method" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg = code("""
        |fn one() {}
        |fn two(x: i32) -> i32 { x }
        |""".stripMargin)

    "have a file-prefixed fullName" in {
      cpg.method.nameExact(globalNamespaceName).fullName.l shouldBe
        List(s"$libPath:rust2cpgtest::$globalNamespaceName")
    }

    "be parented by the crate namespace block" in {
      inside(cpg.method.nameExact(globalNamespaceName).l) { case fakeMethod :: Nil =>
        fakeMethod.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
        fakeMethod.astParentFullName shouldBe s"$libPath:rust2cpgtest"
      }
    }

    "have a () return type" in {
      cpg.method.nameExact(globalNamespaceName).methodReturn.typeFullName.l shouldBe List("()")
    }

    "have MODULE modifier" in {
      cpg.method.nameExact(globalNamespaceName).modifier.modifierType.l shouldBe List(ModifierTypes.MODULE)
    }

    "own top-level fns as AST children" in {
      inside(cpg.method.nameExact(globalNamespaceName).block.astChildren.sortBy(_.order).l) {
        case (one: Method) :: (two: Method) :: Nil =>
          one.name shouldBe "one"
          one.fullName shouldBe "rust2cpgtest::one"
          two.name shouldBe "two"
          two.fullName shouldBe "rust2cpgtest::two"
      }
    }
  }

  "an inline module" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg = code("""
        |mod foo {
        | fn bar() {}
        |}
        |""".stripMargin)

    "create a namespace block whose name is crate-prefixed" in {
      cpg.namespaceBlock.fullName.l should contain(s"$libPath:rust2cpgtest::foo")
    }

    "name the namespace block with the crate name" in {
      cpg.namespaceBlock.fullNameExact(s"$libPath:rust2cpgtest::foo").name.l shouldBe List("rust2cpgtest::foo")
    }

    "own the inner fn as an AST child" in {
      inside(cpg.namespaceBlock.fullNameExact(s"$libPath:rust2cpgtest::foo").astChildren.isMethod.l) {
        case bar :: Nil =>
          bar.name shouldBe "bar"
          bar.fullName shouldBe "rust2cpgtest::foo::bar"
      }
    }

    "parent the inner fn by the module's namespace block" in {
      inside(cpg.method.nameExact("bar").l) { case bar :: Nil =>
        bar.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
        bar.astParentFullName shouldBe s"$libPath:rust2cpgtest::foo"
      }
    }
  }

  "a nested inline module" should {
    val libPath = (Paths.get("src") / "lib.rs").toString
    val cpg = code("""
        |mod a {
        | mod b {
        |   fn c() {}
        | }
        |}
        |""".stripMargin)

    "compose the outer module's name with the crate" in {
      cpg.namespaceBlock.fullName.l should contain(s"$libPath:rust2cpgtest::a")
    }

    "compose the inner module's name with its parent module" in {
      cpg.namespaceBlock.fullName.l should contain(s"$libPath:rust2cpgtest::a::b")
    }

    "compose the fn's fullName with both modules" in {
      cpg.method.nameExact("c").fullName.l shouldBe List("rust2cpgtest::a::b::c")
    }

    "parent the fn by the inner module's namespace block" in {
      inside(cpg.method.nameExact("c").l) { case cFn :: Nil =>
        cFn.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
        cFn.astParentFullName shouldBe s"$libPath:rust2cpgtest::a::b"
      }
    }
  }
}
