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

    "have VIRTUAL and MODULE modifiers" in {
      cpg.method.nameExact(globalNamespaceName).modifier.modifierType.toSet shouldBe
        Set(ModifierTypes.VIRTUAL, ModifierTypes.MODULE)
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
}
