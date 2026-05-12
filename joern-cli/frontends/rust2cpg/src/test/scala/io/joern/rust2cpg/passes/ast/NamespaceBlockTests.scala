package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class NamespaceBlockTests extends Rust2CpgSuite(noSysRoot = true) {

  "crate namespace block has the rust qualified name as `name` and a unique, file-prefixed `fullName`" in {
    val mainPath = (Paths.get("src") / "main.rs").toString
    val cpg      = code("", mainPath)
    inside(cpg.namespaceBlock.filenameExact(mainPath).l) { case namespaceBlock :: Nil =>
      namespaceBlock.name shouldBe "rust2cpgtest"
      namespaceBlock.fullName shouldBe s"$mainPath:rust2cpgtest"
    }
  }

  "file namespace block holds the fake method together with the rest of the file AST" in {
    val cpg = code("""
        |fn one() {}
        |fn two(x: i32) -> i32 { x }
        |""".stripMargin)

    inside(cpg.method.nameExact(globalNamespaceName).l) { case fakeMethod :: Nil =>
      fakeMethod.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
      fakeMethod.astParentFullName shouldBe s"${(Paths.get("src") / "lib.rs").toString}:rust2cpgtest"
    }

    inside(cpg.method.nameExact(globalNamespaceName).block.astChildren.sortBy(_.order).l) {
      case (one: Method) :: (two: Method) :: Nil =>
        one.fullName shouldBe "rust2cpgtest::one"
        one.name shouldBe "one"
        two.fullName shouldBe "rust2cpgtest::two"
        two.name shouldBe "two"
    }

  }

  "a top-level method's fullName is prefixed by the crate namespace fullName" in {
    val cpg = code("fn do_stuff() {}")
    cpg.method.name("do_stuff").fullName.l shouldBe List("rust2cpgtest::do_stuff")
  }
}
