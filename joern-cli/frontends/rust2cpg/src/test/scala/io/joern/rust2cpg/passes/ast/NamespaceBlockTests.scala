package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class NamespaceBlockTests extends Rust2CpgSuite(noSysRoot = true) {

  "the crate namespace block carries the crate name as both `name` and `fullName`" in {
    val mainPath = (Paths.get("src") / "main.rs").toString
    val cpg      = code("", mainPath)
    inside(cpg.namespaceBlock.filename(".*main\\.rs").l) { case namespaceBlock :: Nil =>
      namespaceBlock.name shouldBe "rust2cpgtest"
      namespaceBlock.fullName shouldBe "rust2cpgtest"
    }
  }

  "an empty source file produces a namespace block with no AST children" in {
    val mainPath = (Paths.get("src") / "main.rs").toString
    val cpg      = code("", mainPath)
    cpg.namespaceBlock.filename(".*main\\.rs").astChildren shouldBe empty
  }

  "top-level methods are children of the crate namespace block" in {
    val cpg = code("""
        |fn one() {}
        |fn two(x: i32) -> i32 { x }
        |""".stripMargin)

    inside(cpg.method.name("one").l) { case one :: Nil =>
      one.fullName shouldBe "rust2cpgtest::one"
      one.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
      one.astParentFullName shouldBe "rust2cpgtest"
    }

    inside(cpg.method.name("two").l) { case two :: Nil =>
      two.fullName shouldBe "rust2cpgtest::two"
      two.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
      two.astParentFullName shouldBe "rust2cpgtest"
    }
  }

  "a top-level method's fullName is prefixed by the crate namespace fullName" in {
    val cpg = code("fn do_stuff() {}")
    cpg.method.name("do_stuff").fullName.l shouldBe List("rust2cpgtest::do_stuff")
  }
}
