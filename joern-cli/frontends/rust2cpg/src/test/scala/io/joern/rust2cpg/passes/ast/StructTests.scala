package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class StructTests extends Rust2CpgSuite(noSysRoot = true) {

  "record struct emits a TypeDecl with one member per named field" in {
    val cpg = code("""
        |struct Point {
        |    x: i32,
        |    y: i32,
        |}
        |""".stripMargin)

    inside(cpg.typeDecl.name("Point").l) { case point :: Nil =>
      point.fullName shouldBe "rust2cpgtest::Point"
      point.astParentType shouldBe NodeTypes.METHOD
      point.astParentFullName shouldBe s"${(Paths.get("src") / "lib.rs").toString}:rust2cpgtest:$globalNamespaceName"
      point.member.map(member => (member.name, member.typeFullName)).sorted.l shouldBe List(("x", "i32"), ("y", "i32"))
    }
  }

  "tuple struct emits members named `0`, `1`, ... in declaration order" in {
    val cpg = code("""
        |struct Pair(i32, bool);
        |""".stripMargin)

    inside(cpg.typeDecl.name("Pair").l) { case pair :: Nil =>
      pair.fullName shouldBe "rust2cpgtest::Pair"
      pair.member.map(member => (member.name, member.typeFullName)).sorted.l shouldBe List(("0", "i32"), ("1", "bool"))
    }
  }

  "unit struct emits a TypeDecl with no members" in {
    val cpg = code("""
        |struct Marker;
        |""".stripMargin)

    inside(cpg.typeDecl.name("Marker").l) { case marker :: Nil =>
      marker.fullName shouldBe "rust2cpgtest::Marker"
      marker.astParentType shouldBe NodeTypes.METHOD
      marker.astParentFullName shouldBe s"${(Paths.get("src") / "lib.rs").toString}:rust2cpgtest:$globalNamespaceName"
      marker.member shouldBe empty
    }
  }
}
