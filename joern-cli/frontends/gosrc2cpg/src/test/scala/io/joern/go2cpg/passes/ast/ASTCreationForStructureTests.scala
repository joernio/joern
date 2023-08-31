package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import scala.collection.immutable.List

class ASTCreationForStructureTests extends GoCodeToCpgSuite {
  "AST Creation for structures" should {
    "structure with single element" in {

      val cpg = code("""
          package main
          |
          |type Person struct {
          |    Name string
          |}
  """.stripMargin)
      cpg.typeDecl.name("Person").size shouldBe 1
      cpg.typeDecl.name("Person").head.fullName shouldBe "Person"
      cpg.typeDecl.name("Person").member.size shouldBe 1
      cpg.typeDecl("Person").member.name("Name").size shouldBe 1
    }

    "structure with multiple element" in {

      val cpg = code("""
          package main
          |
          |type Person struct {
          |    Name string
          |    Age  int
          |}
  """.stripMargin)
      cpg.typeDecl.name("Person").size shouldBe 1
      cpg.typeDecl.name("Person").head.fullName shouldBe "Person"
      cpg.typeDecl.name("Person").member.size shouldBe 2
      cpg.typeDecl("Person").member.name("Name").size shouldBe 1
      cpg.typeDecl("Person").member.name("Age").size shouldBe 1
    }
  }
}
