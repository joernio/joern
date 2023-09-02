package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import scala.collection.immutable.List
import io.joern.gosrc2cpg.astcreation.Defines

class ASTCreationForStructureTests extends GoCodeToCpgSuite {

  "structure with single element" should {

    val cpg = code("""
          package main
          |
          |type Person struct {
          |    Name string
          |}
  """.stripMargin)
    cpg.typeDecl.name("Person").size shouldBe 1
    cpg.typeDecl.name("Person").head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
    cpg.typeDecl.name("Person").member.size shouldBe 1

    "Check member node under type decl node" in {
      val typeDeclNodes = cpg.typeDecl("Person").l
      typeDeclNodes.member.name("Name").size shouldBe 1
      typeDeclNodes.member.name("Name").typeFullName.head shouldBe "string"

    }
  }

  "structure with multiple element" should {

    val cpg = code("""
          package main
          |
          |type Person struct {
          |    Name string
          |    Age  int
          |}
  """.stripMargin)
    cpg.typeDecl.name("Person").size shouldBe 1
    cpg.typeDecl.name("Person").head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
    cpg.typeDecl.name("Person").member.size shouldBe 2

    "Check member node under type decl node" in {
      val typeDeclNodes = cpg.typeDecl("Person").l
      typeDeclNodes.member.name("Name").size shouldBe 1
      typeDeclNodes.member.name("Age").size shouldBe 1
      typeDeclNodes.member.name("Name").typeFullName.head shouldBe "string"
      typeDeclNodes.member.name("Age").typeFullName.head shouldBe "int"
    }

    "Traversing member node to typedecl node" in {
      val typeDeclNode = cpg.member.name("Name").typeDecl.head
      typeDeclNode.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
    }
  }

  "structure with element of type another structure" should {

    val cpg = code("""
          package main
          |
          |type Person struct {
          |    Name string
          |    Age  int
          |}
          |
          |type Employee struct {
          |    person Person
          |    Salary  int
          |}
  """.stripMargin)
    cpg.typeDecl.name("Person").size shouldBe 1
    cpg.typeDecl.name("Employee").size shouldBe 1

    cpg.typeDecl.name("Person").head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
    cpg.typeDecl.name("Employee").head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Employee"

    "Check member node under type decl node for Person typeDecl" in {
      val personNode = cpg.typeDecl.name("Person").l

      personNode.member.size shouldBe 2
      personNode.member.name("Name").size shouldBe 1
      personNode.member.name("Age").size shouldBe 1
      personNode.member.name("Name").typeFullName.head shouldBe "string"
      personNode.member.name("Age").typeFullName.head shouldBe "int"
    }

    "Check member node under type decl node for Employee typeDecl" in {
      val employeeNode = cpg.typeDecl("Employee").l
      employeeNode.member.size shouldBe 2
      employeeNode.member.name("Salary").size shouldBe 1
      employeeNode.member.name("Salary").typeFullName.head shouldBe "int"
      employeeNode.member.name("person").typeFullName.head shouldBe "main.Person"
    }

    "traverse from member node to typedecl node" in {
      val memberNodes = cpg.member.l
      memberNodes.name("person").typeDecl.head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Employee"
      memberNodes.name("Salary").typeDecl.head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Employee"

      memberNodes.name("Name").typeDecl.head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
      memberNodes.name("Age").typeDecl.head.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"

    }

  }
}
