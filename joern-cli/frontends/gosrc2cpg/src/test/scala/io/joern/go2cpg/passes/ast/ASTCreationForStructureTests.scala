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
    val List(typeDeclNode) = cpg.typeDecl.name("Person").l
    typeDeclNode.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
    typeDeclNode.member.size shouldBe 1

    "Check member node under type decl node" in {
      val List(memberNode) = typeDeclNode.member.name("Name").l
      memberNode.typeFullName shouldBe "string"
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
    val List(typeDeclNode) = cpg.typeDecl.name("Person").l
    typeDeclNode.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
    typeDeclNode.member.size shouldBe 2

    "Check member node under type decl node" in {
      val List(typeDeclNode) = cpg.typeDecl("Person").l
      typeDeclNode.member.name("Name").size shouldBe 1
      typeDeclNode.member.name("Age").size shouldBe 1

      val List(memberNodeName) = typeDeclNode.member.name("Name").l
      memberNodeName.typeFullName shouldBe "string"

      val List(memberNodeAge) = typeDeclNode.member.name("Age").l
      memberNodeAge.typeFullName shouldBe "int"
    }

    "Traversing member node to typedecl node" in {
      val List(memberNameNode) = cpg.member.name("Name").typeDecl.l
      memberNameNode.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
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
    val List(typeDeclPersonNode)   = cpg.typeDecl.name("Person").l
    val List(typeDeclEmployeeNode) = cpg.typeDecl.name("Employee").l

    typeDeclPersonNode.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"
    typeDeclEmployeeNode.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Employee"

    "Check member node under type decl node for Person typeDecl" in {

      typeDeclPersonNode.member.size shouldBe 2
      typeDeclPersonNode.member.name("Name").size shouldBe 1
      typeDeclPersonNode.member.name("Age").size shouldBe 1

      val List(memberNodeName) = typeDeclPersonNode.member.name("Name").l
      memberNodeName.typeFullName shouldBe "string"

      val List(memberNodeAge) = typeDeclPersonNode.member.name("Age").l
      memberNodeAge.typeFullName shouldBe "int"
    }

    "Check member node under type decl node for Employee typeDecl" in {
      typeDeclEmployeeNode.member.size shouldBe 2
      typeDeclEmployeeNode.member.name("Salary").size shouldBe 1

      val List(memberNodeSalary) = typeDeclEmployeeNode.member.name("Salary").l
      memberNodeSalary.typeFullName shouldBe "int"

      val List(memberNodePerson) = typeDeclEmployeeNode.member.name("person").l
      memberNodePerson.typeFullName shouldBe "main.Person"
    }

    "traverse from member node to typedecl node" in {
      val List(memberPersonNode) = cpg.member.name("person").l
      memberPersonNode.typeDecl.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Employee"

      val List(memberSalaryNode) = cpg.member.name("Salary").l
      memberSalaryNode.typeDecl.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Employee"

      val List(memberNameNode) = cpg.member.name("Name").l
      memberNameNode.typeDecl.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"

      val List(memberAgeNode) = cpg.member.name("Age").l
      memberAgeNode.typeDecl.fullName shouldBe "main" + Defines.qualifiedNameSeparator + "Person"

    }

  }
}
