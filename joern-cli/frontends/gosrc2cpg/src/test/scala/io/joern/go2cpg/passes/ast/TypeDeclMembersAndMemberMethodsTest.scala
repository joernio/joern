package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import scala.collection.immutable.List
import io.joern.gosrc2cpg.astcreation.Defines

class TypeDeclMembersAndMemberMethodsTest extends GoCodeToCpgSuite {

  "structure with single element" should {

    val cpg = code("""
          package main
          |
          |type Person struct {
          |    Name string
          |}
  """.stripMargin)
    val List(typeDeclNode) = cpg.typeDecl.name("Person").l
    typeDeclNode.fullName shouldBe "main" + Defines.dot + "Person"
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
    typeDeclNode.fullName shouldBe "main" + Defines.dot + "Person"
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
      memberNameNode.fullName shouldBe "main" + Defines.dot + "Person"
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

    typeDeclPersonNode.fullName shouldBe "main" + Defines.dot + "Person"
    typeDeclEmployeeNode.fullName shouldBe "main" + Defines.dot + "Employee"

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
      memberPersonNode.typeDecl.fullName shouldBe "main" + Defines.dot + "Employee"

      val List(memberSalaryNode) = cpg.member.name("Salary").l
      memberSalaryNode.typeDecl.fullName shouldBe "main" + Defines.dot + "Employee"

      val List(memberNameNode) = cpg.member.name("Name").l
      memberNameNode.typeDecl.fullName shouldBe "main" + Defines.dot + "Person"

      val List(memberAgeNode) = cpg.member.name("Age").l
      memberAgeNode.typeDecl.fullName shouldBe "main" + Defines.dot + "Person"

    }

  }

  "structure with associated method" should {

    val cpg = code("""
        |package main
        |
        |type Rect struct {
        |  len, wid int
        |}
        |
        |func (re Rect) Area_by_value() int {
        |  return re.len * re.wid
        |}
        |
        |func (re *Rect) Area_by_reference() int {
        |  return re.len * re.wid
        |}
        |""".stripMargin)

    "Check method node under type decl node" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Rect").l
      typeDeclNode.astChildren.isMethod.size shouldBe 2

      val List(areaByValueNode, areaByReferenceNode) = typeDeclNode.astChildren.isMethod.l
      areaByValueNode.name shouldBe "Area_by_value"
      areaByValueNode.fullName shouldBe "main.Rect.Area_by_value"
      areaByReferenceNode.name shouldBe "Area_by_reference"
      areaByReferenceNode.fullName shouldBe "main.Rect.Area_by_reference"
    }

    "Check 'this'/receiver parameter node" in {
      cpg.parameter.name("re").size shouldBe 2
      val List(thisParam, thisParamsec) = cpg.parameter.name("re").l
      thisParam.order shouldBe 0
      thisParam.index shouldBe 0
      thisParamsec.order shouldBe 0
      thisParamsec.index shouldBe 0
    }

    "Traversal from 'this'/receiver parameter to method node" in {
      cpg.parameter.name("re").method.name.l shouldBe List("Area_by_value", "Area_by_reference")
    }

    "Traversal from method to 'this'/receiver parameter node" in {
      cpg.method("Area_by_value").parameter.name.l shouldBe List("re")
    }
  }

  "structure with no associated method" should {

    val cpg = code("""
        package main
        |
        |type Rect struct {
        |  len, wid int
        |}
        |
        |func (c Circle) Area() float64 {
        |    return 3.14 * c.radius * c.radius
        |}
        |""".stripMargin)

    "Check method node under type decl node" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Rect").l
      typeDeclNode.astChildren.isMethod.size shouldBe 0

    }
  }

  "structure with comma separated member" should {

    val cpg = code("""
        |package main
        |
        |type Rect struct {
        |  len, wid int
        |}
        |""".stripMargin)

    "Check number of member nodes" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Rect").l
      typeDeclNode.member.size shouldBe 2

      val List(lenMemberNode, widMemberNode) = typeDeclNode.member.l
      lenMemberNode.typeFullName shouldBe "int"
      widMemberNode.typeFullName shouldBe "int"

    }
  }
  "Method defined on struct type without receiver variable name" should {
    val cpg = code("""
        |package main
        |type StringAlias struct {
        |	name string
        |}
        |func (StringAlias) SomeMethod() {
        |}
        |func (StringAlias) SomeMethodOne() {
        |}
        |""".stripMargin)

    "check method nodes" in {
      val List(typeDeclNode) = cpg.typeDecl.name("StringAlias").l
      typeDeclNode.astChildren.isMethod.size shouldBe 2

      val List(areaByValueNode, areaByReferenceNode) = typeDeclNode.astChildren.isMethod.l
      areaByValueNode.name shouldBe "SomeMethod"
      areaByValueNode.fullName shouldBe "main.StringAlias.SomeMethod"
      areaByReferenceNode.name shouldBe "SomeMethodOne"
      areaByReferenceNode.fullName shouldBe "main.StringAlias.SomeMethodOne"
    }
  }

  "Member method of Struct type defined in another file but in the same package" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package main
        |type Sample struct {
        |	name int
        |}
        |""".stripMargin,
      "lib.go"
    ).moreCode(
      """
        |package main
        |func (sample Sample) someProcessing() int{
        |  return 0
        |}
        |func foo(argc int, argv Sample) {
        |}
        |""".stripMargin,
      "main.go"
    )

    "Have typeDecl created " in {
      cpg.typeDecl("Sample").fullName.l shouldBe List("main.Sample")
    }

    "Have member method part of typeDecl" in {
      cpg.typeDecl("Sample").method.fullName.l shouldBe List("main.Sample.someProcessing")
    }
  }
}
