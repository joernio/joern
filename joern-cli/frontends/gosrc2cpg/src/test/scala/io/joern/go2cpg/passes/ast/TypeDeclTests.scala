package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class TypeDeclTests extends GoCodeToCpgSuite {
  "when struct type is declared with a member node" should {
    val cpg = code(
      """package main
            |type Foo struct {
            |	bar string
            |}
            |func main() {}
            |""".stripMargin,
      "test.go"
    )

    val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
    "test basic ast structure" in {
      typeDeclNode.code shouldBe "Foo struct {\n\tbar string\n}"
      typeDeclNode.lineNumber shouldBe Some(2)
      typeDeclNode.columnNumber shouldBe Some(6)
    }

    "test fullName of TypeDecl nodes" in {
      typeDeclNode.fullName shouldBe "main.Foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.size shouldBe 1
    }
  }

  "when custom type is created using a primitive" should {
    val cpg = code(
      """package main
            |type Foo string
            |func main() {}
            |""".stripMargin,
      "test.go"
    )
    val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
    "test basic ast structure" in {
      typeDeclNode.code shouldBe "Foo string"
      typeDeclNode.lineNumber shouldBe Some(2)
      typeDeclNode.columnNumber shouldBe Some(6)
    }

    "test fullName of TypeDecl nodes" in {
      typeDeclNode.fullName shouldBe "main.Foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.size shouldBe 1
    }
  }

  "when interface is declared" should {
    val cpg = code(
      """package main
            |type Foo interface {}
            |func main() {}
            |""".stripMargin,
      "test.go"
    )
    val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
    "test basic ast structure" in {
      typeDeclNode.code shouldBe "Foo interface {}"
      typeDeclNode.lineNumber shouldBe Some(2)
      typeDeclNode.columnNumber shouldBe Some(6)
    }

    "test fullName of TypeDecl nodes" in {
      typeDeclNode.fullName shouldBe "main.Foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.size shouldBe 1
    }
  }

  "when a private TypeDecl node is created" should {
    val cpg = code(
      """package main
        |type foo int
        |func main() {}
        |""".stripMargin,
      "test.go"
    )

    val List(typeDeclNode) = cpg.typeDecl.nameExact("foo").l
    "test basic ast structure" in {
      typeDeclNode.code shouldBe "foo int"
      typeDeclNode.lineNumber shouldBe Some(2)
      typeDeclNode.columnNumber shouldBe Some(6)
    }

    "test fullName of TypeDecl nodes" in {
      typeDeclNode.fullName shouldBe "main.foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PRIVATE
      typeDeclNode.astOut.isModifier.size shouldBe 1
    }
  }

  "when a TypeDecl node is defined inside a function" should {
    val cpg = code(
      """package main
       |func main() {
       |	type Sample struct {
       |		foo string
       |	}
       |}""".stripMargin,
      "test.go"
    )
    val List(typeDeclNode) = cpg.typeDecl.nameExact("Sample").l
    "test basic ast structure" in {
      typeDeclNode.code shouldBe "Sample struct {\n\t\tfoo string\n\t}"
      typeDeclNode.lineNumber shouldBe Some(3)
      typeDeclNode.columnNumber shouldBe Some(7)
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "main.main.Sample"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.size shouldBe 1
    }

  }

  "when a single TypeDecl is defined using a prefix" should {
    val cpg = code(
      """package main
        |type (
        | Foo struct {}
        |)""".stripMargin,
      "test.go"
    )

    val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
    "test basic ast structure" in {
      typeDeclNode.code shouldBe "Foo struct {}"
      typeDeclNode.lineNumber shouldBe Some(3)
      typeDeclNode.columnNumber shouldBe Some(2)
    }

    "test fullName of TypeDecl nodes" in {
      typeDeclNode.fullName shouldBe "main.Foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.size shouldBe 1
    }

  }

  "when multiple TypeDecls are defined using a prefix" should {
    val cpg = code(
      """package main
        |type (
        | Foo struct {}
        | bar interface {}
        |)""".stripMargin,
      "test.go"
    )
    val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
    "test basic ast structure for Foo" in {
      typeDeclNode.code shouldBe "Foo struct {}"
      typeDeclNode.lineNumber shouldBe Some(3)
      typeDeclNode.columnNumber shouldBe Some(2)
    }

    "test fullName of TypeDecl nodes for Foo" in {
      typeDeclNode.fullName shouldBe "main.Foo"
    }

    "test the modifier for Foo" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.size shouldBe 1
    }

    val List(typeDeclNodeBar) = cpg.typeDecl.nameExact("bar").l
    "test basic ast structure for bar" in {
      typeDeclNodeBar.code shouldBe "bar interface {}"
      typeDeclNodeBar.lineNumber shouldBe Some(4)
      typeDeclNodeBar.columnNumber shouldBe Some(2)
    }

    "test fullName of TypeDecl nodes for bar" in {
      typeDeclNodeBar.fullName shouldBe "main.bar"
    }

    "test the modifier for bar" in {
      typeDeclNodeBar.modifier.head.modifierType shouldBe ModifierTypes.PRIVATE
      typeDeclNodeBar.astOut.isModifier.size shouldBe 1
    }
  }

  "when multiple TypeDecls are defined having multiple members" should {
    val cpg = code("""
        |package main
        |type House struct {
        |   name Name
        |   owner Person
        |}
        |type Person struct {
        |   firstName string
        |   lastName string
        |}
        |type Address struct { shortAddress string }
        |type Name struct { address Address }
        |""".stripMargin)

    "test basic ast structure for House" in {
      val List(typeDeclNode) = cpg.typeDecl.name("House").l
      typeDeclNode.fullName shouldBe "main.House"
      typeDeclNode.member.size shouldBe 2
      val List(name, owner) = typeDeclNode.member.l
      name.code shouldBe "name"
      name.typeFullName shouldBe "main.Name"
      owner.code shouldBe "owner"
      owner.typeFullName shouldBe "main.Person"
    }

    "test basic ast structure for Person" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Person").l
      typeDeclNode.fullName shouldBe "main.Person"
      typeDeclNode.member.size shouldBe 2
      val List(firstname, lastname) = typeDeclNode.member.l
      firstname.code shouldBe "firstName"
      firstname.typeFullName shouldBe "string"
      lastname.code shouldBe "lastName"
      lastname.typeFullName shouldBe "string"
    }

    "test basic ast structure for Address" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Address").l
      typeDeclNode.fullName shouldBe "main.Address"
      typeDeclNode.member.size shouldBe 1
      val List(name) = typeDeclNode.member.l
      name.code shouldBe "shortAddress"
      name.typeFullName shouldBe "string"
    }

    "test basic ast structure for Name" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Name").l
      typeDeclNode.fullName shouldBe "main.Name"
      typeDeclNode.member.size shouldBe 1
      val List(name) = typeDeclNode.member.l
      name.code shouldBe "address"
      name.typeFullName shouldBe "main.Address"
    }
  }

  "when multiple TypeDecls are defined having array of complex(structre-array)" should {
    val cpg = code("""
        |package main
        |type House struct { names []Name }
        |type Address struct { shortAddress string }
        |type Name struct { address Address }
        |""".stripMargin)

    "test basic ast structure for House" in {
      val List(typeDeclNode) = cpg.typeDecl.name("House").l
      typeDeclNode.fullName shouldBe "main.House"
      typeDeclNode.member.size shouldBe 1
      val List(name) = typeDeclNode.member.l
      name.code shouldBe "names"
      name.typeFullName shouldBe "[]main.Name"
    }

    "test basic ast structure for Address" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Address").l
      typeDeclNode.fullName shouldBe "main.Address"
      typeDeclNode.member.size shouldBe 1
      val List(name) = typeDeclNode.member.l
      name.code shouldBe "shortAddress"
      name.typeFullName shouldBe "string"
    }

    "test basic ast structure for Name" in {
      val List(typeDeclNode) = cpg.typeDecl.name("Name").l
      typeDeclNode.fullName shouldBe "main.Name"
      typeDeclNode.member.size shouldBe 1
      val List(name) = typeDeclNode.member.l
      name.code shouldBe "address"
      name.typeFullName shouldBe "main.Address"
    }
  }
}

//TODO: Add unit tests for nested struct within a block
//func bar (x int) int {
//  if true {
//    type mx struct {
//      something string
//    }
//  }
//}
