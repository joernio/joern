package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  ModifierTypes,
  NodeTypes,
  Operators
}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class AstCreationForTypeDeclTests extends GoCodeToCpgSuite {
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

    "test parent ast structure" in {
      cpg.typeDecl("Foo").astParent.isMethod.name.l.head shouldBe "main.<global>"
      cpg.typeDecl("Foo").astParent.isMethod.fullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.METHOD
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::Foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.l.size shouldBe 1
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

    "test ast parent structure" in {
      cpg.typeDecl("Foo").astParent.isMethod.name.l.head shouldBe "main.<global>"
      cpg.typeDecl("Foo").astParent.isMethod.fullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.METHOD
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::Foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.l.size shouldBe 1
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

    "test parent ast structure" in {
      cpg.typeDecl("Foo").astParent.isMethod.name.l.head shouldBe "main.<global>"
      cpg.typeDecl("Foo").astParent.isMethod.fullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.METHOD
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::Foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PUBLIC
      typeDeclNode.astOut.isModifier.l.size shouldBe 1
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

    "test ast parent structure" in {
      cpg.typeDecl("foo").astParent.isMethod.name.l.head shouldBe "main.<global>"
      cpg.typeDecl("foo").astParent.isMethod.fullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("foo").astParentType.l.head shouldBe NodeTypes.METHOD
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::foo"
    }

    "test the modifier" in {
      typeDeclNode.modifier.head.modifierType shouldBe ModifierTypes.PRIVATE
      typeDeclNode.astOut.isModifier.l.size shouldBe 1
    }
  }

}
