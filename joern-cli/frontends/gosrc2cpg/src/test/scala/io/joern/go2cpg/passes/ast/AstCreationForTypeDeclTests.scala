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
      cpg.typeDecl("Foo").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::Foo"
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

    "test ast parent structure" in {
      cpg.typeDecl("Foo").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::Foo"
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

    "test parent ast structure" in {
      cpg.typeDecl("Foo").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::Foo"
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

    "test ast parent structure" in {
      cpg.typeDecl("foo").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("foo").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::foo"
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

    "test ast parent structure" in {
      cpg.typeDecl("Sample").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Sample").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::main::Sample"
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

    "test ast parent structure" in {
      cpg.typeDecl("Foo").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::main::Foo"
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

    "test ast parent structure for Foo" in {
      cpg.typeDecl("Foo").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("Foo").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes for Foo" ignore {
      typeDeclNode.fullName shouldBe "test.go::main.<global>::main::Foo"
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

    "test parent ast structure for bar" in {
      cpg.typeDecl("bar").astParentFullName.l.head shouldBe "test.go:main.<global>"
      cpg.typeDecl("bar").astParentType.l.head shouldBe NodeTypes.TYPE_DECL
    }

    "test fullName of TypeDecl nodes for bar" ignore {
      typeDeclNodeBar.fullName shouldBe "test.go::main.<global>::bar"
    }

    "test the modifier for bar" in {
      typeDeclNodeBar.modifier.head.modifierType shouldBe ModifierTypes.PRIVATE
      typeDeclNodeBar.astOut.isModifier.size shouldBe 1
    }
  }
}
