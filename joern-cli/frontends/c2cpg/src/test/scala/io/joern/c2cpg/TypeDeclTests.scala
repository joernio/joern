package io.joern.c2cpg

import io.joern.c2cpg.fixtures.TestProjectFixture
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.codepropertygraph.generated.nodes.{Member, TypeDecl}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb._
import overflowdb.traversal._

class TypeDeclTests extends AnyWordSpec with Matchers {

  private val fixture: TestProjectFixture = TestProjectFixture("typedecl")

  "Type decl test project" should {
    "contain one internal type decl node for Foo" in {
      val typeDeclNodes = fixture.traversalSource
        .label(TypeDecl.Label)
        .has(Properties.NAME -> "Foo")
        .l
      typeDeclNodes.size shouldBe 1
      typeDeclNodes.head.property(Properties.IS_EXTERNAL) shouldBe false
    }

    "contain three member nodes" in {
      fixture.traversalSource.label(Member.Label).l.size shouldBe 3
    }

    "contain edges from Foo to three members" in {
      val members = fixture.traversalSource
        .label(TypeDecl.Label)
        .out("AST")
        .hasLabel(Member.Label)
        .l
      members.size shouldBe 3
    }

    "contain correct code fields for all members" in {
      fixture.traversalSource.label(Member.Label).property(Properties.CODE).toSetMutable shouldBe Set("x", "y", "*foo")
    }

  }
}
