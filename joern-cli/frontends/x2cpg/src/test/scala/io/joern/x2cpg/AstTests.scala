package io.joern.x2cpg

import flatgraph.SchemaViolationException
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, Call, NewCall, NewClosureBinding, NewIdentifier}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AstTests extends AnyWordSpec with Matchers {

  "subTreeCopy" should {

    implicit val mode: ValidationMode = ValidationMode.Disabled

    val foo        = NewCall().name("foo")
    val bar        = NewCall().name("bar").order(1)
    val idName     = NewIdentifier().name("idname").order(1)
    val moo        = NewCall().name("moo").order(1)
    val callInCall = NewCall().name("callincall").order(1)
    val leaf       = NewIdentifier().name("leaf").order(1)

    val testTree = Ast(foo)
      .withChildren(
        List(
          Ast(bar).withChild(Ast(idName)).withArgEdge(bar, idName),
          Ast(moo)
            .withChild(Ast(callInCall).withChild(Ast(leaf)).withArgEdge(callInCall, leaf))
            .withArgEdge(moo, callInCall)
        )
      )
      .withArgEdges(foo, List(bar, moo))

    val copied = testTree.subTreeCopy(moo.asInstanceOf[AstNodeNew], 123)

    "copy root node correctly" in {
      copied.root match {
        case Some(root: NewCall) =>
          root should not be Some(moo)
          root.properties(PropertyNames.Name) shouldBe "moo"
          root.argumentIndex shouldBe 123
        case _ => fail()
      }
    }

    "copy AST edges correctly" in {
      val Seq(_, callInCallClone: NewCall, leafClone: NewIdentifier) = copied.nodes: @unchecked
      callInCallClone.order shouldBe 1
      leafClone.order shouldBe 1

      copied.edges.filter(_.src == callInCallClone).map(_.dst) match {
        case Seq(x: NewIdentifier) =>
          x shouldBe leafClone
          x should not be leaf
          x.name shouldBe "leaf"
        case _ => fail()
      }
    }

    "copy argument edges correctly" in {
      val Seq(edge1, edge2) = copied.argEdges: @unchecked
      edge1 match {
        case AstEdge(m: NewCall, c: NewCall) =>
          m should not be moo
          c should not be callInCall
          m.name shouldBe "moo"
          c.name shouldBe "callincall"
        case _ => fail()
      }

      edge2 match {
        case AstEdge(m: NewCall, c: NewIdentifier) =>
          m.name shouldBe "callincall"
          c.name shouldBe "leaf"
        case _ => fail()
      }
    }

    "preserve order of nodes" in {
      copied.nodes.collect {
        case node: NewCall       => node.name
        case node: NewIdentifier => node.name
      } shouldBe List("moo", "callincall", "leaf")
    }

  }

  "explicit control-structure edge collections" should {

    implicit val mode: ValidationMode = ValidationMode.Disabled

    "be preserved by subTreeCopy" in {
      val controlStructure = NewCall().name("control")
      val condition        = NewCall().name("condition")
      val trueBody         = NewCall().name("trueBody")
      val falseBody        = NewCall().name("falseBody")
      val doBody           = NewCall().name("doBody")
      val tryBody          = NewCall().name("tryBody")
      val catchBody        = NewCall().name("catchBody")
      val finallyBody      = NewCall().name("finallyBody")
      val forInit          = NewCall().name("forInit")
      val forUpdate        = NewCall().name("forUpdate")
      val forBody          = NewCall().name("forBody")

      val tree = Ast(controlStructure)
        .withChild(Ast(condition))
        .withChild(Ast(trueBody))
        .withChild(Ast(falseBody))
        .withChild(Ast(doBody))
        .withChild(Ast(tryBody))
        .withChild(Ast(catchBody))
        .withChild(Ast(finallyBody))
        .withChild(Ast(forInit))
        .withChild(Ast(forUpdate))
        .withChild(Ast(forBody))
        .withConditionEdge(controlStructure, condition)
        .withTrueBodyEdge(controlStructure, trueBody)
        .withFalseBodyEdge(controlStructure, falseBody)
        .withDoBodyEdge(controlStructure, doBody)
        .withTryBodyEdge(controlStructure, tryBody)
        .withCatchBodyEdge(controlStructure, catchBody)
        .withFinallyBodyEdge(controlStructure, finallyBody)
        .withForInitEdge(controlStructure, forInit)
        .withForUpdateEdge(controlStructure, forUpdate)
        .withForBodyEdge(controlStructure, forBody)

      val copied = tree.subTreeCopy(controlStructure.asInstanceOf[AstNodeNew])

      copied.conditionEdges.size shouldBe 1
      copied.trueBodyEdges.size shouldBe 1
      copied.falseBodyEdges.size shouldBe 1
      copied.doBodyEdges.size shouldBe 1
      copied.tryBodyEdges.size shouldBe 1
      copied.catchBodyEdges.size shouldBe 1
      copied.finallyBodyEdges.size shouldBe 1
      copied.forInitEdges.size shouldBe 1
      copied.forUpdateEdges.size shouldBe 1
      copied.forBodyEdges.size shouldBe 1
    }
  }

  "early AST validation" should {

    val call           = NewCall().name("foo")
    val closureBinding = NewClosureBinding().closureBindingId("bar")

    "not throw error if disabled on AST violations" in {
      implicit val mode: ValidationMode = ValidationMode.Disabled
      Ast(call).withChild(Ast(closureBinding))
    }

    "should throw error if enabled on AST violations" in {
      implicit val mode: ValidationMode = ValidationMode.Enabled
      assertThrows[SchemaViolationException](Ast(call).withChild(Ast(closureBinding)))
    }

    "should throw error if enabled on REF violations" in {
      implicit val mode: ValidationMode = ValidationMode.Enabled
      assertThrows[SchemaViolationException](Ast(call).withRefEdge(call, closureBinding))
    }

  }
}
