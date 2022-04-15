package io.joern.x2cpg

import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, HasName, NewCall, NewIdentifier}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AstTests extends AnyWordSpec with Matchers {

  "subTreeCopy" should {

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
          root.properties("NAME") shouldBe "moo"
          root.argumentIndex shouldBe 123
        case _ => fail()
      }
    }

    "copy AST edges correctly" in {
      val Seq(_, callInCallClone: NewCall, leafClone: NewIdentifier) = copied.nodes
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
      val Seq(edge1, edge2) = copied.argEdges
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
      copied.nodes.collect { case x: HasName => x.name } shouldBe List("moo", "callincall", "leaf")
    }

  }
}
