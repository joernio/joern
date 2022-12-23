package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class ResultTableTests extends AnyWordSpec with Matchers {

  "ResultTable::add" should {

    val cpg = MockCpg()
      .withCustom({ (diffGraph, _) =>
        diffGraph.addNode(nodes.NewLiteral().code("foo"))
        diffGraph.addNode(nodes.NewLiteral().code("bar"))
      })
      .cpg

    "append to existing results on multiple inserts for same key" in {
      val node1 = cpg.literal.head
      val node2 = cpg.literal.last
      val table = new ResultTable
      val res1  = Vector(ReachableByResult(List(TaskFingerprint(node1, List(), 0)), Vector(PathElement(node1))))
      val res2  = Vector(ReachableByResult(List(TaskFingerprint(node1, List(), 0)), Vector(PathElement(node2))))
      table.add(TaskFingerprint(node1, List(), 0), res1)
      table.add(TaskFingerprint(node1, List(), 0), res2)
      table.get(TaskFingerprint(node1, List(), 0)) match {
        case Some(results) =>
          results.flatMap(_.path.map(_.node.id)) shouldBe List(node1.id, node2.id)
        case None => fail()
      }
    }

  }

  "ResultTable::createFromTable" should {

    val cpg = MockCpg()
      .withCustom({ (diffGraph, _) =>
        diffGraph.addNode(nodes.NewLiteral().code("foo"))
        diffGraph.addNode(nodes.NewLiteral().code("bar"))
        diffGraph.addNode(nodes.NewLiteral().code("woo"))
        diffGraph.addNode(nodes.NewLiteral().code("moo"))
      })
      .cpg

    "correctly combine path and path stored in table on createFromTable" in {
      val node1               = cpg.literal.code("foo").head
      val pivotNode           = cpg.literal.code("bar").head
      val node3               = cpg.literal.code("woo").head
      val node4               = cpg.literal.code("moo").head
      val pathContainingPivot = Vector(PathElement(node4), PathElement(pivotNode), PathElement(node3))
      val table               = new ResultTable
      table.add(
        TaskFingerprint(pivotNode, List(), 0),
        Vector(ReachableByResult(List(TaskFingerprint(node1, List(), 0)), pathContainingPivot))
      )
      table.createFromTable(PathElement(pivotNode), List(), Vector(PathElement(node1)), 0) match {
        case Some(Vector(ReachableByResult(_, path, _))) =>
          path.map(_.node.id) shouldBe List(node4.id, pivotNode.id, node1.id)
        case _ => fail()
      }
    }
  }

}
