package io.joern.x2cpg.passes

import io.shiftleft.OverflowDbTestInstance
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.joern.x2cpg.passes.base.ContainsEdgePass
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.*

import scala.jdk.CollectionConverters.*

class ContainsEdgePassTest extends AnyWordSpec with Matchers {

  import ContainsEdgePassTest.Fixture

  "Files " can {
    "contain Methods" in Fixture { fixture =>
      fixture.methodVertex.in(EdgeTypes.CONTAINS).asScala.toList shouldBe List(fixture.fileVertex)
    }
    "contain Classes" in Fixture { fixture =>
      fixture.typeDeclVertex.in(EdgeTypes.CONTAINS).asScala.toList shouldBe List(fixture.fileVertex)
    }
  }

  "Classes " can {
    "contain Methods" in Fixture { fixture =>
      fixture.typeMethodVertex.in(EdgeTypes.CONTAINS).asScala.toList shouldBe List(fixture.typeDeclVertex)
    }
  }

  "Methods " can {
    "contain Methods" in Fixture { fixture =>
      fixture.innerMethodVertex.in(EdgeTypes.CONTAINS).asScala.toList shouldBe List(fixture.methodVertex)
    }
    "contain expressions" in Fixture { fixture =>
      fixture.expressionVertex.in(EdgeTypes.CONTAINS).asScala.toList shouldBe List(fixture.methodVertex)
      fixture.innerExpressionVertex.in(EdgeTypes.CONTAINS).asScala.toList shouldBe List(fixture.innerMethodVertex)
    }
  }

}

object ContainsEdgePassTest {
  private class Fixture {
    private val graph = OverflowDbTestInstance.create

    val fileVertex            = graph + NodeTypes.FILE
    val typeDeclVertex        = graph + NodeTypes.TYPE_DECL
    val typeMethodVertex      = graph + NodeTypes.METHOD
    val methodVertex          = graph + NodeTypes.METHOD
    val innerMethodVertex     = graph + NodeTypes.METHOD
    val expressionVertex      = graph + NodeTypes.CALL
    val innerExpressionVertex = graph + NodeTypes.CALL

    fileVertex --- EdgeTypes.AST --> typeDeclVertex
    typeDeclVertex --- EdgeTypes.AST --> typeMethodVertex

    fileVertex --- EdgeTypes.AST --> methodVertex
    methodVertex --- EdgeTypes.AST --> innerMethodVertex
    methodVertex --- EdgeTypes.AST --> expressionVertex
    innerMethodVertex --- EdgeTypes.AST --> innerExpressionVertex

    val containsEdgeCalculator = new ContainsEdgePass(new Cpg(graph))
    containsEdgeCalculator.createAndApply()
  }

  private object Fixture {
    def apply[T](fun: Fixture => T): T = {
      val fixture = new Fixture()
      try fun(fixture)
      finally fixture.graph.close()
    }
  }
}
