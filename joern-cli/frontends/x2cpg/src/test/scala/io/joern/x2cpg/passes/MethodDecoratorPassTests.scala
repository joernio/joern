package io.joern.x2cpg.passes

import flatgraph.misc.TestUtils.*
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.base.MethodDecoratorPass
import io.joern.x2cpg.testfixtures.EmptyGraphFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MethodDecoratorPassTests extends AnyWordSpec with Matchers {
  "MethodDecoratorTest" in EmptyGraphFixture { graph =>
    val method = graph.addNode(NewMethod())
    val parameterIn = graph.addNode(
      NewMethodParameterIn()
        .code("p1")
        .order(1)
        .name("p1")
        .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
        .typeFullName("some.Type")
        .lineNumber(10)
    )

    // TODO MP get arrow syntax back
//    method --- EdgeTypes.AST --> parameterIn
    graph.applyDiff { diffGraphBuilder =>
      diffGraphBuilder.addEdge(method, parameterIn, EdgeTypes.AST)
    }

    val methodDecorator = new MethodDecoratorPass(new Cpg(graph))
    methodDecorator.createAndApply()

    val parameterOut = parameterIn.asOutput.next()
    parameterOut.code shouldBe "p1"
    parameterOut.order shouldBe 1
    parameterOut.name shouldBe "p1"
    parameterOut.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
    parameterOut.typeFullName shouldBe "some.Type"
    parameterOut.lineNumber.get shouldBe 10

    parameterOut.method shouldBe method
  }

}
