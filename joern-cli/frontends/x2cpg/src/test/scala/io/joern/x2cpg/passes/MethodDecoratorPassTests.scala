package io.joern.x2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.joern.x2cpg.passes.base.MethodDecoratorPass
import io.joern.x2cpg.testfixtures.EmptyGraphFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.*

class MethodDecoratorPassTests extends AnyWordSpec with Matchers {
  "MethodDecoratorTest" in EmptyGraphFixture { graph =>
    val method = graph + NodeTypes.METHOD
    val parameterIn = graph
      .+(
        NodeTypes.METHOD_PARAMETER_IN,
        Properties.Code               -> "p1",
        Properties.Order              -> 1,
        Properties.Name               -> "p1",
        Properties.EvaluationStrategy -> EvaluationStrategies.BY_REFERENCE,
        Properties.TypeFullName       -> "some.Type",
        Properties.LineNumber         -> 10
      )
      .asInstanceOf[MethodParameterIn]

    method --- EdgeTypes.AST --> parameterIn

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
