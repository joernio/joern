package io.shiftleft.semanticcpg.validation

import flatgraph.misc.TestUtils.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.validation.PostFrontendValidator.ErrorType.NONLOCAL_REF
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PostFrontendValidatorRefTests extends AnyWordSpec with Matchers {

  private def nonLocalViolations(target: NewNode, sameMethod: Boolean = false): Int = {
    val cpg = Cpg.empty
    try {
      val graph        = cpg.graph
      val first        = graph.addNode(NewMethod().name("first").fullName("first"))
      val second       = graph.addNode(NewMethod().name("second").fullName("second"))
      val firstBlock   = graph.addNode(NewBlock().order(1))
      val secondBlock  = graph.addNode(NewBlock().order(2))
      val firstReturn  = graph.addNode(NewMethodReturn().order(3))
      val secondReturn = graph.addNode(NewMethodReturn().order(3))
      val identifier   = graph.addNode(NewIdentifier().name("value").code("value").order(1))
      val declaration  = graph.addNode(target)
      val owner        = if (sameMethod) first else second
      val ownerBlock   = if (sameMethod) firstBlock else secondBlock

      graph.applyDiff { builder =>
        builder.addEdge(first, firstBlock, EdgeTypes.AST)
        builder.addEdge(first, firstReturn, EdgeTypes.AST)
        builder.addEdge(second, secondBlock, EdgeTypes.AST)
        builder.addEdge(second, secondReturn, EdgeTypes.AST)
        builder.addEdge(firstBlock, identifier, EdgeTypes.AST)
        builder.addEdge(identifier, declaration, EdgeTypes.REF)
        target match {
          case _: NewMethodParameterIn => builder.addEdge(owner, declaration, EdgeTypes.AST)
          case _                       => builder.addEdge(ownerBlock, declaration, EdgeTypes.AST)
        }
      }

      val validator = new PostFrontendValidator(cpg, ValidationLevel.V0)
      validator.run()
      validator.getViolationsCompressedWithKey.collect { case (NONLOCAL_REF, _, count) => count }.sum
    } finally cpg.close()
  }

  "Identifier reference validation" should {
    "allow member references across method boundaries" in {
      nonLocalViolations(NewMember().name("value").code("value").order(2)) shouldBe 0
    }

    "reject local references across method boundaries" in {
      nonLocalViolations(NewLocal().name("value").code("value").order(2)) shouldBe 1
    }

    "reject parameter references across method boundaries" in {
      nonLocalViolations(NewMethodParameterIn().name("value").code("value").order(1).index(1)) shouldBe 1
    }

    "allow local references in the same method" in {
      nonLocalViolations(NewLocal().name("value").code("value").order(2), sameMethod = true) shouldBe 0
    }
  }
}
