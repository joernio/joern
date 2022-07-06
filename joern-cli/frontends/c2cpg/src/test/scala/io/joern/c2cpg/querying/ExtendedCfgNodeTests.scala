package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.language._

class ExtendedCfgNodeTests extends DataFlowCodeToCpgSuite {

  implicit val resolver: NoResolve.type = NoResolve
  implicit var s: Semantics             = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    s = semantics
  }

  private val cpg = code("""
      |int foo(int y) {
      | int x = source();
      | x += y;
      | sink(y);
      |}
      |""".stripMargin)

  "allow traversing from argument of sink back to param via `ddgIn`" in {
    inside(cpg.method("sink").parameter.argument.ddgIn.l) { case List(param: nodes.MethodParameterIn) =>
      param.name shouldBe "y"
    }
  }

  "allow traversing from argument node to param via `ddgIn`" in {
    inside(cpg.method("sink").parameter.argument.l) { case List(t) =>
      t.code shouldBe "y"
      inside(t.ddgIn.l) { case List(param: nodes.MethodParameterIn) =>
        param.name shouldBe "y"
      }
    }
  }

  "allow traversing from argument back to param while inspecting edge" in {
    inside(cpg.method("sink").parameter.argument.ddgInPathElem.l) { case List(pathElem) =>
      pathElem.outEdgeLabel shouldBe "y"
      pathElem.node.isInstanceOf[nodes.MethodParameterIn] shouldBe true
    }
  }

}
