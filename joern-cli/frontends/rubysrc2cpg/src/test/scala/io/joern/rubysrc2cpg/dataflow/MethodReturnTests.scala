package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodReturnTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "flow from method parameter to explicit return of the same variable" in {
    val cpg = code("""
        |def f(x)
        | return x
        |end
        |""".stripMargin)
    val source = cpg.method.name("f").parameter
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(x)", 2), ("return x", 3), ("RET", 2)))
  }

  "flow from method parameter to implicit return of the same variable" in {
    val cpg = code("""
        |def f(x)
        | x
        |end
        |""".stripMargin)
    val source = cpg.method.name("f").parameter
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(x)", 2), ("x", 3), ("RET", 2)))
  }

  "flow from endless method parameter to implicit return of the same variable" in {
    val cpg = code("""
        |def f(x) = x
        |""".stripMargin)
    val source = cpg.method.name("f").parameter
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(x)", 2), ("x", 2), ("RET", 2)))
  }

  "flow from method parameter to implicit return via assignment to temporary variable" in {
    val cpg = code("""
        |def f(x)
        | y = x
        |end
        |""".stripMargin)
    val source = cpg.method.name("f").parameter
    val sink   = cpg.method.name("f").methodReturn
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("f(x)", 2), ("y = x", 3), ("y", 3), ("y = x", 3), ("RET", 2)))
  }

}
