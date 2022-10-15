package io.joern.pysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.pysrc2cpg.PythonDataflowContext
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DataFlowTests extends AnyFreeSpec with Matchers {

  implicit var context: EngineContext = EngineContext()

  val cpg: Cpg = PythonDataflowContext.buildCpg("""
      |a = 42
      |c = foo(a, b)
      |print(c)
      |""".stripMargin)

  "first test" in {
    val source = cpg.literal("42")
    val sink   = cpg.call.code("print.*").argument
    sink.reachableByFlows(source).size shouldBe 1
  }
}
