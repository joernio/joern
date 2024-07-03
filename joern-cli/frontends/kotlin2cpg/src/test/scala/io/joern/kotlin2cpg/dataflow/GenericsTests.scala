package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class GenericsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with simple fn using type parameter with no upper bound" should {
    val cpg = code("""
        |fun main() = f1("AVALUE")
        |fun <T> f1(p: T) = println(p)
        |""".stripMargin)

    "should find a flow the argument of the callsite to a call in the fn body" in {
      val source = cpg.literal.code(".*AVALUE.*")
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(\"AVALUE\")", Some(2)), ("f1(p)", Some(3)), ("println(p)", Some(3))))
    }
  }

  "CPG for code with simple fn using type parameter with upper bound" should {
    val cpg = code("""
       |fun main() = f1(0.30000000000000004f)
       |fun <T : kotlin.Number> f1(p: T) = println(p)
       |""".stripMargin)

    "should find a flow the argument of the callsite to a call in the fn body" in {
      val source = cpg.literal.codeExact("0.30000000000000004f")
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(List(("f1(0.30000000000000004f)", Some(2)), ("f1(p)", Some(3)), ("println(p)", Some(3))))
    }
  }
}
