package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ForTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with `for-in` control structure" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |    val l = listOf(p, 2, 3)
      |    for (one in l) { println(one) }
      |}
      |""".stripMargin)

    "should find a flow through body of the control structure" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("listOf(p, 2, 3)", Some(3)),
            ("val l = listOf(p, 2, 3)", Some(3)),
            ("l.iterator()", None),
            ("l.iterator()", Some(4)),
            ("iterator_1 = l.iterator()", None),
            ("iterator_1.next()", None),
            ("iterator_1.next()", Some(4)),
            ("one = iterator_1.next()", None),
            ("println(one)", Some(4))
          )
        )
    }
  }
}
