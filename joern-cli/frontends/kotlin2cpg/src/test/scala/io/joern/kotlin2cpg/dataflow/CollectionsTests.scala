package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CollectionsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with call to array index access" should {
    val cpg = code("""
      |fun f1(p: String) {
      |    val aList = listOf(p, "two", "three")
      |    println(aList)
      |}
      |""".stripMargin)

    "should find a flow through index access operator" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("listOf(p, \"two\", \"three\")", Some(3)),
            ("val aList = listOf(p, \"two\", \"three\")", Some(3)),
            ("println(aList)", Some(4))
          )
        )
    }
  }

  "CPG for code with call to map index access" should {
    val cpg = code("""
      |fun f1(p: String) {
      |    val aMap = mapOf("one" to p, "two" to "q")
      |    println(aMap["one"])
      |}
      |""".stripMargin)

    "should find a flow through index access operator" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("\"one\" to p", Some(3)),
            ("mapOf(\"one\" to p, \"two\" to \"q\")", Some(3)),
            ("val aMap = mapOf(\"one\" to p, \"two\" to \"q\")", Some(3)),
            ("println(aMap[\"one\"])", Some(4))
          )
        )
    }
  }
}
