package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DestructuringTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with data class with two defined properties" should {
    val cpg = code("""
      |
      |package no.such.pkg
      |data class Technique(val name: String, val topChunkSize: Int)
      |fun printTechnique(p1: String, p2: Int) {
      |    val (name, id) = Technique(p1, p2)
      |    println(name)
      |    println(id)
      |}
      |fun main() = printTechnique("House of Force", -1)
      |""".stripMargin)

    "find a flow for the first property through the destructuring declaration" in {
      val source = cpg.method.name("printTechnique").parameter.nameExact("p1")
      val sink   = cpg.method.name("println").callIn.argument.codeExact("name")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("printTechnique(p1, p2)", Some(5)),
            ("<init>", Some(6)),
            ("<init>(this, name, topChunkSize)", Some(4)),
            ("this.name = name", Some(4)),
            ("this.name = name", None),
            ("RET", None),
            ("<init>", Some(6)),
            ("tmp_1.component1()", Some(6)),
            ("component1(this)", None),
            ("RET", None),
            ("RET", Some(4)),
            ("RET", None),
            ("tmp_1.component1()", Some(6)),
            ("name = tmp_1.component1()", Some(6)),
            ("println(name)", Some(7))
          )
        )
    }

    "find a flow for the second property through the destructuring declaration" in {
      val source = cpg.method.name("printTechnique").parameter.nameExact("p2")
      val sink   = cpg.method.name("println").callIn.argument.codeExact("id")
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("printTechnique(p1, p2)", Some(5)),
            ("<init>", Some(6)),
            ("<init>(this, name, topChunkSize)", Some(4)),
            ("this.topChunkSize = topChunkSize", Some(4)),
            ("this.topChunkSize = topChunkSize", None),
            ("RET", None),
            ("<init>", Some(6)),
            ("tmp_1.component1()", Some(6)),
            ("component1(this)", None),
            ("RET", None),
            ("tmp_1.component1()", Some(6)),
            ("tmp_1.component2()", Some(6)),
            ("component2(this)", None),
            ("RET", None),
            ("RET", Some(4)),
            ("RET", None),
            ("tmp_1.component2()", Some(6)),
            ("id = tmp_1.component2()", Some(6)),
            ("println(id)", Some(8))
          )
        )
    }
  }
}
