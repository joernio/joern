package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class OperatorTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with a chain of arithmetic operators" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  val tick1 = p + 1
      |  val tick2 = tick1 - 1
      |  val tick3 = tick2 * 1
      |  val tick4 = tick3 / 1
      |  println(tick4)
      |}
      |""".stripMargin)

    "find a flow through all operators" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("p + 1", Some(3)),
            ("val tick1 = p + 1", Some(3)),
            ("tick1 - 1", Some(4)),
            ("val tick2 = tick1 - 1", Some(4)),
            ("tick2 * 1", Some(5)),
            ("val tick3 = tick2 * 1", Some(5)),
            ("tick3 / 1", Some(6)),
            ("val tick4 = tick3 / 1", Some(6)),
            ("println(tick4)", Some(7))
          )
        )
    }
  }

  "CPG for code with a chain of null-safety operators" should {
    val cpg = code("""
      |fun f1(p: Int?) {
      |  val tick1 = p :? 1
      |  val tick2 = tick1 as? Int
      |  val tick3 = tick2!!
      |  println(tick3)
      |}
      |""".stripMargin)

    "find a flow through all operators" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("val tick1 = p", Some(3)),
            ("tick1 as? Int", Some(4)),
            ("val tick2 = tick1 as? Int", Some(4)),
            ("tick2!!", Some(5)),
            ("val tick3 = tick2!!", Some(5)),
            ("println(tick3)", Some(6))
          )
        )
    }
  }

  "CPG for code using string interpolation" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  val tick1 = "suffix_$p"
      |  println(tick1)
      |}
      |""".stripMargin)

    "find a flow through the operators" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("p", Some(3)),
            ("\"suffix_$p\"", Some(3)),
            ("val tick1 = \"suffix_$p\"", Some(3)),
            ("println(tick1)", Some(4))
          )
        )
    }
  }

  "CPG for code with reassignment operators" should {
    val cpg = code("""
     |fun f1(p: Int) {
     |  var tick1 = p
     |  tick1 += 1
     |  tick1 -= 1
     |  tick1 *= 1
     |  tick1 /= 1
     |  println(tick1)
     |}
     |""")

    "find a flow through the operators" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("var tick1 = p", Some(3)),
            ("tick1 += 1", Some(4)),
            ("tick1 -= 1", Some(5)),
            ("tick1 *= 1", Some(6)),
            ("tick1 /= 1", Some(7)),
            ("println(tick1)", Some(8))
          )
        )
    }
  }
}
