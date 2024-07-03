package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ControlExpressionsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with `if`-expressions without blocks in their branches" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  val r = 41
      |  val tick1 = if (r > 0) p else 0
      |  val tick2 = if (r > 0) 0 else tick1
      |  println(tick2)
      |}
      |""".stripMargin)

    "find a flow through the expressions" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("if (r > 0) p else 0", Some(4)),
            ("val tick1 = if (r > 0) p else 0", Some(4)),
            ("if (r > 0) 0 else tick1", Some(5)),
            ("val tick2 = if (r > 0) 0 else tick1", Some(5)),
            ("println(tick2)", Some(6))
          )
        )
    }
  }

  "CPG for code with `if`-expressions with blocks in their branches" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  val r = 41
      |  val tick1 = if (r > 0) { p } else { 0 }
      |  val tick2 = if (r > 0) { 0 } else { tick1 }
      |  println(tick2)
      |}
      |""".stripMargin)

    "find a flow through the expressions" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("if (r > 0) { p } else { 0 }", Some(4)),
            ("val tick1 = if (r > 0) { p } else { 0 }", Some(4)),
            ("if (r > 0) { 0 } else { tick1 }", Some(5)),
            ("val tick2 = if (r > 0) { 0 } else { tick1 }", Some(5)),
            ("println(tick2)", Some(6))
          )
        )
    }
  }

  "CPG for code with `try`-expressions with blocks in their branches" should {
    val cpg = code("""
      |fun f1(p: Int) {
      |  val r = 41
      |  val tick1 = try { p / 1 } catch(e: Exception) { 0 }
      |  val tick2 = try { 41 / 0 } catch(e: Exception) { tick1 }
      |  println(tick2)
      |}
      |""".stripMargin)

    "find a flow through the expressions" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.method.name("println").callIn.argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(p)", Some(2)),
            ("p / 1", Some(4)),
            ("try { p / 1 } catch(e: Exception) { 0 }", Some(4)),
            ("val tick1 = try { p / 1 } catch(e: Exception) { 0 }", Some(4)),
            ("tick1", Some(5)),
            ("try { 41 / 0 } catch(e: Exception) { tick1 }", Some(5)),
            ("val tick2 = try { 41 / 0 } catch(e: Exception) { tick1 }", Some(5)),
            ("println(tick2)", Some(6))
          )
        )
    }
  }
}
