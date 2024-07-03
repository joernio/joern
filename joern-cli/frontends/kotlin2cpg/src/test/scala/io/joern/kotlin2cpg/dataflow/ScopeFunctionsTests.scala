package io.joern.kotlin2cpg.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ScopeFunctionsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with `let` scope function usage" should {
    val cpg = code("""fun f1(canaryId: String) = canaryId.let{ println(it) }""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(canaryId)", Some(1)),
            ("canaryId.let{ println(it) }", Some(1)),
            ("<lambda>0(it)", Some(1)),
            ("println(it)", Some(1))
          )
        )
    }
  }

  "CPG for code with `run` scope function usage" should {
    val cpg = code("""fun f1(canaryId: String) = canaryId.run{ println(this) }""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(canaryId)", Some(1)),
            ("canaryId.run{ println(this) }", Some(1)),
            ("<lambda>0(this)", Some(1)),
            ("println(this)", Some(1))
          )
        )
    }
  }

  "CPG for code with `also` scope function usage" should {
    val cpg = code("""fun f1(canaryId: String) = canaryId.also{ println(it) }""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(canaryId)", Some(1)),
            ("canaryId.also{ println(it) }", Some(1)),
            ("<lambda>0(it)", Some(1)),
            ("println(it)", Some(1))
          )
        )
    }
  }

  "CPG for code with `apply` scope function usage" should {
    val cpg = code("""fun f1(canaryId: String) = canaryId.apply{ println(this) }""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(canaryId)", Some(1)),
            ("canaryId.apply{ println(this) }", Some(1)),
            ("<lambda>0(this)", Some(1)),
            ("println(this)", Some(1))
          )
        )
    }
  }

  "CPG for code with `takeIf` scope function usage" should {
    val cpg =
      code("""fun f1(canaryId: String) = canaryId.takeIf{ println(it); it == "startYourEngines" }""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(canaryId)", Some(1)),
            ("canaryId.takeIf{ println(it); it == \"startYourEngines\" }", Some(1)),
            ("<lambda>0(it)", Some(1)),
            ("println(it)", Some(1))
          )
        )
    }
  }

  "CPG for code with `takeUnless` scope function usage" should {
    val cpg =
      code("""fun f1(canaryId: String) = canaryId.takeUnless{ println(it); it != "startYourEngines" }""".stripMargin)

    "should find a flow from method parameter to method return" in {
      val source = cpg.method.name("f1").parameter
      val sink   = cpg.call.methodFullName(".*println.*").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSet shouldBe
        Set(
          List(
            ("f1(canaryId)", Some(1)),
            ("canaryId.takeUnless{ println(it); it != \"startYourEngines\" }", Some(1)),
            ("<lambda>0(it)", Some(1)),
            ("println(it)", Some(1))
          )
        )
    }
  }
}
