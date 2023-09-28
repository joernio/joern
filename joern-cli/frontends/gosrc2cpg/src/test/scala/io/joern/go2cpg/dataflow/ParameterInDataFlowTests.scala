package io.joern.go2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class ParameterInDataFlowTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  "Simple parameter in to identifier data flow use case" should {
    val cpg = code("""
        |package main
        |func foo(argv string) {
        |  var a = argv
        |}
        |""".stripMargin)

    "data flow from parameterIn to identifier" in {
      val source = cpg.parameter("argv")
      val sink   = cpg.identifier("a")
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

  "Simple parameter in to call argument data flow use case" should {
    val cpg = code("""
        |package main
        |func foo(argv string) {
        |  sample("sample", argv)
        |  var a, _ = sample("sample", argv)
        |  b, _ := sample("sample", argv)
        |  c, d := sample("sample", argv)
        |}
        |""".stripMargin)

    "data flow from parameterIn to identifier" in {
      val source = cpg.parameter("argv").l
      val sink   = cpg.identifier("argv").l
      sink.reachableByFlows(source).size shouldBe 4
    }

    "data flow from parameterIn to CALL Node" in {
      val source = cpg.parameter("argv").l
      val sink   = cpg.call("sample").l
      sink.reachableByFlows(source).size shouldBe 4
    }

    "data flow from parameterIn to lhs varaible" in {
      val source = cpg.parameter("argv").l
      source.size shouldBe 1
      val sink = cpg.identifier("[a|b|c]").l
      sink.size shouldBe 3
      sink.reachableByFlows(source).size shouldBe 3
    }
  }
}
