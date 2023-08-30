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
}
