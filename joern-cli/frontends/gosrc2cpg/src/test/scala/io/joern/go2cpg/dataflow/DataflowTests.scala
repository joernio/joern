package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._

class DataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  "Source to sink dataflow for through operators" should {

    "be reachable" in {
      val cpg = code("""
          |package main
          |func main(){
          |   var a int = 4
          |   var b int = 5
          |   c := a
          |   d := c
          |}
          |
          |""".stripMargin)
      val source = cpg.identifier("a")
      val sink   = cpg.identifier("d")
      sink.reachableByFlows(source).size shouldBe 2

    }

  }

}
