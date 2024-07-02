package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite

class SwitchDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {
  "Source to sink dataflow through switch case" should {
    "be reachable for expression condition" in {
      val cpg = code("""
          |package main
          |func method() {
          |  var marks int = 90
          |  var grade string = "B"
          |  switch marks {
          |      case 90: myGrade := grade
          |      case 50,60,70: grade = "C"
          |      default: grade = "D"
          |   }
          |}
    """.stripMargin)
      val source = cpg.identifier("grade").lineNumber(5)
      val sink   = cpg.identifier("myGrade").lineNumber(7)
      sink.reachableByFlows(source).size shouldBe 1

    }

    "be reachable for empty condition" ignore {
      // TODO (BUG)dataflow doesn't work for empty condition in switch case
      val cpg = code("""
          |package main
          |func method() {
          |  var marks int = 90
          |  var grade string = "B"
          |  switch {
          |      case grade == "A" :
          |         mymarks := grade
          |      case grade == "B":
          |         marks = 80
          |   }
          |}
      """.stripMargin)
      val source = cpg.identifier("grade").lineNumber(5).l
      val sink   = cpg.identifier("mymarks").lineNumber(8).l
      sink.reachableByFlows(source).size shouldBe 1
    }
  }
}
