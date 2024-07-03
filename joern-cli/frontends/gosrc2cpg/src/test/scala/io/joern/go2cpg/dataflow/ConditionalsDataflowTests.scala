package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite

class ConditionalsDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {
  "Source to sink dataflow through if blocks" should {

    "be reachable (case 1)" in {
      val cpg = code("""
          |package main
          |func main(){
          |   var a int = 4
          |   var b int
          |   if (a > 6) {
          |     c := a
          |     b = c
          |   }
          |}
          |
          |""".stripMargin)
      val source = cpg.identifier("a").lineNumber(4)
      val sink   = cpg.identifier("b").lineNumber(8)
      sink.reachableByFlows(source).size shouldBe 1

    }

    "be reachable (case 2)" in {
      val cpg = code("""
          |package main
          |func main(){
          |   var a int = 4
          |   var b int
          |   if (a > 6) {
          |     b = a
          |   }else {
          |      c := a
          |      b = c
          |   }
          |}
          |
          |""".stripMargin)
      val source = cpg.identifier("a").lineNumber(4)
      val sink   = cpg.identifier("b").lineNumber(10)
      sink.reachableByFlows(source).size shouldBe 1

    }

    "be reachable (case 3)" in {
      val cpg = code("""
          |package main
          |func main(){
          |   var a int = 4
          |   var b int
          |   if (a > 6) {
          |     b = a
          |   }else if (a < 4) {
          |      c := a
          |      b = c
          |   }else {
          |      b = a
          |   }
          |}
          |
          |""".stripMargin)
      val source = cpg.identifier("a").lineNumber(4)
      val sink   = cpg.identifier("b").lineNumber(10)
      sink.reachableByFlows(source).size shouldBe 1

    }

    "be reachable (case 4)" in {
      val cpg = code("""
          |package main
          |func main(){
          |   var a int = 4
          |   var b int
          |   if (a > 6) {
          |     if (a < 10) {
          |       c := a
          |       b = c
          |     }
          |   }
          |}
          |
          |""".stripMargin)
      val source = cpg.identifier("a").lineNumber(4)
      val sink   = cpg.identifier("b").lineNumber(9)
      sink.reachableByFlows(source).size shouldBe 1
    }
  }
}
