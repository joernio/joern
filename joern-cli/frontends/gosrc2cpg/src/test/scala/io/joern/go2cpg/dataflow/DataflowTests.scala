package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._

class DataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  "Source to sink dataflow through operators" should {

    "be reachable (case 1)" in {
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

    "be reachable (case 2)" in {
      val cpg = code("""
          |package main
          |func main(){
          |   var a int = 4
          |   var b int = 5
          |   c := b + (a + 3)
          |   var d int = c
          |}
          |
          |""".stripMargin)
      val source = cpg.identifier("a")
      val sink   = cpg.identifier("d")
      sink.reachableByFlows(source).size shouldBe 2

    }
  }

  "Source to sink dataflow through if loop" should {

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
