package io.joern.go2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class ArrayDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {
  "Source to sink dataflow through arrays" should {
    "be reachable (case 1)" in {
      val cpg = code("""
          |package main
          |func main() {
          | a:=1
          |	b:=[5]int{a}
          |}
          |""".stripMargin)

      val source = cpg.identifier("a")
      val sink   = cpg.identifier("b")
      sink.reachableByFlows(source).size shouldBe 2
    }

    "be reachable (case 2)" in {
      val cpg = code("""
          |package main
          |func main() {
          | a:=1
          |	b:=[...]int{a}
          |}
          |""".stripMargin)

      val source = cpg.identifier("a")
      val sink   = cpg.identifier("b")
      sink.reachableByFlows(source).size shouldBe 2
    }

    "be reachable (case 3)" in {
      val cpg = code("""
          |package main
          |func main() {
          | a:="hello"
          |	b:=[...]string{a}
          |}
          |""".stripMargin)

      val source = cpg.identifier("a")
      val sink   = cpg.identifier("b")
      sink.reachableByFlows(source).size shouldBe 2
    }
  }
  "Data flows around multidimentional Arrays" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var myArray [][]string = [][]string{{"1", "2"}, {"3", "4"}}
        |   first := myArray[0][1]
        |   second := myArray[1][0]
        |   third := myArray
        |}
        |""".stripMargin)

    "data flow check one" ignore {
      val src  = cpg.identifier("myArray").l
      val sink = cpg.identifier("first")
      sink.reachableByFlows(src).size shouldBe 2
    }
    "data flow check two" ignore {
      val src  = cpg.literal("\"2\"").l
      val sink = cpg.identifier("first")
      sink.reachableByFlows(src).size shouldBe 1
    }

    "data flow check three" in {
      val src  = cpg.identifier("myArray").l
      val sink = cpg.identifier("third")
      sink.reachableByFlows(src).size shouldBe 2
    }
  }
}
