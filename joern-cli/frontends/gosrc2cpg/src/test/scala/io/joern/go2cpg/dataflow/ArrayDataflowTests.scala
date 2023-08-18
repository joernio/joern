package io.joern.go2cpg.dataflow
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite

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

}
