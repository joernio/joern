package io.joern.go2cpg.dataflow
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite

class ArrayDataflowTests extends GoCodeToCpgSuite {
  "Source to sink dataflow through arrays" should {
    // TODO: Investigate why this is not working.
    "be reachable (case 1)" in {
      val cpg = code("""
          |package main
          |func main() {
          | a=1
          |	b:=[5]int{a}
          |}
          |""".stripMargin)

      println(cpg.literal.code.l)
      val source = cpg.identifier("a")
      val sink   = cpg.identifier("b")
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

}
