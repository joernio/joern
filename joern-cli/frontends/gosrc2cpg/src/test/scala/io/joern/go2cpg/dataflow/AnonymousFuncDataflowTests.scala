package io.joern.go2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class AnonymousFuncDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {
  "Simple Lambda expression dataflow" should {
    val cpg = code("""
        |package main
        |
        |import "fmt"
        |
        |func main() {
        |	// Define a lambda function and assign it to a variable
        |	add := func(a, b int) int {
        |       println(a)
        |		return a + b
        |	}
        |
        |	// Call the lambda function
        |	result := add(3, 5)
        |	fmt.Println("Result:", result) // Output: 8
        |}
        |""".stripMargin)
    "work dataflow within lambda from parameter to println sink" in {
      val source = cpg.identifier("a")
      val sink   = cpg.call("println")
      sink.reachableByFlows(source).size shouldBe 1
    }

    "work dataflow from literal parameter passed to lambda invocation to println sink" in {
      val source = cpg.literal("3")
      val sink   = cpg.call("println")
      sink.reachableByFlows(source).size shouldBe 1
    }

    "work dataflow from literal parameter passed to lambda invocation to outside println sink" in {
      val source = cpg.literal("3")
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

}
