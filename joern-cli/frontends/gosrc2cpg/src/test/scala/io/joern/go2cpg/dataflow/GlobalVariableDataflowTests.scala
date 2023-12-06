package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*
import java.io.File
class GlobalVariableDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  "Global variable declaration check" should {
    val cpg = code("""
        |package main
        |const (
        |	FooConst = "Test"
        |)
        |var (
        |	BarVar = 100
        |)
        |func main() {
        |  println(FooConst)
        |}
        |""".stripMargin)
    "get the data flow from global variable to println sink" in {
      val source = cpg.literal("\"Test\"")
      val sink   = cpg.call("println")
      sink.reachableByFlows(source).size shouldBe 1
    }
  }
}
