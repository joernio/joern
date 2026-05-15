package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ConcurrencyTests extends GoCodeToCpgSuite {

  "AST creation for defer statements" should {
    "create call node for deferred function call" in {
      val cpg = code("""
          |package main
          |func main() {
          |  f := openFile()
          |  defer f.Close()
          |}""".stripMargin)

      val closeCall = cpg.call.name("Close").head
      closeCall.code shouldBe "f.Close()"
      closeCall.lineNumber shouldBe Some(5)
    }

    "create call node for deferred function with arguments" in {
      val cpg = code("""
          |package main
          |import "fmt"
          |func main() {
          |  defer fmt.Println("done")
          |}""".stripMargin)

      val printlnCall = cpg.call.name("Println").head
      printlnCall.code shouldBe "fmt.Println(\"done\")"
    }
  }

  "AST creation for go statements" should {
    "create call node for goroutine function call" in {
      val cpg = code("""
          |package main
          |func handler(x int) {}
          |func main() {
          |  go handler(42)
          |}""".stripMargin)

      val handlerCalls = cpg.call.name("handler").l
      handlerCalls.size should be >= 1
      handlerCalls.exists(_.code == "handler(42)") shouldBe true
    }

    "create call node for goroutine with simple function" in {
      val cpg = code("""
          |package main
          |func worker(id int) {}
          |func main() {
          |  go worker(1)
          |  go worker(2)
          |}""".stripMargin)

      val workerCalls = cpg.call.name("worker").l
      workerCalls.size shouldBe 2
    }
  }

  "AST creation for send statements" should {
    "create operator call for channel send" in {
      val cpg = code("""
          |package main
          |func main() {
          |  ch := make(chan int)
          |  ch <- 42
          |}""".stripMargin)

      val sendCall = cpg.call.name("<operator>.send").head
      sendCall.code shouldBe "ch <- 42"
      sendCall.argument.size shouldBe 2
    }
  }

  "AST creation for select statements" should {
    "create control structure for select with cases" in {
      val cpg = code("""
          |package main
          |func main() {
          |  ch1 := make(chan int)
          |  ch2 := make(chan int)
          |  select {
          |  case msg := <-ch1:
          |    x := msg
          |  case ch2 <- 1:
          |    y := 2
          |  default:
          |    z := 3
          |  }
          |}""".stripMargin)

      val selectStmt = cpg.method.name("main").controlStructure.l
        .filter(_.code == "select")
      selectStmt.size shouldBe 1
      selectStmt.head.controlStructureType shouldBe ControlStructureTypes.SWITCH
    }
  }
}
