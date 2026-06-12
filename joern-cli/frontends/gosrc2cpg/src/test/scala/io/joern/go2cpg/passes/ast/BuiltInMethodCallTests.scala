package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes

class BuiltInMethodCallTests extends GoCodeToCpgSuite {

  "When inbuilt method(make) is used to create map" should {
    val cpg = code("""package main
        |
        |func main() {
        |	myMap := make(map[string]int)
        |}""".stripMargin)
    "check identifier properties" in {
      val List(x) = cpg.identifier("myMap").l
      x.lineNumber.get shouldBe 4
      x.name shouldBe "myMap"
    }

    "check identifier(LHS) properties type" ignore {
      val List(x) = cpg.identifier("myMap").l
      x.typeFullName shouldBe "map"
    }

    "check call node properties" in {
      val List(x) = cpg.call("make").l
      x.name shouldBe "make"
      x.typeFullName shouldBe "map|chan"
    }

    "check call node arguments" in {
      val List(x) = cpg.call("make").argument.isLiteral.l
      x.code shouldBe "map[string]int"
      x.argumentIndex shouldBe 1
      x.typeFullName shouldBe "map"
    }

  }

  "When inbuilt method(make) is used to create channel" should {
    val cpg = code("""package main
        |
        |func main() {
        |	ch := make(chan int)
        |}""".stripMargin)
    "check identifier properties" in {
      val List(x) = cpg.identifier("ch").l
      x.lineNumber.get shouldBe 4
      x.name shouldBe "ch"
    }

    "check call node properties" in {
      val List(x) = cpg.call("make").l
      x.name shouldBe "make"
      x.typeFullName shouldBe "map|chan"
    }

    "check call node arguments" in {
      val List(x) = cpg.call("make").argument.isLiteral.l
      x.code shouldBe "chan int"
      x.argumentIndex shouldBe 1
      x.typeFullName shouldBe "chan"
    }

  }

  "Check assignment operation when channel is used" should {
    val cpg = code("""package main
        |
        |import "fmt"
        |
        |func main() {
        |    ch := make(chan int)
        |
        |    go func() {
        |        ch <- 42 // Send a value to the channel
        |    }()
        |
        |    // Receive the value from the channel
        |    value := <-ch
        |    fmt.Println(value)
        |}""".stripMargin)
    "check identifier properties" in {
      cpg.identifier("ch").lineNumber.l.sorted.shouldBe(List(6, 9, 13))
    }

  }

  "Go concurrency statements" should {
    val cpg = code("""package main
        |
        |import "fmt"
        |
        |func main() {
        |    ch := make(chan int)
        |    defer fmt.Println("done")
        |    go func() {
        |        ch <- 42
        |    }()
        |}""".stripMargin)

    "create explicit go, defer and channel send calls" in {
      val List(goCall)    = cpg.call.nameExact("<operator>.go").l
      val List(deferCall) = cpg.call.nameExact("<operator>.defer").l
      val List(sendCall)  = cpg.call.nameExact("<operator>.channelSend").l

      goCall.code should startWith("go func()")
      deferCall.code shouldBe """defer fmt.Println("done")"""
      sendCall.code shouldBe "ch <- 42"
      sendCall.argument.isIdentifier.nameExact("ch").size shouldBe 1
      sendCall.argument.isLiteral.codeExact("42").size shouldBe 1
    }

    "capture channel variables used inside goroutine literals" in {
      val List(lambda)     = cpg.method.isLambda.l
      val List(capturedCh) = lambda.local.nameExact("ch").l

      capturedCh.closureBindingId shouldBe Some(s"${lambda.fullName}:ch")
      lambda.ast.isIdentifier.nameExact("ch").refsTo.l shouldBe List(capturedCh)
    }
  }

  "Select statements" should {
    val cpg = code("""package main
        |
        |import "fmt"
        |
        |func main() {
        |    ch := make(chan int)
        |    select {
        |    case ch <- 1:
        |        fmt.Println("sent")
        |    default:
        |        fmt.Println("default")
        |    }
        |}""".stripMargin)

    "lower to a switch-like control structure with communication and body calls" in {
      val List(selectControl) = cpg.controlStructure.codeExact("select").l
      selectControl.controlStructureType shouldBe ControlStructureTypes.SWITCH
      cpg.call.nameExact("<operator>.channelSend").codeExact("ch <- 1").size shouldBe 1
      cpg.call.nameExact("Println").codeExact("""fmt.Println("sent")""").size shouldBe 1
      cpg.call.nameExact("Println").codeExact("""fmt.Println("default")""").size shouldBe 1
    }
  }

}
