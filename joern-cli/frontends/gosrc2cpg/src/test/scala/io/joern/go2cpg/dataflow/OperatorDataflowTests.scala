package io.joern.go2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class OperatorDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {
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

    "be reachable (case 3)" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := 10
          |	b := 5
          | c := 10
          |	// Arithmetic Operators
          |	c := a + b
          |	c := a - b
          |	c := a * b
          |	c := a / b
          |	c := a % b
          |
          |	// Comparison Operators
          |	c := a == b
          |	c := a != b
          |	c := a > b
          |	c := a < b
          |	c := a >= b
          |	c := a <= b
          |
          |	// Logical Operators
          |	c := a && b
          |	c := a || b
          |
          |	// Bitwise Operators
          |	c := a & b
          |	c := a | b
          |	c := a ^ b
          |	c := a << 2
          |	c := a >> 2
          |
          | var d int = a
          |}
          |
          |""".stripMargin)

      val source = cpg.identifier("a")
      val sink   = cpg.identifier("d")
      sink.reachableByFlows(source).size shouldBe 20
    }

    "be reachable using shorthand assignment operators" in {
      val cpg = code("""
          |package main
          |func main() {
          | a := 5
          | b := 10
          | a += b
          | a -= b
          | a *= b
          | a %= b
          | a /= b
          | a <<= b
          | a >>= b
          | a &= b
          | a ^= b
          | a |= b
          | var c int = a
          |}
          |""".stripMargin)

      val source = cpg.identifier("a")
      val sink   = cpg.identifier("c")
      sink.reachableByFlows(source).size shouldBe 12
    }

    "be reachable using unary operators" in {
      val cpg = code("""
          |package main
          |func main() {
          | var a int = 5
          | var b int = 10
          | b := !a
          | c := &b
          | d := +c
          | e := -d
          | c := *e
          |}
          |""".stripMargin)

      val source = cpg.identifier("a")
      val sink   = cpg.identifier("c")
      sink.reachableByFlows(source).size shouldBe 4
    }

  }

  "dataflows across type casting operation" ignore {
    val cpg = code("""
        |package main
        |func main() {
        |	var m interface{} = "a"
        |	str, ok := m.(string)
        |}
        |""".stripMargin)
    "data flow from identifier to type casted identifier" in {
      val source = cpg.identifier("m")
      val sink   = cpg.identifier("str")
      sink.reachableByFlows(source).size shouldBe 1
    }
    "data flow from literal to type casted identifier" in {
      val source = cpg.literal("a")
      val sink   = cpg.identifier("str")
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

  "Check dataflow when channel is used" should {
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

    "check dataflow from literal to Println" ignore {
      val source = cpg.literal("42")
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 1
    }

    "check dataflow from identifier to Println" in {
      val source = cpg.identifier("ch").lineNumber(6)
      val sink   = cpg.call("Println")
      sink.reachableByFlows(source).size shouldBe 1
    }

  }

}
