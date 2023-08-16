package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._

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

}
