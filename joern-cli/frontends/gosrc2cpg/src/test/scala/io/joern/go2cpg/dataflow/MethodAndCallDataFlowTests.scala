package io.joern.go2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

import java.io.File

class MethodAndCallDataFlowTests extends GoCodeToCpgSuite(withOssDataflow = true) {
  "basic data flow test for across the function calls from same file" should {
    val cpg = code("""
         |package main
         |func foo() {
         |  var a = 10
         |	var b = bar(a)
         |  var c = bartwo(20, a)
         |  var d = bartwo(a, 30)
         |  var e = barthree(a,40)
         |  var f = barfour(a)
         |}
         |func bartwo(x int, y int) int {
         |  return y
         |}
         |func bar(x int) int {
         |  return x
         |}
         |
         |func barthree(x int, y int) int {
         |  if (x > 0) {
         |    return x
         |  } else {
         |    return y
         |  }
         |}
         |
         |func barfour(x int) int {
         |  return 0
         |}
         |""".stripMargin)
    "data flow from a to b should work" in {
      val src  = cpg.identifier("a").lineNumber(4).l
      val sink = cpg.identifier("b").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "data flow from a to c should work" in {
      val src  = cpg.identifier("a").lineNumber(4).l
      val sink = cpg.identifier("c").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "data flow from 30 to d should work" in {
      val src  = cpg.literal("30").l
      val sink = cpg.identifier("d").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "data flow from 40 and a to e should work" in {
      val src  = cpg.literal("40").l
      val sink = cpg.identifier("e").l
      sink.reachableByFlows(src).size shouldBe 1

      val srcone = cpg.identifier("a").lineNumber(4).l
      sink.reachableByFlows(srcone).size shouldBe 1
    }

    "no data flow from a to d as well as from 20 to c" in {
      val src  = cpg.identifier("a").lineNumber(4).l
      val sink = cpg.identifier("d").l
      sink.reachableByFlows(src).size shouldBe 0

      val srcone = cpg.literal("20").l
      sink.reachableByFlows(srcone).size shouldBe 0
    }

    "no data flow from a to f" in {
      val src  = cpg.identifier("a").lineNumber(4).l
      val sink = cpg.identifier("f").l
      sink.reachableByFlows(src).size shouldBe 0
    }
  }
  "Data flow test across the files function calls" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package fpkg
        |func bar(a int, b string) int{
        |  if (b == "") {
        |    return a
        |  } else {
        |    return 0
        |  }
        |}
        |""".stripMargin,
      Seq("fpkg", "mainlib.go").mkString(File.separator)
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  var c = fpkg.bar(10, "somestr")
        |}
        |""".stripMargin,
      "main.go"
    )

    "data flow from literal 10 to c should work" in {
      val src  = cpg.literal("10").l
      val sink = cpg.identifier("c").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "no data flow from literal 'somestr' to c" in {
      val src  = cpg.literal("\"somestr\"").l
      val sink = cpg.identifier("c").l
      sink.reachableByFlows(src).size shouldBe 0
    }
  }

  "data flow test through external method calls" should {
    val cpg = code("""
        |package main
        |import "joern.io/sample/fpkg"
        |func foo() {
        |  var c = fpkg.bar(10, "somestr")
        |  println(c)
        |}
        |""".stripMargin)

    "data flow from literal 10 and 'somestr' to c should work" in {
      val src  = cpg.literal("10").l
      val sink = cpg.identifier("c").lineNumber(5).l
      sink.reachableByFlows(src).size shouldBe 1

      val srcone = cpg.literal("\"somestr\"").l
      sink.reachableByFlows(srcone).size shouldBe 1
    }

    "data flow to call node as sink node should work" in {
      val src  = cpg.literal("10").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1

      val srcone = cpg.literal("\"somestr\"").l
      sink.reachableByFlows(srcone).size shouldBe 1
    }
  }

  "data flow test through struct type" should {
    val cpg = code("""
        |package main
        |type Person struct {
        |	fname string
        |	lname string
        |}
        |func (person Person) fullName() string {
        |	return person.fname + " " + person.lname
        |}
        |func main() {
        |	var a  = Person{fname: "Pandurang", lname: "Patil"}
        |	var fulname string = a.fullName()
        |   println(fulname)
        |}
        |""".stripMargin)
    "data flow from literal passed to constructor to identifier" in {
      val src  = cpg.literal("\"Pandurang\"").l
      val sink = cpg.identifier("a").l
      sink.reachableByFlows(src).size shouldBe 2

      val sinkOne = cpg.identifier("fulname").l
      sinkOne.reachableByFlows(src).size shouldBe 2
    }

    "data flow use case 2" in {
      val src  = cpg.identifier("a").l
      val sink = cpg.identifier("fulname").l
      sink.reachableByFlows(src).size shouldBe 4
    }

    "data flow use case 3" in {
      val src  = cpg.identifier("a").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 2
    }
  }

  "multiple assignment with tuple using var keyword initialization" should {
    val cpg = code("""
        |package main
        |
        |func add(int a, int b) (int, int) {
        |return (a+b)/2 , a+b
        |}
        |func foo() {
        |  var a = 10
        |  var b = 20
        |  var avg, sum = add(a, b)
        |}
        |""".stripMargin)

    "data flow to first tuple variable" in {
      val srcfirst = cpg.identifier("a").l
      val avgsink  = cpg.identifier("avg").l
      avgsink.reachableByFlows(srcfirst).size shouldBe 2

    }

    // tuple return handling
    "data flow to second tuple variable" ignore {
      val srcfirst = cpg.identifier("a").l
      val sumsink  = cpg.identifier("sum").l
      sumsink.reachableByFlows(srcfirst).size shouldBe 1
    }
  }
}
