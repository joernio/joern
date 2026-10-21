package io.joern.go2cpg.dataflow

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite

class LoopsDataflowTests extends GoCodeToCpgSuite(withOssDataflow = true) {
  "Source and sink dataflow for for loop" should {
    "be reachable when initialization, condition and post step are present" in {
      val cpg = code("""
          |package main
          |func main() {
          |   var b int = 10
          |
          |   /* for loop execution */
          |   for a := 0; a < 10; a++ {
          |       b += a
          |   }
          |
          |   z := b
          |}
    """.stripMargin)
      val source1 = cpg.identifier("b").lineNumber(4).l
      val sink1   = cpg.identifier("z").l
      sink1.reachableByFlows(source1).size shouldBe 1

      val source2 = cpg.identifier("b").lineNumber(8).l
      val sink2   = cpg.identifier("z").l
      sink2.reachableByFlows(source2).size shouldBe 1
    }

    // TODO Looks like some issue due to `range` being a unaryExpr here
    "be reachable for range statement" ignore {
      val cpg = code("""package main
          |func main() {
          |   var message string = "Hello, Gophers!"
          |
          |   counter := 0
          |   for index, char := range message {
          |        counter += index
          |    }
          |    indexSum := counter
          |}
          |""".stripMargin)
      val source1 = cpg.identifier("counter").lineNumber(5).l
      val sink1   = cpg.identifier("z").l
      sink1.reachableByFlows(source1).size shouldBe 1

      val source2 = cpg.identifier("counter").lineNumber(7).l
      val sink2   = cpg.identifier("indexSum").l
      sink2.reachableByFlows(source2).size shouldBe 1

    }
  }

  "Code after a range loop" should {
    // The range loop used to be a FOR with no condition, so its CFG had no exit edge and everything after it was
    // unreachable. https://github.com/joernio/joern/issues/6323
    val cpg = code("""
        |package main
        |
        |var items []string
        |
        |func sink(s string) {}
        |func step()         {}
        |
        |func NoKey(p string) {
        |	for range items {
        |		step()
        |	}
        |	sink(p)
        |}
        |
        |func KeyOnly(p string) {
        |	for i := range items {
        |		_ = i
        |	}
        |	sink(p)
        |}
        |
        |func KeyAndValue(p string) {
        |	for _, s := range items {
        |		_ = s
        |	}
        |	sink(p)
        |}
        |
        |func IntRange(p string) {
        |	for range 3 {
        |		step()
        |	}
        |	sink(p)
        |}
        |
        |func Assign(p string) {
        |	var i int
        |	for i = range items {
        |	}
        |	_ = i
        |	sink(p)
        |}
        |""".stripMargin)

    "be reachable from a parameter" in {
      for (name <- List("NoKey", "KeyOnly", "KeyAndValue", "IntRange", "Assign")) {
        val source = cpg.method.nameExact(name).parameter.nameExact("p")
        val sink   = cpg.method.nameExact(name).call.nameExact("sink").argument(1)
        withClue(s"$name: ") { sink.reachableByFlows(source).size shouldBe 1 }
      }
    }
  }
}
