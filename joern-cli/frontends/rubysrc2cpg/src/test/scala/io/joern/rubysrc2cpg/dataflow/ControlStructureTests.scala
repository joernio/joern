package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  "flow through body of a `while-end` statement" in {
    val cpg = code("""
        |x = 10
        |while x > 0 do
        |  x = x - 1
        |end
        |puts x
        |""".stripMargin)
    val source = cpg.literal("10")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 10", 2), ("x", 2), ("x - 1", 4), ("x = x - 1", 4), ("x > 0", 3), ("puts x", 6)))
  }

  "flow through body of a `... while ...` statement" in {
    val cpg = code("""
        |x = 0
        |x = x + 1 while x < 10
        |puts x
        |""".stripMargin)
    val source = cpg.literal("0")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 0", 2), ("x", 2), ("x + 1", 3), ("x = x + 1", 3), ("x < 10", 3), ("puts x", 4)))
  }

  "flow through body of an `until-end` statement" in {
    val cpg = code("""
        |x = 10
        |until x <= 0 do
        |  x = x - 1
        |end
        |puts x
        |""".stripMargin)
    val source = cpg.literal("10")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 10", 2), ("x", 2), ("x - 1", 4), ("x = x - 1", 4), ("x <= 0", 3), ("puts x", 6)))
  }

  "flow through the 1st branch of an `if-end` statement" in {
    val cpg = code("""
        |t = 100
        |if true
        | t = t + 1
        |end
        |puts t
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("t = 100", 2), ("t", 2), ("t + 1", 4), ("t = t + 1", 4), ("puts t", 6)))
  }

  "flow through the 2nd branch of an `if-else-end` statement" in {
    val cpg = code("""
        |t = 100
        |if false
        | foo
        |else
        | t = t - 1
        |end
        |puts t
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("t = 100", 2), ("t", 2), ("t - 1", 6), ("t = t - 1", 6), ("puts t", 8)))
  }

  "flow through the 2nd branch of an `if-elsif-end` statement" in {
    val cpg = code("""
        |t = 100
        |if false
        | foo
        |elsif true
        | t = t * 2
        |end
        |puts t
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("t = 100", 2), ("t", 2), ("t * 2", 6), ("t = t * 2", 6), ("puts t", 8)))
  }

  "flow through both branches of an `if-else-end` statement" in {
    val cpg = code("""
        |t = 100
        |if false
        | puts t + 1
        |else
        | puts t + 2
        |end
        |""".stripMargin)
    val source = cpg.literal("100")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("t = 100", 2), ("t", 2), ("t + 2", 6)), List(("t = 100", 2), ("t", 2), ("t + 1", 4)))
  }

  "flow through an `unless-end` statement" in {
    val cpg = code("""
        |x = 1
        |unless __LINE__ == 0 then
        |  x = x * 2
        |end
        |puts x
        |""".stripMargin)
    val source = cpg.literal("1")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet shouldBe
      Set(List(("x = 1", 2), ("x", 2), ("x * 2", 4), ("x = x * 2", 4), ("puts x", 6)))
  }

  "flow through a `begin-rescue-end` expression" ignore {
    val cpg = code("""
        |x = 1
        |y = begin
        | x
        |rescue
        | x
        |end
        |puts y
        |""".stripMargin)
    val source = cpg.literal("1")
    val sink   = cpg.method.name("puts").callIn.argument
    val flows  = sink.reachableByFlows(source)
    flows.map(flowToResultPairs).toSet should not be empty
  }

}
