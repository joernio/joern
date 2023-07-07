package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import overflowdb.traversal.toNodeTraversal
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic

class ControlStructureTests extends RubyCode2CpgFixture {

  "while-loop use cases 1" should {
    val cpg = code("""
        |def method()
        |  counter = 0
        |  while counter < 5 do
        |    puts "Counter: #{counter}"
        |    counter += 1
        |  end
        |end
        |""".stripMargin)

    // TODO: Need to be fixed.
    "be correct for while loop" ignore {
      cpg.method.name("method").block.astChildren.isControlStructure.l.size shouldBe 1
      inside(cpg.method.name("method").block.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.controlStructureType shouldBe ControlStructureTypes.WHILE
          inside(controlStruct.condition.l) { case List(cndNode) =>
            cndNode.code shouldBe "counter < 5"
          }
          controlStruct.whenTrue.astChildren.isCall.l.size should be > 2
      }
    }
    // TODO: Need to be fixed. I think above unit test is cause of this data flow to not work
    "data flow test" ignore {
      val src  = cpg.identifier("counter").lineNumber(3).l
      val sink = cpg.call("puts").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }
  "while-loop use cases 2" should {
    val cpg = code("""
        |def method()
        |  counter = 0
        |  begin
        |    puts "Counter: #{counter}"
        |    counter += 1
        |  end while counter < 5
        |end
        |""".stripMargin)

    // TODO: Need to be fixed
    "data flow test" ignore {
      val src  = cpg.identifier("counter").lineNumber(3).l
      val sink = cpg.call("puts").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "until-loop use cases 1" should {
    val cpg = code("""
        |def method()
        |  counter = 0
        |  until counter >= 5 do
        |    puts "Counter: #{counter}"
        |    counter += 1
        |  end
        |end
        |""".stripMargin)

    // TODO: Need to be fixed
    "data flow test" ignore {
      val src  = cpg.identifier("counter").lineNumber(3).l
      val sink = cpg.call("puts").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "until-loop use cases 2" should {
    val cpg = code("""
        |def method()
        |  counter = 0
        |  begin
        |    puts "Counter: #{counter}"
        |    counter += 1
        |  end until counter < 5
        |end
        |""".stripMargin)

    // TODO: Need to be fixed
    "data flow test" ignore {
      val src  = cpg.identifier("counter").lineNumber(3).l
      val sink = cpg.call("puts").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "if statement use case 1" should {
    val cpg = code("""
        |def method()
        |  counter = 5
        |  if counter > 1
        |    puts "Counter: #{counter}"
        |    sink(counter)
        |  end
        |end
        |""".stripMargin)

    // TODO: Need to be fixed
    "data flow test" ignore {
      val src  = cpg.identifier("counter").lineNumber(3).l
      val sink = cpg.call("puts").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    // TODO: Need to be fixed
    "data flow test 2" ignore {
      val src  = cpg.identifier("counter").lineNumber(3).l
      val sink = cpg.call("sink").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }
}
