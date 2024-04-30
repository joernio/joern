package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MethodTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {
  // Works in deprecated
  "Data flow through class method" ignore {
    val cpg = code("""
                     |class MyClass
                     |  def print(text)
                     |    puts text
                     |  end
                     |end
                     |
                     |
                     |x = "some text"
                     |inst = MyClass.new
                     |inst.print(x)
                     |""".stripMargin)

    val src  = cpg.identifier.name("x").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works in deprecated
  "Data flow through do-while loop" ignore {
    val cpg = code("""
                     |x = 0
                     |num = -1
                     |loop do
                     |   num = x + 1
                     |   x = x + 1
                     |   if x > 10
                     |     break
                     |   end
                     |end
                     |puts num
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }

  "Data flow through methodOnlyIdentifier usage" in {
    val cpg = code("""
                     |x = 1
                     |y = SomeConstant! + x
                     |puts y
                     |""".stripMargin)

    val source = cpg.identifier.name("x").l
    val sink   = cpg.call.name("puts").l
    sink.reachableByFlows(source).l.size shouldBe 2
  }
}
