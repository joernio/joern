package io.joern.rubysrc2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CallTests extends RubyCode2CpgFixture(withPostProcessing = true, withDataFlow = true) {

  // Works on deprecated
  "Flow through call" ignore {
    val cpg = code("""
        |def print(content)
        |puts content
        |end
        |
        |def main
        |n = 1
        |print( n )
        |end
        |""".stripMargin)

    val src  = cpg.method.name("print").parameter.where(_.index(1)).argument.l
    val sink = cpg.method.name("puts").callIn.argument(1).l
    sink.reachableByFlows(src).size shouldBe 1
  }

  // Works on deprecated
  "Explicit return via call with initialization" ignore {
    val cpg = code("""
                     |def add(p)
                     |q = 5
                     |q = p
                     |return q
                     |end
                     |
                     |n = 1
                     |ret = add(n)
                     |puts ret
                     |""".stripMargin)

    val src  = cpg.identifier.name("n").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

  // Works on deprecated
  "Implicit return via call with initialization" ignore {
    val cpg = code("""
                     |def add(p)
                     |q = 5
                     |q = p
                     |q
                     |end
                     |
                     |n = 1
                     |ret = add(n)
                     |puts ret
                     |""".stripMargin)

    val src  = cpg.identifier.name("n").l
    val sink = cpg.call.name("puts").l
    sink.reachableByFlows(src).l.size shouldBe 2
  }

}
