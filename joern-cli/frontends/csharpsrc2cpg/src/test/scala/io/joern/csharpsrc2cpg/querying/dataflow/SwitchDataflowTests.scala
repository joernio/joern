package io.joern.csharpsrc2cpg.querying.dataflow

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class SwitchDataflowTests extends CSharpCode2CpgFixture(withDataFlow = true) {
  "switch statement dataflows" ignore {
    val cpg = code(basicBoilerplate("""
        |int i = 5;
        |switch (i) {
        | case < 0:
        |   Console.Write(i);
        |   break;
        | case > 0:
        |   break;
        | default:
        |   break;
        |}
        |""".stripMargin))

    "find a path from i to Write" in {
      val src  = cpg.identifier.nameExact("i").lineNumber(9).l
      val sink = cpg.call.nameExact("Write").l
      sink.reachableBy(src).size shouldBe 1
    }
  }
}
