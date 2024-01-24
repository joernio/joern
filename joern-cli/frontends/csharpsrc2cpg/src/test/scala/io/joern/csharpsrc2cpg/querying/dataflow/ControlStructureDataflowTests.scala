package io.joern.csharpsrc2cpg.querying.dataflow

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class ControlStructureDataflowTests extends CSharpCode2CpgFixture(withDataFlow = true) {

  "if data-flows" should {

    val cpg = code(basicBoilerplate("""
        |var b = true;
        |var s = "MALICIOUS";
        |if (b) {
        |  s = "SAFE";
        |}
        |Console.WriteLine(s);
        |""".stripMargin))

    "find a path if `MALICIOUS` is reassigned to `SAFE` in only one path of an if" in {
      val src  = cpg.literal.codeExact("\"MALICIOUS\"").l
      val sink = cpg.call.nameExact("WriteLine").l
      sink.reachableBy(src).size shouldBe 1
    }

  }

}
