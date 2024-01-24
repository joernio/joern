package io.joern.csharpsrc2cpg.querying.dataflow

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class SimpleDataflowTests extends CSharpCode2CpgFixture(withDataFlow = true) {

  "a source with a simple re-assignment" should {

    val cpg = code(basicBoilerplate("""
        |int i = 0;
        |int a = i + 1;
        |Console.WriteLine(a);
        |""".stripMargin))

    "still propagate to the sink" in {
      val src  = cpg.assignment.target.isIdentifier.nameExact("i").l
      val sink = cpg.call.nameExact("WriteLine").l
      sink.reachableBy(src).size shouldBe 1
    }

  }

}
