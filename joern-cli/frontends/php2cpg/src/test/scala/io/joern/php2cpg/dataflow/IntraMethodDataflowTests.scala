package io.joern.php2cpg.dataflow

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class IntraMethodDataflowTests extends PhpCode2CpgFixture(runOssDataflow = true) {
  "flows from parameters to corresponding identifiers should be found" in {
    val cpg = code("""<?php
        |function runShell($cmd) {
        |  system($cmd);
        |}
        |""".stripMargin)

    cpg.identifier.name("cmd").reachableBy(cpg.parameter.name("cmd")).size shouldBe 1
  }

  "flows between function calls should be found" in {
    val cpg = code("""<?php
        |function Foo() {
        |  $my_input = input();
        |  sink($my_input);
        |}
        |""".stripMargin)

    val source = cpg.call("input")
    val sink   = cpg.call("sink")
    val flows  = sink.reachableByFlows(source)

    flows.size shouldBe 1
  }
}
