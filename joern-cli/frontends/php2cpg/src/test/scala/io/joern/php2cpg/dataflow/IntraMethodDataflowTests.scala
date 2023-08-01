package io.joern.php2cpg.dataflow

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._

class IntraMethodDataflowTests extends PhpCode2CpgFixture(runOssDataflow = true) {
  "flows from parameters to corresponding identifiers should be found" in {
    val cpg = code("""<?php
        |function runShell($cmd) {
        |  system($cmd);
        |}
        |""".stripMargin)

    cpg.identifier.name("cmd").reachableBy(cpg.parameter.name("cmd")).size shouldBe 1
  }
}
