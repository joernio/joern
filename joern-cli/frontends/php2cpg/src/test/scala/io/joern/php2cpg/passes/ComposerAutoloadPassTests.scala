package io.joern.php2cpg.passes

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.joern.x2cpg.frontendspecific.php2cpg.ComposerAutoloadPass
import io.shiftleft.semanticcpg.language.*

class ComposerAutoloadPassTests extends PhpCode2CpgFixture() {

  "ComposerAutoloadPass" should {
    val cpg = code(
      """|<?php
         |require 'vendor/autoload.php';
         |
         |class AutoloadedClass {
         |    public function run() {
         |        return 42;
         |    }
         |}
         |""".stripMargin,
      "bug.php"
    )

    "not crash on modules with top-level type declarations" in {
      // Top-level type declarations sit on the module method's CONTAINS edge but are not CfgNodes,
      // so findMethods must traverse the untyped `_containsOut` rather than the typed `containsOut`.
      val pass = new ComposerAutoloadPass(cpg)
      pass.generateParts().map(_.name).toList should contain("run")
    }
  }
}
