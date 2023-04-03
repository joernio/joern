package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class NamespaceTests extends PhpCode2CpgFixture {
  "namespaces should be able to contain statements as top-level AST children" in {
    val cpg = code("""<?php
        |namespace foo {
        |  echo 0;
        |}
        |""".stripMargin)

    inside(cpg.namespaceBlock.name("foo").l) { case List(ns) =>
      ns.astChildren.code.l shouldBe List("echo 0")
    }
  }
}
