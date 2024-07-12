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

  "flow from single layer array unpacking should be found" in {
    val cpg = code("""<?php
        |[$a1, $a2] = $arr1;
        |echo $a1;
        |""".stripMargin)
    val source = cpg.identifier("arr1")
    val sink   = cpg.call("echo").argument(1)
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 1
  }

  "flow from nested array unpacking should be found" in {
    val cpg = code("""<?php
        |[[$b1, $b2]] = $arr2;
        |echo $b1;
        |""".stripMargin)
    val source = cpg.identifier("arr2")
    val sink   = cpg.call("echo").argument(1)
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 1
  }

  "flow from nested list unpacking should be found" in {
    val cpg = code("""<?php
        |list(list($c1, $c2)) = $arr3;
        |echo $c1;
        |""".stripMargin)
    val source = cpg.identifier("arr3")
    val sink   = cpg.call("echo").argument(1)
    val flows  = sink.reachableByFlows(source)
    flows.size shouldBe 1
  }
}
