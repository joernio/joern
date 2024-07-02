package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LocalTests extends PhpCode2CpgFixture {

  "locals for methods" should {
    "be created for methods with assigns" in {
      val cpg = code("""<?php
          |function foo() {
          |  $x = 0;
          |  $y = 1;
          |  $x = 2;
          |}""".stripMargin)

      inside(cpg.method.name("foo").local.sortBy(_.name).toList) { case List(xLocal, yLocal) =>
        xLocal.name shouldBe "x"
        xLocal.code shouldBe "$x"

        yLocal.name shouldBe "y"
        yLocal.code shouldBe "$y"
      }
    }

    "have ref edges from uses" in {
      val cpg = code("""<?php
          |function foo() {
          |  $x = bar();
          |  $x();
          |}
          |""".stripMargin)

      inside(cpg.method.name("foo").ast.isIdentifier.name("x").l) { case List(xIdent1, xIdent2) =>
        xIdent1._localViaRefOut.map(_.name) should contain("x")
        xIdent2._localViaRefOut.map(_.name) should contain("x")
      }
    }

    "not be created if the variable matches a parameter type" in {
      val cpg = code("""<?php
          |function foo($x) {
          |  $x = 12;
          |}
          |""".stripMargin)

      cpg.method.name("foo").local.isEmpty shouldBe true
    }

    "create a local when it is first used if not defined in an assign" in {
      val cpg = code("""<?php
          |function foo() {
          |  if ($x) {
          |    echo "Hello, world";
          |  }
          |}
          |""".stripMargin)

      inside(cpg.method.name("foo").local.l) { case List(xLocal) =>
        xLocal.name shouldBe "x"
        xLocal.code shouldBe "$x"
      }
    }
  }

  "nested static locals should be created correctly" in {
    val cpg = code("""<?php
          |function foo($y) {
          |  if ($y) {
          |    static $x  = 1;
          |  }
          |}
          |""".stripMargin)
    println(cpg.local.name.l)
    inside(cpg.local.l) { case List(xLocal) =>
      xLocal.name shouldBe "x"
      xLocal.code shouldBe "static $x"
      xLocal.lineNumber shouldBe Some(4)
    }
  }
}
