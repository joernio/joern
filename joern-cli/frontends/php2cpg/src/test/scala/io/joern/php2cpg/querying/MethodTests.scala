package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, Local}
import io.shiftleft.semanticcpg.language._

class MethodTests extends PhpCode2CpgFixture {

  "static variables without default values should be represented as the correct local nodes" in {
    val cpg = code("""<?php
        |function foo() {
        |  static $x, $y;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(xLocal: Local, yLocal: Local) =>
      xLocal.name shouldBe "x"
      xLocal.code shouldBe "static $x"
      xLocal.lineNumber shouldBe Some(3)

      yLocal.name shouldBe "y"
      yLocal.code shouldBe "static $y"
      yLocal.lineNumber shouldBe Some(3)
    }
  }

  "static variables with default values should have the correct initialisers" in {
    val cpg = code("""<?php
        |function foo() {
        |  static $x = 42, $y;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").body.astChildren.l) { case List(xLocal: Local, yLocal: Local, xAssign: Call) =>
      xLocal.name shouldBe "x"
      xLocal.code shouldBe "static $x"
      xLocal.lineNumber shouldBe Some(3)

      yLocal.name shouldBe "y"
      yLocal.code shouldBe "static $y"
      yLocal.lineNumber shouldBe Some(3)

      xAssign.name shouldBe Operators.assignment
      xAssign.code shouldBe "static $x = 42"
      inside(xAssign.argument.l) { case List(xIdent: Identifier, literal: Literal) =>
        xIdent.name shouldBe "x"
        xIdent.code shouldBe "$x"
        xIdent.lineNumber shouldBe Some(3)

        literal.code shouldBe "42"
        literal.lineNumber shouldBe Some(3)
      }
    }
  }
}
