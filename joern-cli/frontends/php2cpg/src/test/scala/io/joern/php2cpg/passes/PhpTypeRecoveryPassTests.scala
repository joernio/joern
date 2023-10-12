package io.joern.php2cpg.passes

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class PhpTypeRecoveryPassTests extends PhpCode2CpgFixture() {

  "literals declared from built-in types" should {
    lazy val cpg = code("""
      |<?php
      |function foo_return_int() {
      |   $x = 1;
      |   return $x;
      |}
      |""".stripMargin).cpg

    "resolve 'x' identifier type" in {
      val List(xIdentifier) = cpg.identifier("x").take(1).l
      //xIdentifier.dynamicTypeHintFullName shouldBe Seq("int")
      xIdentifier.typeFullName shouldBe "int"
    }
    "resolve 'foo_return_int()' return value" in {
      val List(fooMethod) = cpg.method("foo_return_int").take(1).l
      fooMethod.methodReturn.dynamicTypeHintFullName shouldBe Seq("int")
    }
  }

  "literals declared from built-in types that are shadowed" should {
    lazy val cpg = code("""
        |<?php
        |$x = 123;
        |
        |function foo_shadowing() {
        |   $x = "foo";
        |}
        |""".stripMargin).cpg

    "resolve 'x' identifier types despite shadowing" in {
      val List(xOuterScope, xInnerScope) = cpg.identifier("x").take(2).l
      xOuterScope.dynamicTypeHintFullName shouldBe Seq("int")
      xInnerScope.dynamicTypeHintFullName shouldBe Seq("string")
    }
  }
}