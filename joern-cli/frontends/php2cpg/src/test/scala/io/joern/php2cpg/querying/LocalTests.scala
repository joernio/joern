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

      cpg.method.name("foo").ast.isIdentifier.name("x")._localViaRefOut.map(_.name).l shouldBe List("x", "x")
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
    inside(cpg.local.l) { case List(xLocal) =>
      xLocal.name shouldBe "x"
      xLocal.code shouldBe "static $x"
      xLocal.lineNumber shouldBe Some(4)
    }
  }

  "function-local variable scoping" should {
    "not resolve a variable to a top-level local without the global keyword" in {
      val cpg = code(
        """<?php
          |$aaa = 1;
          |function foo() {
          |  return $aaa;
          |}
          |""".stripMargin,
        fileName = "main.php"
      )

      val fooLocal = cpg.method.name("foo").local.nameExact("aaa").loneElement
      fooLocal.code shouldBe "$aaa"
      cpg.method.name("foo").ast.isIdentifier.nameExact("aaa")._localViaRefOut.l shouldBe List(fooLocal)
    }

    "not resolve a variable to a parameter of a sibling method" in {
      val cpg = code("""<?php
          |class ClassLoader {
          |  public function setPsr4($prefix) {
          |    return $prefix;
          |  }
          |  public function findFile() {
          |    return $prefix;
          |  }
          |}
          |""".stripMargin)

      cpg.method.name("setPsr4").parameter.nameExact("prefix").size shouldBe 1
      val findFileLocal = cpg.method.name("findFile").local.nameExact("prefix").loneElement
      findFileLocal.code shouldBe "$prefix"
    }

    "not capture an outer variable into a plain closure without a use clause" in {
      val cpg = code(
        """<?php
          |$aaa = 1;
          |$fn = function () {
          |  return $aaa;
          |};
          |""".stripMargin,
        fileName = "server.php"
      )

      val closure      = cpg.method.name(".*<lambda>0").loneElement
      val closureLocal = closure.local.nameExact("aaa").loneElement
      closureLocal.closureBindingId shouldBe None
    }

    "keep the tmp local for a parameter attribute array literal inside the method" in {
      val cpg = code(
        """<?php
          |function f(
          |  #[CompletionProvider(values: ['welcome', 'faq'])]
          |  string $noteId
          |): string {
          |  return $noteId;
          |}
          |""".stripMargin,
        fileName = "server.php"
      )

      cpg.local.method.name.toSet shouldBe Set("f")
    }
  }
}
