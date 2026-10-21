package io.joern.php2cpg.passes

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FullNameUniquenessPassTests extends PhpCode2CpgFixture {

  "the FullNameUniquenessPass" should {
    "create unique type decl fullNames for a class declared in multiple files" in {
      val cpg = code(
        """<?php
          |class Log {
          |  public function __destruct() {}
          |}
          |""".stripMargin,
        fileName = "utils.php"
      ).moreCode(
        """<?php
          |class Log {
          |  public function __destruct() {}
          |}
          |""".stripMargin,
        fileName = "payload.php"
      )

      cpg.typeDecl.name("Log").fullName.sorted.l shouldBe List("Log", "Log<duplicate>0")
      cpg.typeDecl.nameExact("Log").map(td => (td.fullName, td.filename)).sorted.l shouldBe List(
        ("Log", "payload.php"),
        ("Log<duplicate>0", "utils.php")
      )
    }

    "create unique method fullNames for a class method declared in multiple files" in {
      val cpg = code(
        """<?php
          |class Log {
          |  public function __destruct() {}
          |}
          |""".stripMargin,
        fileName = "payload.php"
      ).moreCode(
        """<?php
          |class Log {
          |  public function __destruct() {}
          |}
          |""".stripMargin,
        fileName = "utils.php"
      )

      cpg.method.nameExact("__destruct").map(method => (method.fullName, method.filename)).sorted.l shouldBe List(
        ("Log.__destruct", "payload.php"),
        ("Log.__destruct<duplicate>0", "utils.php")
      )
    }

    "keep the binding of a renamed class method pointing to the correct method" in {
      val cpg = code(
        """<?php
          |class Log {
          |  public function write() {}
          |}
          |""".stripMargin,
        fileName = "a.php"
      ).moreCode(
        """<?php
          |class Log {
          |  public function write() {}
          |}
          |""".stripMargin,
        fileName = "b.php"
      )

      cpg.typeDecl.fullNameExact("Log<duplicate>0").bindsOut.refOut.fullName.l shouldBe List("Log.write<duplicate>0")
    }

    "create unique fullNames for a top-level function declared in multiple files and fix same-file calls" in {
      val cpg = code(
        """<?php
          |function generateToken() { return 1; }
          |generateToken();
          |""".stripMargin,
        fileName = "a.php"
      ).moreCode(
        """<?php
          |function generateToken() { return 2; }
          |generateToken();
          |""".stripMargin,
        fileName = "b.php"
      )

      cpg.method.nameExact("generateToken").map(method => (method.fullName, method.filename)).sorted.l shouldBe List(
        ("generateToken", "a.php"),
        ("generateToken<duplicate>0", "b.php")
      )

      val callInB = cpg.call.name("generateToken").where(_.file.name("b.php")).loneElement
      callInB.methodFullName shouldBe "generateToken<duplicate>0"

      val callInA = cpg.call.name("generateToken").where(_.file.name("a.php")).loneElement
      callInA.methodFullName shouldBe "generateToken"
    }

    "leave unique fullNames untouched" in {
      val cpg = code(
        """<?php
          |class Foo {
          |  public function bar() {}
          |}
          |""".stripMargin,
        fileName = "a.php"
      ).moreCode(
        """<?php
          |class Baz {
          |  public function qux() {}
          |}
          |""".stripMargin,
        fileName = "b.php"
      )

      cpg.method.fullName(".*<duplicate>.*") shouldBe empty
      cpg.typeDecl.fullName(".*<duplicate>.*") shouldBe empty
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("Foo")
      cpg.typeDecl.nameExact("Baz").fullName.l shouldBe List("Baz")
    }

    "not rename the global namespace method" in {
      val cpg = code(
        """<?php
          |echo 1;
          |""".stripMargin,
        fileName = "a.php"
      ).moreCode(
        """<?php
          |echo 2;
          |""".stripMargin,
        fileName = "b.php"
      )

      cpg.method.nameExact(NamespaceTraversal.globalNamespaceName).fullName.l.foreach { fullName =>
        fullName should not include "<duplicate>"
      }
    }
  }
}
