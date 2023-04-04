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

  "methods defined in non-namespaced code should not include a namespace prefix" in {
    val cpg = code("""<?php
        |function foo() {}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("foo")
  }

  "methods defined in a namespace without sub-namespace should have the correct name" in {
    val cpg = code("""<?php
        |namespace ns;
        |function foo() {}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("ns\\foo")
  }

  "methods defined in a namespace with brace syntax without sub-namespace should have the correct name" in {
    val cpg = code(
      """<?php
        |namespace ns {
        |  function foo() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("ns\\foo")
  }

  "methods defined in sub-namespace should have the correct name" in {
    val cpg = code(
      """<?php
        |namespace ns {
        |  function foo() {}
        |}
        |
        |namespace ns\sub {
        |  function bar() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("ns\\foo")
    cpg.method.name("bar").fullName.l shouldBe List("ns\\sub\\bar")
  }

  "methods in different namespaces in the same file should have the correct names" in {
    val cpg = code(
      """<?php
        |namespace first;
        |function foo() {}
        |
        |namespace second;
        |function bar() {}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("first\\foo")
    cpg.method.name("bar").fullName.l shouldBe List("second\\bar")
  }

  "methods in different namespaces using brace syntax in the same file should have the correct names" in {
    val cpg = code(
      """<?php
        |namespace first {
        |  function foo() {}
        |}
        |
        |namespace second {
        |  function bar() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("first\\foo")
    cpg.method.name("bar").fullName.l shouldBe List("second\\bar")
  }

  "methods in different namespaces mixing global and named namespaces should have the correct names" in {
    val cpg = code(
      """<?php
        |namespace first {
        |  function foo() {}
        |}
        |
        |namespace ns\second {
        |  function bar() {}
        |}
        |
        |namespace {
        |  function baz() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("first\\foo")
    cpg.method.name("bar").fullName.l shouldBe List("ns\\second\\bar")
    cpg.method.name("baz").fullName.l shouldBe List("baz")
  }

  "static and instance methods in non-namespaced code should be correct" in {
    val cpg = code(
      """<?php
        |class A {
        |  function foo() {}
        |
        |  static function bar() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("A->foo")
    cpg.method.name("bar").fullName.l shouldBe List("A::bar")
  }
}
