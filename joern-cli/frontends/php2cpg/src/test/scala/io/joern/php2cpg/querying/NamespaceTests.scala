package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.Method

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
    val cpg = code("""<?php
        |namespace ns {
        |  function foo() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("ns\\foo")
  }

  "methods defined in sub-namespace should have the correct name" in {
    val cpg = code("""<?php
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
    val cpg = code("""<?php
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
    val cpg = code("""<?php
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
    val cpg = code("""<?php
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
    val cpg = code("""<?php
        |class A {
        |  function foo() {}
        |
        |  static function bar() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("A->foo")
    cpg.method.name("bar").fullName.l shouldBe List("A::bar")
  }

  "static and instance methods in namespaced code should be correct" in {
    val cpg = code("""<?php
        |namespace ns;
        |class A {
        |  function foo() {}
        |
        |  static function bar() {}
        |}
        |""".stripMargin)

    cpg.method.name("foo").fullName.l shouldBe List("ns\\A->foo")
    cpg.method.name("bar").fullName.l shouldBe List("ns\\A::bar")
  }

  "global namespace block should have the relative filename prepended to fullName" in {
    val cpg = code("<?php", fileName = "foo.php").moreCode("<?php", fileName = "bar.php")

    cpg.namespaceBlock.nameExact("<global>").fullName.sorted.l shouldBe List(
      // The <global> namespace added by the MetaDataPass
      "<global>",
      // The per-file <global> namespaces actually used
      "bar.php:<global>",
      "foo.php:<global>"
    )
  }

  "global variables should have AST in edges from the enclosing global method" in {
    val cpg = code("""<?php $a = 1;""", fileName = "foo.php")

    inside(cpg.local.l) { case List(aLocal) =>
      aLocal.name shouldBe "a"
      aLocal.code shouldBe "$a"
      aLocal.lineNumber shouldBe Some(1)

      inside(aLocal.method.l) { case List(globalMethod) =>
        globalMethod.name shouldBe "<global>"
        globalMethod.fullName shouldBe "foo.php:<global>"
      }
    }
  }
}
