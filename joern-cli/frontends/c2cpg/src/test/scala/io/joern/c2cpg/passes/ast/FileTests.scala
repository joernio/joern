package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.Config
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FileTests extends C2CpgSuite {

  "File test for single file" should {

    val cpg = code("""
        | int foo() {}
        | int bar() {}
        | struct my_struct { int x; };
        |""".stripMargin)

    "contain the correct file nodes" in {
      val List(fileTest, fileUnknown) = cpg.file.nameNot("<includes>").l
      fileTest.name shouldBe "Test0.c"
      fileTest.order shouldBe 0
      fileUnknown.name shouldBe FileTraversal.UNKNOWN
      fileUnknown.order shouldBe 0
    }

    "contain exactly one placeholder file node with `name=\"<unknown>\"/order=0`" in {
      cpg.file(FileTraversal.UNKNOWN).order.l shouldBe List(0)
      cpg.file(FileTraversal.UNKNOWN).hash.l shouldBe List()
    }

    "allow traversing from file to its namespace blocks" in {
      cpg.file.nameNot(FileTraversal.UNKNOWN).namespaceBlock.name.toSetMutable shouldBe Set(
        NamespaceTraversal.globalNamespaceName
      )
    }

    "allow traversing from file to its methods via namespace block" in {
      cpg.file.nameNot(FileTraversal.UNKNOWN).method.name.toSetMutable shouldBe Set(
        NamespaceTraversal.globalNamespaceName,
        "foo",
        "bar"
      )
    }

    "allow traversing from file to its type declarations via namespace block" in {
      cpg.file
        .nameNot(FileTraversal.UNKNOWN)
        .typeDecl
        .nameNot(NamespaceTraversal.globalNamespaceName)
        .name
        .l
        .sorted shouldBe List("ANY", "int", "my_struct", "void")
    }

    "allow traversing to namespaces" in {
      val List(ns1, ns2, ns3) = cpg.file.namespaceBlock.l
      ns1.filename shouldBe "Test0.c"
      ns1.fullName shouldBe "Test0.c:<global>"
      ns2.filename shouldBe "<includes>"
      ns2.fullName shouldBe "<includes>:<global>"
      ns3.filename shouldBe "<unknown>"
      ns3.fullName shouldBe "<global>"
      cpg.file.namespace.name(NamespaceTraversal.globalNamespaceName).l.size shouldBe 3
    }
  }

  "File test for single file with a header include" should {

    val cpg = code(
      """
        |#include "fetch.h"
        |#include "cache.h"
        |const char *write_ref = NULL;
        |void pull_say(const char *fmt, const char *hex {
        |  if (get_verbosely) { fprintf(stderr, fmt, hex); }
        |}
        |""".stripMargin,
      "fetch.c"
    )

    "contain the correct file nodes" in {
      cpg.file.name.sorted.l shouldBe List("<includes>", "<unknown>", "fetch.c")
    }

  }

  "File test for single file with a header include that actually exists" should {

    val cpg = code(
      """
        |#include "fetch.h"
        |#include "cache.h"
        |const char *write_ref = NULL;
        |void pull_say(const char *fmt, const char *hex {
        |  if (get_verbosely) { fprintf(stderr, fmt, hex); }
        |}
        |""".stripMargin,
      "fetch.c"
    ).moreCode(
      """
        |extern const char *write_ref;
        |""".stripMargin,
      "fetch.h"
    )

    "contain the correct file nodes" in {
      cpg.file.name.sorted.l shouldBe List("<includes>", "<unknown>", "fetch.c", "fetch.h")
    }

  }

  "File test for multiple source files and preprocessed files" should {

    val cpg = code("int foo() {}", "main.c")
      .moreCode("int bar() {}", "main.cpp")
      .moreCode("int foo() {}", "main.i")
      .moreCode("int baz() {}", "main.h")
      .moreCode("int other() {}", "other.h")
      .moreCode("int other() {}", "other.i")
      .withConfig(Config(withPreprocessedFiles = true))

    "contain the correct file nodes" in {
      cpg.method.nameNot("<global>").internal.name.sorted.l shouldBe List("foo", "other")
      cpg.file.nameNot("<includes>", "<unknown>").name.sorted.l shouldBe List("main.i", "other.i")
    }

  }

  "File test for preprocessed files from C and CPP files" should {

    val cpg = code(
      """
        |# 1 "a.c" 1
        |int bar() {}
        |""".stripMargin,
      "a.i"
    ).moreCode(
      """
          |# 1 "b.cpp" 1
          |class B {};
          |""".stripMargin,
      "b.i"
    ).withConfig(Config(withPreprocessedFiles = true))

    "be parsed correctly" in {
      cpg.file.nameNot("<includes>", "<unknown>").name.sorted.l shouldBe List("a.i", "b.i")
      cpg.method.nameExact("bar").file.name.l shouldBe List("a.i")
      cpg.typeDecl.nameExact("B").file.name.l shouldBe List("b.i")
    }

  }

}
