package io.shiftleft.joern

import better.files.File
import better.files.Dsl._

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

import io.shiftleft.codepropertygraph.generated.nodes.Call

class RunScriptTests extends WordSpec with Matchers with AbstractJoernCliTest {

  private def withCpgZip[T](file: File)(f: Cpg => T): T = {
    val cpgZip = File(".") / "cpg.bin.zip"
    withTestCpg(file) {
      case (cpg, outputFilename) =>
        cp(File(outputFilename), cpgZip)
        try {
          f(cpg)
        } finally {
          rm(cpgZip)
        }
    }
  }

  "Executing scripts for example code 'testcode/unsafe-ptr" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/unsafe-ptr"))) { cpg: Cpg =>
    "work correctly for 'pointer-to-int.sc'" in {
      val calls =
        console.Console.runScript("c/pointer-to-int.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.code) should contain theSameElementsAs List(
        "simple_subtraction = p - q",
        "nested_subtraction = p - q - r",
        "literal_subtraction = p - i",
        "addrOf_subtraction = p - &i",
        "nested_addrOf_subtraction =  3 - &i - 4",
        "literal_addrOf_subtraction = 3 - &i",
        "array_subtraction = x - p",
        "array_literal_subtraction = x - 3",
        "array_addrOf_subtraction = x - &i"
        // TODO: We don't have access to type info for indirect field member access.
        // "unsafe_struct = foo_t->p - 1"
      )
    }
  }

  "Executing scripts for example code 'testcode/syscalls" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/syscalls"))) { cpg: Cpg =>
    "work correctly for 'syscalls.sc'" in {
      val calls =
        console.Console.runScript("c/syscalls.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.name) should contain theSameElementsAs List(
        "gettimeofday",
        "exit"
      )
    }
  }

  "Executing scripts for example code 'testcode/malloc-overflow" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/malloc-overflow"))) { cpg: Cpg =>
    "work correctly for 'malloc-overflow.sc'" in {
      val calls =
        console.Console.runScript("c/malloc-overflow.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.code) should contain theSameElementsAs List(
        "malloc(sizeof(int) * 42)",
        "malloc(sizeof(int) * 3)",
        "malloc(sizeof(int) + 55)"
      )
    }
  }

  "Executing scripts for example code 'testcode/free'" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/free"))) { cpg: Cpg =>
    "work correctly for 'list-funcs'" in {
      val expected = cpg.method.name.l
      val actual = console.Console.runScript("general/list-funcs.sc", Map.empty, cpg)
      actual shouldBe expected
    }

    "work correctly for 'cfgToDot'" in {
      val expected =
        """digraph g {
          | node[shape=plaintext];
          | "free.c: 11 p" -> "free.c: 11 free(p)";
          |  "free.c: 11 free(p)" -> "free.c: 9 p";
          |  "free.c: 10 next" -> "free.c: 10 p->next";
          |  "free.c: 10 p" -> "free.c: 10 next";
          |  "free.c: 10 p->next" -> "free.c: 10 q = p->next";
          |  "free.c: 10 q" -> "free.c: 10 p";
          |  "free.c: 10 q = p->next" -> "free.c: 11 p";
          |  "free.c: 9 q" -> "free.c: 9 p = q";
          |  "free.c: 9 p" -> "free.c: 9 q";
          |  "free.c: 9 p = q" -> "free.c: 9 p";
          |  "free.c: 9 NULL" -> "free.c: 9 p != NULL";
          |  "free.c: 9 p" -> "free.c: 9 NULL";
          |  "free.c: 9 p != NULL" -> "free.c: 10 q";
          |  "free.c: 9 p != NULL" -> "";
          |  "free.c: 9 head" -> "free.c: 9 *p = head";
          |  "free.c: 9 p" -> "free.c: 9 head";
          |  "free.c: 9 *p = head" -> "free.c: 9 p";
          |  "" -> "free.c: 9 p";
          | }""".stripMargin
      val actual = console.Console.runScript("graph/cfgToDot.sc", Map.empty, cpg)
      actual shouldBe expected
    }

    "work correctly for 'ast-for-funcs'" in {
      val actual = console.Console.runScript("graph/ast-for-funcs.sc", Map.empty, cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectFieldAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Ast""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Cfg"""
    }

    "work correctly for 'cfg-for-funcs'" in {
      val actual = console.Console.runScript("graph/cfg-for-funcs.sc", Map.empty, cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectFieldAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Cfg""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Ast"""
    }

    "work correctly for 'pdg'" in {
      val actual = console.Console.runScript("graph/pdg.sc", Map.empty, cpg).toString

      val expectedRegex = """List\(\(None,List\((\(\d+,\d+\),?\s?)+\),List\((\(\d+,[\w\W]+\),?\s?)+\)\)\)""".r

      actual should fullyMatch regex expectedRegex
    }

    "work correctly for 'graph-for-funcs'" in {
      val actual = console.Console.runScript("graph/graph-for-funcs.sc", Map.empty, cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectFieldAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Ast""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Cfg""")
      actual should include(""""AST"""")
      actual should include(""""CFG"""")
      actual should include(""""PDG"""")
    }

    "work correctly for 'functions-to-dot'" in {
      val List(actual) =
        console.Console.runScript("graph/functions-to-dot.sc", Map.empty, cpg).asInstanceOf[List[String]]

      val expectedRegex =
        """|digraph free_list \{
           | "\d+" -> "\d+" \[label="for \(struct node \*p = head; p != NULL; p = q\)"\];
           | "\d+" -> "\d+" \[label="\*p = head"\];
           | "\d+" -> "\d+" \[label="p != NULL"\];
           | "\d+" -> "\d+" \[label="p = q"\];
           | "\d+" -> "\d+" \[label=BLOCK\];
           | "\d+" -> "\d+" \[label="q = p->next"\];
           | "\d+" -> "\d+" \[label="free\(p\)"\];
           |\}
           |""".stripMargin

      actual should fullyMatch regex expectedRegex
    }
  }

}
