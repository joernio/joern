package io.shiftleft.joern

import better.files.File
import better.files.Dsl._

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}

class RunScriptTests extends WordSpec with Matchers with AbstractJoernCliTest {

  private def withCpgZip[T](file: File)(f: Cpg => T): T = {
    val cpgZip = File(".") / "cpg.bin"
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
        console.JoernConsole.runScriptTest("c/pointer-to-int.sc", Map.empty, cpg).asInstanceOf[List[Call]]

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
        console.JoernConsole.runScriptTest("c/syscalls.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.name) should contain theSameElementsAs List(
        "gettimeofday",
        "exit"
      )
    }

    "work correctly for 'userspace-memory-access.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/userspace-memory-access.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.name) should contain theSameElementsAs List(
        "get_user"
      )
    }
  }

  "Executing scripts for example code 'testcode/malloc-overflow" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/malloc-overflow"))) { cpg: Cpg =>
    "work correctly for 'malloc-overflow.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/malloc-overflow.sc", Map.empty, cpg).asInstanceOf[List[Call]]

      calls.map(_.code) should contain theSameElementsAs List(
        "malloc(sizeof(int) * 42)",
        "malloc(sizeof(int) * 3)",
        "malloc(sizeof(int) + 55)"
      )
    }
  }

  "Executing scripts for example code 'testcode/leak" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/leak"))) { cpg: Cpg =>
    "work correctly for 'malloc-leak.sc'" in {
      val calls =
        console.JoernConsole.runScriptTest("c/malloc-leak.sc", Map.empty, cpg).asInstanceOf[Set[String]]

      calls should contain theSameElementsAs Set("leak")
    }
  }

  "Executing scripts for example code 'testcode/const-funcs" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/const-funcs"))) { cpg: Cpg =>
    "work correctly for 'const-funcs.sc'" in {
      val methods =
        console.JoernConsole.runScriptTest("c/const-funcs.sc", Map.empty, cpg).asInstanceOf[Set[Method]]

      // "side_effect_number" is included here as we are simply trying to emulate a side effect.
      methods.map(_.name) should contain theSameElementsAs
        Set("eligible", "eligible_params", "side_effect_number")
    }
  }

  "Executing scripts for example code 'testcode/const-ish" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/const-ish"))) { cpg: Cpg =>
    "work correctly for 'const-ish.sc'" in {
      val methods =
        console.JoernConsole.runScriptTest("c/const-ish.sc", Map.empty, cpg).asInstanceOf[Set[Method]]

      // "side_effect_number" is included here as we are simply trying to emulate a side effect.
      methods.map(_.name) should contain theSameElementsAs
        Set(
          "modify_const_struct_member_cpp_cast",
          "modify_const_struct_member_c_cast",
          "modify_const_struct_cpp_cast",
          "modify_const_struct_c_cast"
        )
    }
  }

  "Executing scripts for example code 'testcode/free'" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/free"))) { cpg: Cpg =>
    "work correctly for 'list-funcs'" in {
      val expected = cpg.method.name.l
      val actual = console.JoernConsole.runScriptTest("general/list-funcs.sc", Map.empty, cpg)
      actual shouldBe expected
    }

    "work correctly for 'cfgToDot'" ignore {
      // TODO: This test is flaky for some unexplained reason. The last dot statement changes position when running.
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
      val actual = console.JoernConsole.runScriptTest("graph/cfgToDot.sc", Map.empty, cpg).asInstanceOf[String]
      actual shouldBe expected
    }

    "work correctly for 'ast-for-funcs'" in {
      val actual = console.JoernConsole.runScriptTest("graph/ast-for-funcs.sc", Map.empty, cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectFieldAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Ast""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Cfg"""
    }

    "work correctly for 'cfg-for-funcs'" in {
      val actual = console.JoernConsole.runScriptTest("graph/cfg-for-funcs.sc", Map.empty, cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectFieldAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Cfg""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Ast"""
    }

    "work correctly for 'pdg'" in {
      val actual = console.JoernConsole.runScriptTest("graph/pdg.sc", Map.empty, cpg).toString

      val expectedRegex = """List\(\(None,List\((\(\d+,\d+\),?\s?)+\),List\((\(\d+,[\w\W]+\),?\s?)+\)\)\)""".r

      actual should fullyMatch regex expectedRegex
    }

    "work correctly for 'graph-for-funcs'" in {
      val actual = console.JoernConsole.runScriptTest("graph/graph-for-funcs.sc", Map.empty, cpg).toString
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
        console.JoernConsole.runScriptTest("graph/functions-to-dot.sc", Map.empty, cpg).asInstanceOf[List[String]]

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
