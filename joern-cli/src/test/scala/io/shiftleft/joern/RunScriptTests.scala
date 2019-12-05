package io.shiftleft.joern

import better.files.File
import better.files.Dsl._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

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

  "Executing scripts for example code 'testcode/free'" should withCpgZip(
    File(getClass.getClassLoader.getResource("testcode/free"))) { cpg: Cpg =>
    "work correctly for 'list-funcs'" in {
      val expected = cpg.method.name.l
      val actual = console.Console.runScript("general/list-funcs", cpg)
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
      val actual = console.Console.runScript("general/cfgToDot", cpg)
      actual shouldBe expected
    }

    "work correctly for 'ast-for-funcs'" in {
      val actual = console.Console.runScript("general/ast-for-funcs", cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectMemberAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Ast""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Cfg"""
    }

    "work correctly for 'cfg-for-funcs'" in {
      val actual = console.Console.runScript("general/cfg-for-funcs", cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectMemberAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Cfg""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Ast"""
    }

    "work correctly for 'graph-for-funcs'" in {
      val actual = console.Console.runScript("general/graph-for-funcs", cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectMemberAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Ast""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Cfg""")
      actual should include(""""AST"""")
      actual should include(""""CFG"""")
      actual should include(""""PDG"""")
    }

    "work correctly for 'functions-to-dot'" in {
      val List(actual) = console.Console.runScript("general/functions-to-dot", cpg).asInstanceOf[List[String]]

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

    "work correctly for 'pdg-for-funcs'" in {
      val result = console.Console.runScript("general/pdg-for-funcs", cpg).asInstanceOf[String]
      result should contain ("""
                            |[
                            |  {
                            |    "functionName" : "free_list",
                            |    "reachableNodes" : [
                            |      {
                            |        "id" : 18,
                            |        "code" : "*p = head",
                            |        "edgesIn" : {
                            |          "CONTAINS" : 1,
                            |          "REACHING_DEF" : 1,
                            |          "CFG" : 1,
                            |          "POST_DOMINATE" : 1,
                            |          "AST" : 1,
                            |          "DOMINATE" : 1
                            |        },
                            |        "edgesOut" : {
                            |          "CONTAINS" : 1,
                            |          "REACHING_DEF" : 1,
                            |          "CFG" : 1,
                            |          "POST_DOMINATE" : 1,
                            |          "AST" : 1,
                            |          "DOMINATE" : 1
                            |        },
                            |        "properties" : {
                            |          "DISPATCH_TYPE" : "STATIC_DISPATCH",
                            |          "METHOD_INST_FULL_NAME" : "<operator>.assignment",
                            |          "NAME" : "<operator>.assignment",
                            |          "CODE" : "*p = head",
                            |          "LINE_NUMBER" : "9",
                            |          "SIGNATURE" : "TODO assignment signature",
                            |          "TYPE_FULL_NAME" : "ANY",
                            |          "COLUMN_NUMBER" : "21",
                            |          "ARGUMENT_INDEX" : "1",
                            |          "ORDER" : "1"
                            |        }
                            |      },
                            |""".stripMargin)
    }
  }

}
