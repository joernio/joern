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
      val actual = console.Console.runScript("list-funcs", cpg)
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
      val actual = console.Console.runScript("cfgToDot", cpg)
      actual shouldBe expected
    }

    "work correctly for 'ast-for-funcs'" in {
      val actual = console.Console.runScript("ast-for-funcs", cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectMemberAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Ast""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Cfg"""
    }

    "work correctly for 'cfg-for-funcs'" in {
      val actual = console.Console.runScript("cfg-for-funcs", cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectMemberAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Cfg""")
      actual should not include """io.shiftleft.codepropertygraph.generated.edges.Ast"""
    }

    "work correctly for 'pdg-for-funcs'" ignore {
      val actual = console.Console.runScript("pdg-for-funcs", cpg).toString
      actual should include(""""function" : "free_list"""")
      actual should include(""""function" : "free"""")
      actual should include(""""function" : "<operator>.indirectMemberAccess"""")
      actual should include(""""function" : "<operator>.assignment"""")
      actual should include(""""function" : "<operator>.notEquals"""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Ast""")
      actual should include("""io.shiftleft.codepropertygraph.generated.edges.Cfg""")
    }

    "work correctly for 'graph-for-funcs'" in {
      val actual = console.Console.runScript("graph-for-funcs", cpg).toString
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
      val actual = console.Console.runScript("functions-to-dot", cpg).asInstanceOf[List[String]]
      val expected =
        """|digraph free_list {
           | node[shape=box];
           | "11" -> "16" [label="for (struct node *p = head; p != NULL; p = q)"];
           | "16" -> "18" [label="*p = head"];
           | "16" -> "22" [label="p != NULL"];
           | "16" -> "26" [label="p = q"];
           | "16" -> "30" [label=BLOCK];
           | "30" -> "31" [label="q = p->next"];
           | "30" -> "38" [label="free(p)"];
           |}
           |""".stripMargin

      actual shouldBe List(expected)
    }
  }

}
