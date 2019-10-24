package io.shiftleft.joern

import better.files.File
import better.files.Dsl._
import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

class ConsoleTests extends WordSpec with Matchers with AbstractJoernCliTest {

  "should execute the list-funcs correctly for example code" in withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/free"))) {
    case (cpg, outputFilename) =>
      val expected = cpg.method.name.l
      cp(File(outputFilename), File(".") / "cpg.bin.zip")
      val actual = console.Console.runScript("list-funcs")
      actual shouldBe expected
      rm(File(".") / "cpg.bin.zip")
  }

  "should execute the cfgToDot correctly for example code" in withTestCpg(
    File(getClass.getClassLoader.getResource("testcode/free"))) {
    case (_, outputFilename) =>
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
      cp(File(outputFilename), File(".") / "cpg.bin.zip")
      val actual = console.Console.runScript("cfgToDot")
      actual shouldBe expected
      rm(File(".") / "cpg.bin.zip")
  }

}
