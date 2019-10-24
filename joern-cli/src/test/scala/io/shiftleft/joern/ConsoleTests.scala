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

}
