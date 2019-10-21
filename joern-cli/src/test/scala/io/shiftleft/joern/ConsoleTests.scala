package io.shiftleft.joern

import java.io.File

import io.shiftleft.console.ScriptManager
import io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

class ConsoleTests extends WordSpec with Matchers {

  // TODO: This test currently fails when run via SBT (see https://github.com/scala/bug/issues/10058)
  "should execute the list-funcs correctly for example code" ignore {
    val inputFilenames = Array("joern-cli/src/test/resources/testcode/free/")
    val tmpFile = File.createTempFile("cpg", ".bin.zip")
    val outputFilename = tmpFile.getPath
    tmpFile.delete()

    // Create a CPG using the C/C++ fuzzy parser
    val fuzzyc2Cpg = new FuzzyC2Cpg(outputFilename)
    fuzzyc2Cpg.runAndOutput(inputFilenames)
    // Link CPG fragments and enhance to create semantic CPG
    Cpg2Scpg.run(outputFilename, false, "")

    class TestConsole extends ScriptManager(new JoernScriptExecutor(outputFilename))

    val expected = CpgLoader.load(outputFilename).method.name.l
    val actual = new TestConsole().runScript("list-funcs")

    actual shouldBe expected
  }
}
