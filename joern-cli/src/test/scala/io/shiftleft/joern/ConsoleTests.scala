package io.shiftleft.joern

import java.io.File

import io.shiftleft.console.ScriptManager
import io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.{Matchers, WordSpec}

class ConsoleTests extends WordSpec with Matchers {

  private class TestConsole(outputFilename: String) extends ScriptManager(new JoernScriptExecutor(outputFilename))

  "should execute the list-funcs correctly for example code" in {
    val inputFilenames = Array("joern-cli/src/test/resources/testcode/free/")
    val tmpFile = File.createTempFile("cpg", ".bin.zip")
    val outputFilename = tmpFile.getPath
    tmpFile.delete()

    // Create a CPG using the C/C++ fuzzy parser
    val fuzzyc2Cpg = new FuzzyC2Cpg(outputFilename)
    fuzzyc2Cpg.runAndOutput(inputFilenames)
    // Link CPG fragments and enhance to create semantic CPG
    Cpg2Scpg.run(outputFilename, dataFlow = false, "")

    val expected = CpgLoader.load(outputFilename).method.name.l
    val actual = new TestConsole(outputFilename).runScript("list-funcs")

    actual shouldBe expected
  }

}
