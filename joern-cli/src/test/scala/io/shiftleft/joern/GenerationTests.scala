package io.shiftleft.joern

import java.io.File

import io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg
import org.scalatest.{Matchers, WordSpec}
import io.shiftleft.semanticcpg.language._

/**
  * Test code that shows how code property graphs can be
  * generated using the FuzzyC language frontend
  * */
class GenerationTests extends WordSpec with Matchers {
  "should generate and load CPG for example code" in {
    val inputFilenames = Array("joern-cli/src/test/resources/testcode/free/")
    val tmpFile = File.createTempFile("cpg", ".bin.zip")
    val outputFilename = tmpFile.getPath
    tmpFile.delete()

    // Create a CPG using the C/C++ fuzzy parser
    val fuzzyc2Cpg = new FuzzyC2Cpg(outputFilename)
    fuzzyc2Cpg.runAndOutput(inputFilenames)
    // Link CPG fragments and enhance to create semantic CPG
    Cpg2Scpg.run(outputFilename)

    // Load the CPG
    val cpg = CpgLoader.load(outputFilename)
    // Query to retrieve all method names
    cpg.method.name.l should not be empty
  }
}
