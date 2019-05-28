package io.shiftleft.joern

import io.shiftleft.fuzzyc2cpg.Fuzzyc2Cpg
import org.scalatest.{Matchers, WordSpec}

/**
  * Test code that shows how code property graphs can be
  * generated using the FuzzyC language frontend
  * */
class GenerationTests extends WordSpec with Matchers {
  "should generate and load CPG for example code" in {
    val inputFilenames = Array("joern-cli/src/test/resources/testcode/free/")
    val outputFilename = "/tmp/cpg.bin.zip"

    // Create a CPG using the C/C++ fuzzy parser
    val fuzzyc2Cpg = new Fuzzyc2Cpg(outputFilename)
    fuzzyc2Cpg.runAndOutput(inputFilenames)
    // Link CPG fragments and enhance to create semantic CPG
    Cpg2Scpg.run(outputFilename)

    // Load the CPG
    val cpg = CpgLoader.load(outputFilename)
    // Query to retrieve all method names
    cpg.method.name.l should not be empty
  }
}
