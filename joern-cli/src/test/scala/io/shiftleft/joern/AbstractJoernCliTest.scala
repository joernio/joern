package io.shiftleft.joern

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg

trait AbstractJoernCliTest {

  protected def withTestCpg[T](file: File)(f: ((Cpg, String)) => T): T = {
    f(loadTestCpg(file))
  }

  private def loadTestCpg(file: File): (Cpg, String) = {
    val inputFilenames = Set(file.pathAsString)
    val tmpFile = File.newTemporaryFile("cpg", "bin")
    val fuzzycOutFilename = tmpFile.pathAsString
    tmpFile.delete()

    // Create a CPG using the C/C++ fuzzy parser
    val fuzzyc2Cpg = new FuzzyC2Cpg()
    fuzzyc2Cpg.runAndOutput(inputFilenames, Set(".c"), Some(fuzzycOutFilename)).close()
    // Link CPG fragments and enhance to create semantic CPG
    val cpg = Cpg2Scpg.run(fuzzycOutFilename, dataFlow = true)
    (cpg, fuzzycOutFilename)
  }

}
