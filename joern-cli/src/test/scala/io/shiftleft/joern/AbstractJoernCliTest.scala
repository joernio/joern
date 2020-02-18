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
    val tmpFile = File.newTemporaryFile("cpg", "bin.zip")
    val fuzzycOutFilename = tmpFile.pathAsString
    tmpFile.delete()

    // Create a CPG using the C/C++ fuzzy parser
    val fuzzyc2Cpg = new FuzzyC2Cpg(fuzzycOutFilename)
    fuzzyc2Cpg.runAndOutput(inputFilenames, Set(".c"))
    // Link CPG fragments and enhance to create semantic CPG

    val storeTmpFile = File.newTemporaryFile("store", "bin")
    val storeFilename = storeTmpFile.pathAsString
    storeTmpFile.delete()
    Cpg2Scpg.run(fuzzycOutFilename, storeFilename, dataFlow = true, CpgLoader.defaultSemanticsFile)

    // Load the CPG
    (CpgLoader.loadFromOdb(storeFilename), fuzzycOutFilename)
  }

}
