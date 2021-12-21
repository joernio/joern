package io.joern

import better.files.File
import io.joern.c2cpg.C2Cpg
import io.shiftleft.codepropertygraph.Cpg

trait AbstractJoernCliTest {

  protected def withTestCpg[T](file: File)(f: ((Cpg, String)) => T): T = {
    f(loadTestCpg(file))
  }

  private def loadTestCpg(file: File): (Cpg, String) = {
    val inputFilenames = Set(file.pathAsString)
    val tmpFile = File.newTemporaryFile("cpg", "bin")
    val c2cpgOutFilename = tmpFile.pathAsString
    tmpFile.delete()

    // Create a CPG using the C/C++ parser
    val c2cpg = new C2Cpg()
    val config = C2Cpg.Config(inputPaths = inputFilenames, outputPath = c2cpgOutFilename)
    c2cpg.runAndOutput(config).close()
    // Link CPG fragments and enhance to create semantic CPG
    val cpg = DefaultOverlays.create(c2cpgOutFilename)
    (cpg, c2cpgOutFilename)
  }

}
