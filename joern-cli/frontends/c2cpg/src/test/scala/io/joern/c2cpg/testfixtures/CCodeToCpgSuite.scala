package io.joern.c2cpg.testfixtures

import better.files.File
import io.joern.c2cpg.{C2Cpg, Config}
import io.joern.c2cpg.parser.FileDefaults
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.LanguageFrontend
import org.scalatest.Inside

class C2CpgFrontend(override val fileSuffix: String = FileDefaults.C_EXT) extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile("c2cpg.bin")
    cpgOutFile.deleteOnExit()
    val c2cpg = new C2Cpg()
    val config = Config(
      inputPath = sourceCodePath.getAbsolutePath,
      outputPath = cpgOutFile.pathAsString,
      includeComments = true,
      includePathsAutoDiscovery = false
    )
    c2cpg.createCpg(config).get
  }
}

class CCodeToCpgSuite(fileSuffix: String = FileDefaults.C_EXT)
    extends Code2CpgFixture(new C2CpgFrontend(fileSuffix))
    with Inside
