package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.{C2Cpg, Config}
import io.joern.c2cpg.parser.FileDefaults
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}
import org.scalatest.Inside

import java.io.File

class C2CpgFrontend(override val fileSuffix: String = FileDefaults.C_EXT) extends LanguageFrontend {
  def execute(sourceCodePath: File): Cpg = {
    val cpgFile = File.createTempFile("c2cpg", ".zip")
    cpgFile.deleteOnExit()
    val c2cpg = new C2Cpg()
    val config = Config(
      inputPaths = Set(sourceCodePath.getAbsolutePath),
      outputPath = cpgFile.getAbsolutePath,
      includeComments = true
    )
    c2cpg.createCpg(config).get
  }
}

class CCodeToCpgSuite(fileSuffix: String = FileDefaults.C_EXT)
    extends CodeToCpgFixture(new C2CpgFrontend(fileSuffix))
    with Inside
