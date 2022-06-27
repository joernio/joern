package io.joern.jssrc2cpg.testfixtures

import better.files.File
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.layers.Base
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.Inside

class JsSrc2CpgFrontend(override val fileSuffix: String = ".js") extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    var _cpg = Cpg.emptyCpg
    File.usingTemporaryFile("cpg", ".bin") { cpgFile =>
      val jssrc2cpg = new JsSrc2Cpg()
      val config    = Config(inputPath = sourceCodePath.getAbsolutePath, outputPath = cpgFile.toString())
      val cpg       = jssrc2cpg.createCpg(config).get
      val context   = new LayerCreatorContext(cpg)
      new Base().run(context)
      _cpg = cpg
    }
    _cpg
  }
}

class JsSrc2CpgSuite(fileSuffix: String = ".js") extends Code2CpgFixture(new JsSrc2CpgFrontend(fileSuffix)) with Inside
