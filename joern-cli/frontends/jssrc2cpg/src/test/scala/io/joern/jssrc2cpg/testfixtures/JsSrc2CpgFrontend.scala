package io.joern.jssrc2cpg.testfixtures

import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.validation.PostFrontendValidator

trait JsSrc2CpgFrontend extends LanguageFrontend {
  override type ConfigType = Config

  def execute(sourceCodePath: java.io.File): Cpg = {
    val jssrc2cpg = new JsSrc2Cpg()
    val config = getConfig()
      .getOrElse(Config(tsTypes = false))
      .withInputPath(sourceCodePath.getAbsolutePath)
    val res = jssrc2cpg.createCpg(config).get
    new PostFrontendValidator(res, true).run()
    res
  }
}
