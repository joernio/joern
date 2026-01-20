package io.joern.swiftsrc2cpg.testfixtures

import io.joern.swiftsrc2cpg.{Config, SwiftSrc2Cpg}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.validation.PostFrontendValidator

trait SwiftSrc2CpgFrontend extends LanguageFrontend {
  override type ConfigType = Config

  def execute(sourceCodePath: java.io.File): Cpg = {
    val config = getConfig()
      .fold(Config())(identity)
      .withInputPath(sourceCodePath.getAbsolutePath)

    val tmp = new SwiftSrc2Cpg().createCpg(config).get
    new PostFrontendValidator(tmp, false).run()
    tmp
  }
}
