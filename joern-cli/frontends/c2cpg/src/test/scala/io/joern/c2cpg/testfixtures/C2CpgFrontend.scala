package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.{C2Cpg, Config}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.validation.{PostFrontendValidator, ValidationLevel}

trait C2CpgFrontend extends LanguageFrontend {
  override type ConfigType = Config

  def execute(sourceCodePath: java.io.File): Cpg = {
    val c2cpg  = new C2Cpg()
    val config = getConfig().getOrElse(Config()).withInputPath(sourceCodePath.getAbsolutePath)
    val res    = c2cpg.createCpg(config).get
    new PostFrontendValidator(res, ValidationLevel.V1).run()
    res
  }
}
