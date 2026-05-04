package io.joern.rust2cpg.testfixtures

import io.joern.rust2cpg.{Config, Rust2Cpg}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.validation.{PostFrontendValidator, ValidationLevel}

import java.io.File

trait Rust2CpgFrontend extends LanguageFrontend {
  override type ConfigType = Config

  override def execute(sourceCodePath: File): Cpg = {
    val config = getConfig().getOrElse(Config()).withInputPath(sourceCodePath.getAbsolutePath)
    val cpg    = Rust2Cpg().createCpg(config).get
    PostFrontendValidator(cpg, ValidationLevel.V1).run()
    cpg
  }

}
