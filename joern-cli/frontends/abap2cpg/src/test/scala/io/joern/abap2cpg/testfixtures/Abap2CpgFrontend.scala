package io.joern.abap2cpg.testfixtures

import io.joern.abap2cpg.{Abap2Cpg, Config}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg

import java.io.File

trait Abap2CpgFrontend extends LanguageFrontend {
  override type ConfigType = Config

  def execute(sourceCodePath: File): Cpg = {
    val abap2cpg = new Abap2Cpg()
    val config   = getConfig().getOrElse(Config()).withInputPath(sourceCodePath.getAbsolutePath)
    abap2cpg.createCpg(config).get
  }
}
