package io.joern.rubysrc2cpg.testfixtures

import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}

import java.io.File

trait RubyFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".rb"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config()
    new RubySrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class DefaultTestCpgWithPhp extends DefaultTestCpg with RubyFrontend

class RubyCode2CpgFixture extends Code2CpgFixture(() => new DefaultTestCpgWithPhp) {
  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()
}
