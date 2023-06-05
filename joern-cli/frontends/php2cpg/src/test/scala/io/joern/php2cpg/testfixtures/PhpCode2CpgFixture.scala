package io.joern.php2cpg.testfixtures

import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}

import java.io.File
import io.joern.x2cpg.X2CpgConfig

trait PhpFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".php"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config()
    new Php2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }

  override def execute[T <: X2CpgConfig[_]](sourceCodeFile: File, config: T): Cpg = {
    config match {
      case phpConfig: Config =>
        new Php2Cpg().createCpg(sourceCodeFile.getAbsolutePath)(phpConfig).get
      case _ =>
        throw new RuntimeException(s"Cannot invoke php2cpg with config type ${config.getClass().getCanonicalName()}")
    }
  }
}

class DefaultTestCpgWithPhp extends DefaultTestCpg with PhpFrontend

class PhpCode2CpgFixture extends Code2CpgFixture(() => new DefaultTestCpgWithPhp) {
  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()
}
