package io.joern.php2cpg.testfixtures

import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}

import java.io.File

class PhpFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".php"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config()
    new Php2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class PhpCode2CpgFixture extends Code2CpgFixture(new PhpFrontend()) {
  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()
}
