package io.joern.php2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.frontendspecific.php2cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.validation.PostFrontendValidator

import java.io.File

trait PhpFrontend extends LanguageFrontend {
  override type ConfigType = Config

  override val fileSuffix: String = ".php"

  override def execute(sourceCodeFile: File): Cpg = {
    val defaultConfig: Config = getConfig().getOrElse(Config())
    val cpg                   = new Php2Cpg().createCpg(defaultConfig.withInputPath(sourceCodeFile.getAbsolutePath)).get
    new PostFrontendValidator(cpg, false).run()
    cpg
  }
}

class PhpTestCpg extends DefaultTestCpg with PhpFrontend with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override protected def applyPostProcessingPasses(): Unit =
    php2cpg.postProcessingPasses(this).foreach(_.createAndApply())

}

class PhpCode2CpgFixture(
  runOssDataflow: Boolean = false,
  semantics: Semantics = DefaultSemantics(),
  withPostProcessing: Boolean = false
) extends Code2CpgFixture(() =>
      new PhpTestCpg()
        .withOssDataflow(runOssDataflow)
        .withSemantics(semantics)
        .withPostProcessingPasses(withPostProcessing)
    )
    with SemanticCpgTestFixture(semantics) {
  implicit val resolver: ICallResolver = NoResolve
}
