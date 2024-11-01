package io.joern.rubysrc2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.testfixtures.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import org.scalatest.Inside

import java.io.File
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

trait RubyFrontend(withDownloadDependencies: Boolean, disableFileContent: Boolean) extends LanguageFrontend {
  override val fileSuffix: String = ".rb"

  implicit val config: Config =
    getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(Config().withSchemaValidation(ValidationMode.Enabled))
      .withDownloadDependencies(withDownloadDependencies)
      .withDisableFileContent(disableFileContent)

  override def execute(sourceCodeFile: File): Cpg = {
    new RubySrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }

}

class DefaultTestCpgWithRuby(downloadDependencies: Boolean = false, disableFileContent: Boolean = true)
    extends DefaultTestCpg
    with RubyFrontend(downloadDependencies, disableFileContent)
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override protected def applyPostProcessingPasses(): Unit = {
    RubySrc2Cpg.postProcessingPasses(this, config).foreach(_.createAndApply())
  }
}

class RubyCode2CpgFixture(
  withPostProcessing: Boolean = false,
  withDataFlow: Boolean = false,
  downloadDependencies: Boolean = false,
  disableFileContent: Boolean = true,
  semantics: Semantics = DefaultSemantics()
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithRuby(downloadDependencies, disableFileContent)
        .withOssDataflow(withDataFlow)
        .withSemantics(semantics)
        .withPostProcessingPasses(withPostProcessing)
    )
    with Inside
    with SemanticCpgTestFixture(semantics) {

  implicit val resolver: ICallResolver = NoResolve

  protected def flowToResultPairs(path: Path): List[(String, Int)] =
    path.resultPairs().collect { case (firstElement, secondElement) =>
      (firstElement, secondElement.getOrElse(-1))
    }
}

class RubyCfgTestCpg(downloadDependencies: Boolean = false, disableFileContent: Boolean = true)
    extends CfgTestCpg
    with RubyFrontend(downloadDependencies, disableFileContent) {
  override val fileSuffix: String = ".rb"

}
