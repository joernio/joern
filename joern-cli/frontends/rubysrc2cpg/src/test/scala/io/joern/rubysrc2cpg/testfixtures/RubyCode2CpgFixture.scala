package io.joern.rubysrc2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.rubysrc2cpg.parser.RubyAstGenRunner.JRubyEnvironment
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.testfixtures.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.validation.{PostFrontendValidator, ValidationLevel}
import org.scalatest.Inside

import java.io.File
import java.nio.charset.Charset
import scala.util.Using

trait RubyFrontend(withDownloadDependencies: Boolean, disableFileContent: Boolean) extends LanguageFrontend {
  override type ConfigType = Config

  override val fileSuffix: String = ".rb"

  protected def sharedJRubyEnv: Option[JRubyEnvironment] = None

  implicit val config: Config =
    getConfig()
      .getOrElse(Config().withSchemaValidation(ValidationMode.Enabled))
      .withDownloadDependencies(withDownloadDependencies)
      .withDisableFileContent(disableFileContent)

  override def execute(sourceCodeFile: File): Cpg = {
    val tmp =
      Using.resource(new RubySrc2Cpg(sharedJRubyEnv))(
        _.createCpg(config.withInputPath(sourceCodeFile.getAbsolutePath)).get
      )
    new PostFrontendValidator(tmp, ValidationLevel.V0).run()
    tmp
  }

}

class DefaultTestCpgWithRuby(downloadDependencies: Boolean = false, disableFileContent: Boolean = true)
    extends DefaultTestCpg
    with RubyFrontend(downloadDependencies, disableFileContent)
    with SemanticTestCpg {

  private var _sharedJRubyEnv: Option[JRubyEnvironment] = None

  def withSharedJRubyEnv(env: JRubyEnvironment): this.type = {
    _sharedJRubyEnv = Some(env)
    this
  }

  override protected def sharedJRubyEnv: Option[JRubyEnvironment] = _sharedJRubyEnv

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

  private lazy val _sharedJRubyEnv = JRubyEnvironment()

  override def code(code: String): DefaultTestCpgWithRuby = {
    super.code(code).withSharedJRubyEnv(_sharedJRubyEnv)
  }

  override def code(code: String, fileName: String): DefaultTestCpgWithRuby = {
    super.code(code, fileName).withSharedJRubyEnv(_sharedJRubyEnv)
  }

  override def code(code: String, fileName: String, charset: Charset): DefaultTestCpgWithRuby = {
    super.code(code, fileName, charset).withSharedJRubyEnv(_sharedJRubyEnv)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    _sharedJRubyEnv.close()
  }

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
