package io.joern.rubysrc2cpg.testfixtures

import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.rubysrc2cpg.deprecated.utils.PackageTable
import io.joern.rubysrc2cpg.parser.{ResourceManagedParser, RubyNodeCreator, RubyParser}
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.testfixtures.*
import io.joern.x2cpg.utils.{ConcurrentTaskUtil, ConcurrentTaskUtilTests, TestCodeWriter}
import io.joern.x2cpg.{SourceFiles, ValidationMode, X2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import org.scalatest.Tag
import better.files.File as BFile

import java.io.File
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AnyWordSpecLike}
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path as FPath}
import scala.util.{Failure, Success, Using}

trait RubyFrontend(useDeprecatedFrontend: Boolean, withDownloadDependencies: Boolean) extends LanguageFrontend {
  override val fileSuffix: String = ".rb"

  implicit val config: Config =
    getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(Config().withSchemaValidation(ValidationMode.Enabled))
      .withUseDeprecatedFrontend(useDeprecatedFrontend)
      .withDownloadDependencies(withDownloadDependencies)

  override def execute(sourceCodeFile: File): Cpg = {
    println(sourceCodeFile.getAbsolutePath)
    new RubySrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }

}

class DefaultTestCpgWithRuby(
  packageTable: Option[PackageTable],
  useDeprecatedFrontend: Boolean,
  downloadDependencies: Boolean = false
) extends DefaultTestCpg
    with RubyFrontend(useDeprecatedFrontend, downloadDependencies)
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override protected def applyPostProcessingPasses(): Unit = {
    packageTable match {
      case Some(table) =>
        RubySrc2Cpg.packageTableInfo.set(table)
      case None =>
    }
    RubySrc2Cpg.postProcessingPasses(this, config).foreach(_.createAndApply())
  }
}

class RubyCode2CpgFixture(
  withPostProcessing: Boolean = false,
  withDataFlow: Boolean = false,
  downloadDependencies: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty,
  packageTable: Option[PackageTable] = None,
  useDeprecatedFrontend: Boolean = false
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithRuby(packageTable, useDeprecatedFrontend, downloadDependencies)
        .withOssDataflow(withDataFlow)
        .withExtraFlows(extraFlows)
        .withPostProcessingPasses(withPostProcessing)
    )
    with Inside
    with SemanticCpgTestFixture(extraFlows) {

  implicit val resolver: ICallResolver = NoResolve

  protected def flowToResultPairs(path: Path): List[(String, Int)] =
    path.resultPairs().collect { case (firstElement, secondElement) =>
      (firstElement, secondElement.getOrElse(-1))
    }
}

class RubyCfgTestCpg(useDeprecatedFrontend: Boolean = true, downloadDependencies: Boolean = false)
    extends CfgTestCpg
    with RubyFrontend(useDeprecatedFrontend, downloadDependencies) {
  override val fileSuffix: String = ".rb"

}

/** Denotes a test which has been similarly ported to the new frontend.
  */
object SameInNewFrontend extends Tag("SameInNewFrontend")

/** Denotes a test which has been ported to the new frontend, but has different expectations.
  */
object DifferentInNewFrontend extends Tag("DifferentInNewFrontend")
