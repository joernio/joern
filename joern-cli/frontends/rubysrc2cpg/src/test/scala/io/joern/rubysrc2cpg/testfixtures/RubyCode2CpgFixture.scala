package io.joern.rubysrc2cpg.testfixtures

import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
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

trait RubyFrontend(
  withDownloadDependencies: Boolean,
  disableFileContent: Boolean,
  antlrDebugging: Boolean,
  antlrProfiling: Boolean
) extends LanguageFrontend {
  override val fileSuffix: String = ".rb"

  implicit val config: Config =
    getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(Config().withSchemaValidation(ValidationMode.Enabled))
      .withDownloadDependencies(withDownloadDependencies)
      .withDisableFileContent(disableFileContent)
      .withAntlrDebugging(antlrDebugging)
      .withAntlrProfiling(antlrProfiling)

  override def execute(sourceCodeFile: File): Cpg = {
    val cpg = new RubySrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
    if (antlrProfiling) {
      if (sourceCodeFile.isDirectory) {
        Files
          .walk(sourceCodeFile.toPath)
          .iterator()
          .asScala
          .filter(_.getFileName.toString.endsWith(".log"))
          .map(_.toFile)
          .foreach(printAntlrProfilingInfo)
      } else {
        printAntlrProfilingInfo(sourceCodeFile)
      }
    }
    cpg
  }

  private def printAntlrProfilingInfo(logfile: File): Unit = {
    if (logfile.exists()) {
      println(Files.readString(logfile.toPath))
      logfile.delete() // cleanup
    }
  }

}

class DefaultTestCpgWithRuby(
  downloadDependencies: Boolean = false,
  disableFileContent: Boolean = true,
  antlrDebugging: Boolean = false,
  antlrProfiling: Boolean
) extends DefaultTestCpg
    with RubyFrontend(downloadDependencies, disableFileContent, antlrDebugging, antlrProfiling)
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
  extraFlows: List[FlowSemantic] = List.empty,
  antlrDebugging: Boolean = false,
  antlrProfiling: Boolean = false
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithRuby(downloadDependencies, disableFileContent, antlrDebugging, antlrProfiling)
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

class RubyCfgTestCpg(
  downloadDependencies: Boolean = false,
  disableFileContent: Boolean = true,
  antlrDebugging: Boolean = false,
  antlrProfiling: Boolean = false
) extends CfgTestCpg
    with RubyFrontend(downloadDependencies, disableFileContent, antlrDebugging, antlrProfiling) {
  override val fileSuffix: String = ".rb"

}
