package io.joern.javasrc2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.frontendspecific.javasrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.joern.x2cpg.utils.{ArtifactFetcher, HttpArtifact}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Literal}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.validation.PostFrontendValidator

import java.io.File
import java.nio.file.Path as JavaPath

trait JavaSrcFrontend extends LanguageFrontend {
  final override type ConfigType = Config
  override val fileSuffix: String = ".java"

  def withRemoteInferenceJars(artifacts: HttpArtifact*): this.type = {
    artifacts.foreach { artifact =>
      ArtifactFetcher.fetch(artifact).foreach { jarPath =>
        val currentConfig = getConfig().getOrElse(JavaSrc2Cpg.DefaultConfig.withDelombokMode("no-delombok"))
        setConfig(currentConfig.withInferenceJarPaths(currentConfig.inferenceJarPaths + jarPath.toString))
      }
    }
    this
  }

  override def execute(sourceCodeFile: File): Cpg = {
    val config = getConfig()
      .getOrElse(JavaSrc2Cpg.DefaultConfig.withDelombokMode("no-delombok"))
      .withCacheJdkTypeSolver(true)
    val res = new JavaSrc2Cpg().createCpg(config.withInputPath(sourceCodeFile.getAbsolutePath)).get
    new PostFrontendValidator(res, false).run()
    res
  }
}

class JavaSrcTestCpg(enableTypeRecovery: Boolean = false)
    extends DefaultTestCpg
    with JavaSrcFrontend
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    if (enableTypeRecovery)
      javasrc2cpg.typeRecoveryPasses(this, XTypeRecoveryConfig(enabledDummyTypes = true)).foreach(_.createAndApply())
    applyOssDataFlow()
  }

}

class JavaSrcCode2CpgFixture(
  withOssDataflow: Boolean = false,
  semantics: Semantics = DefaultSemantics(),
  enableTypeRecovery: Boolean = false
) extends Code2CpgFixture(() =>
      new JavaSrcTestCpg(enableTypeRecovery).withOssDataflow(withOssDataflow).withSemantics(semantics)
    )
    with SemanticCpgTestFixture(semantics) {

  implicit val resolver: ICallResolver = NoResolve

  def getConstSourceSink(
    cpg: Cpg,
    methodName: String,
    sourceCode: String = "\"MALICIOUS\"",
    sinkPattern: String = ".*println.*"
  ): (Iterator[Literal], Iterator[Expression]) = {
    getMultiFnSourceSink(cpg, methodName, methodName, sourceCode, sinkPattern)
  }

  def getMultiFnSourceSink(
    cpg: Cpg,
    sourceMethodName: String,
    sinkMethodName: String,
    sourceCode: String = "\"MALICIOUS\"",
    sinkPattern: String = ".*println.*"
  ): (Iterator[Literal], Iterator[Expression]) = {
    val sourceMethod = cpg.method(s".*$sourceMethodName.*").head
    val sinkMethod   = cpg.method(s".*$sinkMethodName.*").head
    def source       = sourceMethod.literal.code(sourceCode)
    def sink         = sinkMethod.call.name(sinkPattern).argument(1).ast.collectAll[Expression]

    // If either of these fail, then the testcase was written incorrectly or the AST was created incorrectly.
    if (source.size <= 0) {
      fail(s"Could not find source $sourceCode in method $sourceMethodName")
    }
    if (sink.size <= 0) {
      fail(s"Could not find sink $sinkPattern for method $sinkMethodName")
    }

    (source, sink)
  }

  protected def flowToResultPairs(path: Path): List[(String, Option[Int])] = path.resultPairs()
}
