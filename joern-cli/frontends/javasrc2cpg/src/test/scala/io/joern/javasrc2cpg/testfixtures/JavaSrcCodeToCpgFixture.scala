package io.joern.javasrc2cpg.testfixtures

import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.frontendspecific.javasrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Literal}
import io.shiftleft.semanticcpg.language.*

import java.io.File

trait JavaSrcFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    val config = getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(JavaSrc2Cpg.DefaultConfig.withDelombokMode("no-delombok"))
      .withCacheJdkTypeSolver(true)
    new JavaSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath)(config).get
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
  extraFlows: List[FlowSemantic] = List.empty,
  enableTypeRecovery: Boolean = false
) extends Code2CpgFixture(() =>
      new JavaSrcTestCpg(enableTypeRecovery).withOssDataflow(withOssDataflow).withExtraFlows(extraFlows)
    )
    with SemanticCpgTestFixture(extraFlows) {

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
}
