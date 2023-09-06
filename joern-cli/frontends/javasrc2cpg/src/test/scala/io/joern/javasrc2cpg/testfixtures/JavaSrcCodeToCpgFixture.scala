package io.joern.javasrc2cpg.testfixtures

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Literal}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.io.File

trait JavaSrcFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    val config = getConfig().map(_.asInstanceOf[Config]).getOrElse(JavaSrc2Cpg.DefaultConfig)
    new JavaSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath)(config).get
  }
}

class JavaSrcTestCpg(enableTypeRecovery: Boolean = false) extends TestCpg with JavaSrcFrontend {
  private var _withOssDataflow = false

  def withOssDataflow(value: Boolean = true): this.type = {
    _withOssDataflow = value
    this
  }

  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    if (enableTypeRecovery) JavaSrc2Cpg.typeRecoveryPasses(this).foreach(_.createAndApply())
    if (_withOssDataflow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }

}

class JavaSrcCode2CpgFixture(withOssDataflow: Boolean = false, enableTypeRecovery: Boolean = false)
    extends Code2CpgFixture(() => new JavaSrcTestCpg(enableTypeRecovery).withOssDataflow(withOssDataflow)) {

  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()

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
