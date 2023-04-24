package io.joern.javasrc2cpg.testfixtures

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Literal}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import overflowdb.traversal.Traversal

import java.io.File

trait JavaSrcFrontend extends LanguageFrontend {
  protected val delombokMode: String

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config =
      Config(delombokMode = Some(delombokMode))
    new JavaSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class JavaSrcTestCpg(override protected val delombokMode: String, enableTypeRecovery: Boolean = false)
    extends TestCpg
    with JavaSrcFrontend {
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

class JavaSrcCode2CpgFixture(
  withOssDataflow: Boolean = false,
  delombokMode: String = "default",
  enableTypeRecovery: Boolean = false
) extends Code2CpgFixture(() => new JavaSrcTestCpg(delombokMode, enableTypeRecovery).withOssDataflow(withOssDataflow)) {

  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()

  def getConstSourceSink(
    cpg: Cpg,
    methodName: String,
    sourceCode: String = "\"MALICIOUS\"",
    sinkPattern: String = ".*println.*"
  ): (Traversal[Literal], Traversal[Expression]) = {
    getMultiFnSourceSink(cpg, methodName, methodName, sourceCode, sinkPattern)
  }

  def getMultiFnSourceSink(
    cpg: Cpg,
    sourceMethodName: String,
    sinkMethodName: String,
    sourceCode: String = "\"MALICIOUS\"",
    sinkPattern: String = ".*println.*"
  ): (Traversal[Literal], Traversal[Expression]) = {
    val sourceMethod = cpg.method(s".*$sourceMethodName.*").next()
    val sinkMethod   = cpg.method(s".*$sinkMethodName.*").next()
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
