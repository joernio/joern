package io.joern.javasrc2cpg.testfixtures

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, CodeToCpgFixture, LanguageFrontend}
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Literal}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot
import overflowdb.traversal.Traversal

import java.io.File

class JavaSrcFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config(skipDependencyDownload = true)
    new JavaSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class JavaSrcCodeToCpgFixture extends CodeToCpgFixture(new JavaSrcFrontend) {}

class JavaSrcCode2CpgFixture(withOssDataflow: Boolean = false) extends Code2CpgFixture(new JavaSrcFrontend) {

  val semanticsFile: String            = ProjectRoot.relativise("joern-cli/src/main/resources/default.semantics")
  lazy val defaultSemantics: Semantics = Semantics.fromList(new Parser().parseFile(semanticsFile))
  implicit val resolver: ICallResolver = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext(defaultSemantics, EngineConfig(maxCallDepth = 4))

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)

    if (withOssDataflow) {
      val context = new LayerCreatorContext(cpg)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }

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
