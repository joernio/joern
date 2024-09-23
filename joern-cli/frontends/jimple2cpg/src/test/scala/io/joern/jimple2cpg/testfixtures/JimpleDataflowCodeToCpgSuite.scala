package io.joern.jimple2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class JimpleDataflowTestCpg(val semantics: Semantics = DefaultSemantics()) extends JimpleTestCpg {

  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()

  override def applyPasses(): Unit = {
    super.applyPasses()
    val context = new LayerCreatorContext(this)
    val options = new OssDataFlowOptions(semantics = semantics)
    new OssDataFlow(options).run(context)
  }

}

class JimpleDataFlowCodeToCpgSuite(val semantics: Semantics = DefaultSemantics())
    extends Code2CpgFixture(() => new JimpleDataflowTestCpg(semantics)) {

  implicit var context: EngineContext = EngineContext()

  def getConstSourceSink(methodName: String, sourceCode: String = "\"MALICIOUS\"", sinkPattern: String = ".*println.*")(
    implicit cpg: Cpg
  ): (Iterator[Literal], Iterator[Expression]) = {
    getMultiFnSourceSink(methodName, methodName, sourceCode, sinkPattern)
  }

  def getMultiFnSourceSink(
    sourceMethodName: String,
    sinkMethodName: String,
    sourceCode: String = "\"MALICIOUS\"",
    sinkPattern: String = ".*println.*"
  )(implicit cpg: Cpg): (Iterator[Literal], Iterator[Expression]) = {
    val sourceMethod = cpg.method(s".*$sourceMethodName").head
    val sinkMethod   = cpg.method(s".*$sinkMethodName").head

    def source = sourceMethod.literal.code(sourceCode)

    def sink = sinkMethod.call.name(sinkPattern).argument(1).ast.collectAll[Expression]

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
