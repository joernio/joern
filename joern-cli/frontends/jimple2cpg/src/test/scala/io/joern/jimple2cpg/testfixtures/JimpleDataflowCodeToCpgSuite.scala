package io.joern.jimple2cpg.testfixtures

import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import overflowdb.traversal.Traversal

class JimpleDataflowTestCpg extends JimpleTestCpg {

  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()

  override def applyPasses(): Unit = {
    super.applyPasses()
    val context = new LayerCreatorContext(this)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
  }

}

class JimpleDataFlowCodeToCpgSuite extends Code2CpgFixture(() => new JimpleDataflowTestCpg()) {

  implicit var context: EngineContext = EngineContext()

  def getConstSourceSink(methodName: String, sourceCode: String = "\"MALICIOUS\"", sinkPattern: String = ".*println.*")(
    implicit cpg: Cpg
  ): (Traversal[Literal], Traversal[Expression]) = {
    getMultiFnSourceSink(methodName, methodName, sourceCode, sinkPattern)
  }

  def getMultiFnSourceSink(
    sourceMethodName: String,
    sinkMethodName: String,
    sourceCode: String = "\"MALICIOUS\"",
    sinkPattern: String = ".*println.*"
  )(implicit cpg: Cpg): (Traversal[Literal], Traversal[Expression]) = {
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
