package io.joern.javasrc2cpg.testfixtures

import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.javasrc2cpg.JavaSrc2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Literal}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JavaDataflowFixture(extraFlows: List[FlowSemantic] = List.empty) extends AnyFlatSpec with Matchers {

  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()

  val code: String  = ""
  lazy val cpg: Cpg = JavaSrc2CpgTestContext.buildCpgWithDataflow(code, extraFlows = extraFlows)

  def getConstSourceSink(
    methodName: String,
    sourceCode: String = "\"MALICIOUS\"",
    sinkPattern: String = ".*println.*"
  ): (Iterator[Literal], Iterator[Expression]) = {
    getMultiFnSourceSink(methodName, methodName, sourceCode, sinkPattern)
  }

  def getMultiFnSourceSink(
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
