package io.joern.suites

import io.joern.util.QueryUtil
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan._
import io.joern.console.QueryBundle
import io.joern.console.Query
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class CQueryTestSuite extends DataFlowCodeToCpgSuite {
  val argumentProvider = new QDBArgumentProvider(3)

  override def beforeAll(): Unit = {
    semanticsFilename = argumentProvider.testSemanticsFilename
    super.beforeAll()
  }

  def queryBundle: QueryBundle = QueryUtil.EmptyBundle

  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  def concatedQueryCodeExamples: String =
    allQueries
      .map { q =>
        q.codeExamples.positive
          .mkString("\n")
          .concat("\n")
          .concat(
            q.codeExamples.negative
              .mkString("\n")
          )
      }
      .mkString("\n")

  /** Used for tests that match names of vulnerable functions
    */
  def findMatchingCalls(query: Query): Set[String] = {
    query(cpg)
      .flatMap(_.evidence)
      .collect { case call: nodes.Call => call }
      .method
      .name
      .toSetImmutable
  }

  override val code: String = concatedQueryCodeExamples
}
