package io.joern.suites

import io.joern.util.QueryUtil
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.scan.*
import io.joern.console.QueryBundle
import io.joern.console.Query
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.semanticcpg.language.*

class CQueryTestSuite[QB <: QueryBundle](val queryBundle: QB) extends DataFlowCodeToCpgSuite {

  private val argumentProvider = new QDBArgumentProvider(3)

  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  def concatQueryCodeExamples: String =
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

  protected val cpg: TestCpg = code(concatQueryCodeExamples)
}
