package io.joern.suites

import io.joern.console.scan.*
import io.joern.console.{CodeSnippet, Query, QueryBundle}
import io.joern.util.QueryUtil
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, Method}
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.testfixtures.TestCpg
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture

/** A test suite for running security queries on PHP code. This suite is built on top of `PhpCode2CpgFixture` to support
  * Code Property Graph (CPG) analysis.
  *
  * @param queryBundle
  *   The set of security queries to be tested.
  */
class PHPQueryTestSuite[QB <: QueryBundle](val queryBundle: QB) extends PhpCode2CpgFixture(runOssDataflow = true) {

  val argumentProvider = new QDBArgumentProvider(3)

  override def beforeAll(): Unit = {
    super.beforeAll()
  }

  /** Retrieves all available queries from the query bundle. */
  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  /** Concatenates all positive and negative query code examples to construct a representative CPG for testing.
    */
  def concatQueryCodeExamples: String =
    allQueries
      .map { q =>
        q.codeExamples.positive.mkString("\n") + "\n" +
          q.codeExamples.negative.mkString("\n")
      }
      .mkString("\n")

  /** Generates a Code Property Graph (CPG) from a list of code snippets.
    *
    * @param snippets
    *   List of PHP code snippets
    * @return
    *   A constructed CPG from the provided snippets
    */
  protected def cpgForSnippets(snippets: List[CodeSnippet]): Cpg = {
    val first = snippets.head
    val cpg   = code(first.content, first.filename)
    snippets.drop(1).foldLeft(cpg) { (foldCpg, e) =>
      foldCpg.moreCode(e.content, e.filename)
    }
  }

  /** Extracts call expressions from the CPG that match the given query.
    *
    * @param cpg
    *   The Code Property Graph
    * @param q
    *   The query to execute
    * @return
    *   A list of matching call expressions as strings
    */
  def findMatchingCalls(cpg: Cpg, q: Query): List[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: Call => c.code }
  }

  /** Extracts literals from the CPG that match the given query.
    *
    * @param cpg
    *   The Code Property Graph
    * @param q
    *   The query to execute
    * @return
    *   A list of matching literal values as strings
    */
  def findMatchingLiterals(cpg: Cpg, q: Query): List[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: Literal => c.code }
  }

  /** Extracts method names from the CPG that match the given query.
    *
    * @param cpg
    *   The Code Property Graph
    * @param q
    *   The query to execute
    * @return
    *   A list of matching method names as strings
    */
  def findMatchingMethods(cpg: Cpg, q: Query): List[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: Method => c.name }
  }

  /** Builds a CPG from concatenated query examples for initial test execution.
    */
  protected val cpg: TestCpg = code(concatQueryCodeExamples)
}
