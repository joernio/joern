package io.joern.suites

import io.joern.util.QueryUtil
import io.joern.console.{CodeSnippet, Query, QueryBundle}
import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.joern.console.scan._
import io.shiftleft.utils.ProjectRoot

class KotlinQueryTestSuite extends KotlinCode2CpgFixture(withOssDataflow = true) {
  var semanticsFilename = ProjectRoot.relativise("joern-cli/src/main/resources/default.semantics")
  val argumentProvider  = new QDBArgumentProvider(3)

  override def beforeAll(): Unit = {
    super.beforeAll()
    semanticsFilename = argumentProvider.testSemanticsFilename
  }

  def queryBundle: QueryBundle = QueryUtil.EmptyBundle

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

  protected def cpgForSnippets(snippets: List[CodeSnippet]): Cpg = {
    val first = snippets(0)
    val cpg   = code(first.content, first.filename)
    snippets.drop(1).foldLeft(cpg) { (foldCpg, e) =>
      foldCpg.moreCode(e.content, e.filename)
    }
  }

  def findMatchingCalls(cpg: Cpg, q: Query): List[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: Call => c.code }
  }

  protected val cpg: TestCpg = code(concatQueryCodeExamples)
}
