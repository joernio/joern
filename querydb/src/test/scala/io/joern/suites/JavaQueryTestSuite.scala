package io.joern.suites

import io.joern.console.scan._
import io.joern.console.{Query, QueryBundle}
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.joern.util.QueryUtil
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, Method, StoredNode}

class JavaQueryTestSuite[QB <: QueryBundle](val queryBundle: QB)
    extends JavaSrcCode2CpgFixture(withOssDataflow = true) {
  val argumentProvider = new QDBArgumentProvider(3)

  override def beforeAll(): Unit = {
    super.beforeAll()
  }

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

  def findMatchingCalls(cpg: Cpg, q: Query): List[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: Call => c.code }
  }

  def findMatchingLiterals(cpg: Cpg, q: Query): List[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: Literal => c.code }
  }

  def findMatchingMethods(cpg: Cpg, q: Query): List[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: Method => c.name }
  }

  protected val cpg: TestCpg = code(concatQueryCodeExamples)
}
