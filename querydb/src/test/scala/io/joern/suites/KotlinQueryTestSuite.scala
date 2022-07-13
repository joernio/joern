package io.joern.suites

import io.joern.util.QueryUtil
import io.joern.console.QueryBundle
import io.joern.console.Query
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, TestCpg}
import io.shiftleft.utils.ProjectRoot

class KotlinQueryTestSuite extends Code2CpgFixture(new Kotlin2CpgFrontend()) {
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

  protected val cpg: TestCpg = code(concatQueryCodeExamples)
}
