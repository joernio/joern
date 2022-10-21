package io.joern.suites

import io.joern.util.QueryUtil
import io.joern.console.{CodeSnippet, Query, QueryBundle}
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.ProjectRoot
import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.shiftleft.semanticcpg.language._

import java.io.File

trait Kotlin2CpgFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".kt"

  def execute(sourceCodePath: File): Cpg = {
    val cpgFile = File.createTempFile("kt2cpg", ".zip")
    cpgFile.deleteOnExit()
    val kt2cpg = new Kotlin2Cpg()
    val config = Config(inputPath = sourceCodePath.getAbsolutePath, outputPath = cpgFile.getAbsolutePath)
    kt2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithKotlin extends DefaultTestCpg with Kotlin2CpgFrontend

class AndroidQueryTestSuite extends Code2CpgFixture(() => new DefaultTestCpgWithKotlin()) {

  val argumentProvider = new QDBArgumentProvider(3)

  def queryBundle: QueryBundle = QueryUtil.EmptyBundle

  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  protected def cpgForSnippets(snippets: List[CodeSnippet]): Cpg = {
    val first = snippets(0)
    val cpg   = code(first.content, first.filename)
    snippets.drop(1).foldLeft(cpg) { (foldCpg, e) =>
      foldCpg.moreCode(e.content, e.filename)
    }
  }

  def findMatchingConfigFiles(cpg: Cpg, q: Query): Set[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.name.toSetImmutable
  }
}
