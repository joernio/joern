package io.joern.suites

import io.joern.util.QueryUtil
import io.joern.console.QueryBundle
import io.joern.console.Query
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.ProjectRoot
import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.shiftleft.semanticcpg.language._

import java.io.File

class Kotlin2CpgFrontend(override val fileSuffix: String = ".kt") extends LanguageFrontend {
  def execute(sourceCodePath: File): Cpg = {
    val cpgFile = File.createTempFile("kt2cpg", ".zip")
    cpgFile.deleteOnExit()
    val kt2cpg = new Kotlin2Cpg()
    val config = Config(inputPath = sourceCodePath.getAbsolutePath, outputPath = cpgFile.getAbsolutePath)
    kt2cpg.createCpg(config).get
  }
}

class AndroidQueryTestSuite extends Code2CpgFixture(new Kotlin2CpgFrontend()) {
  var semanticsFilename =
    ProjectRoot.relativise("joern-cli/src/main/resources/default.semantics")
  val argumentProvider = new QDBArgumentProvider(3)

  override def beforeAll(): Unit = {
    super.beforeAll()
    semanticsFilename = argumentProvider.testSemanticsFilename
  }

  def queryBundle: QueryBundle = QueryUtil.EmptyBundle

  def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  def findMatchingConfigFiles(cpg: Cpg, q: Query): Set[String] = {
    q(cpg).flatMap(_.evidence).collect { case c: ConfigFile => c }.name.toSetImmutable
  }
}
