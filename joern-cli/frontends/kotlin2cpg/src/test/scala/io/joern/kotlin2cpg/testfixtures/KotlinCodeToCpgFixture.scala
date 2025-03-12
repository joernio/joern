package io.joern.kotlin2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.dataflowengineoss.testfixtures.SemanticCpgTestFixture
import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.kotlin2cpg.Config
import io.joern.kotlin2cpg.Kotlin2Cpg
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.joern.x2cpg.testfixtures.DefaultTestCpg
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.utils.ProjectRoot

import java.io.File
import java.nio.file.Paths

trait KotlinFrontend extends LanguageFrontend {
  protected val withTestResourcePaths: Boolean

  override val fileSuffix: String = ".kt"
  private lazy val defaultContentRoot =
    Paths.get(ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/jars/"))
  private lazy val defaultConfig: Config =
    Config(
      classpath = if (withTestResourcePaths) Set(defaultContentRoot.absolutePathAsString) else Set(),
      includeJavaSourceFiles = true
    )

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val config: Config = getConfig() match {
      case Some(config: Config) => config
      case _ =>
        setConfig(defaultConfig)
        defaultConfig
    }

    new Kotlin2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class KotlinTestCpg(override protected val withTestResourcePaths: Boolean)
    extends DefaultTestCpg
    with KotlinFrontend
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override protected def applyPostProcessingPasses(): Unit = Kotlin2Cpg.postProcessingPass(this)

}

class KotlinCode2CpgFixture(
  withOssDataflow: Boolean = false,
  withDefaultJars: Boolean = false,
  withPostProcessing: Boolean = false,
  semantics: Semantics = DefaultSemantics()
) extends Code2CpgFixture(() =>
      new KotlinTestCpg(withDefaultJars)
        .withOssDataflow(withOssDataflow)
        .withSemantics(semantics)
        .withPostProcessingPasses(withPostProcessing)
    )
    with SemanticCpgTestFixture(semantics) {

  protected def flowToResultPairs(path: Path): List[(String, Option[Int])] = path.resultPairs()
}
