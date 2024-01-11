package io.joern.kotlin2cpg.testfixtures

import better.files.File as BFile
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

import java.io.File

trait KotlinFrontend extends LanguageFrontend {
  protected val withTestResourcePaths: Boolean

  override val fileSuffix: String = ".kt"

  override def execute(sourceCodeFile: File): Cpg = {
    val defaultContentRoot =
      BFile(ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/jars/"))
    implicit val defaultConfig: Config =
      Config(
        classpath = if (withTestResourcePaths) Set(defaultContentRoot.path.toAbsolutePath.toString) else Set(),
        includeJavaSourceFiles = true
      )
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
  extraFlows: List[FlowSemantic] = List.empty
) extends Code2CpgFixture(() =>
      new KotlinTestCpg(withDefaultJars)
        .withOssDataflow(withOssDataflow)
        .withExtraFlows(extraFlows)
        .withPostProcessingPasses(withPostProcessing)
    )
    with SemanticCpgTestFixture(extraFlows) {

  protected def flowToResultPairs(path: Path): List[(String, Option[Integer])] = path.resultPairs()
}
