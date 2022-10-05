package io.joern.kotlin2cpg.testfixtures

import better.files.{File => BFile}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

import java.io.File

class KotlinFrontend(withTestResourcePaths: Boolean = false) extends LanguageFrontend {
  override val fileSuffix: String = ".kt"

  override def execute(sourceCodeFile: File): Cpg = {
    val defaultContentRoot =
      BFile(ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/jars/"))
    implicit val defaultConfig: Config =
      Config(classpath = if (withTestResourcePaths) Set(defaultContentRoot.path.toAbsolutePath.toString) else Set())
    new Kotlin2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class KotlinCode2CpgFixture(withOssDataflow: Boolean = false, withDefaultJars: Boolean = false)
    extends Code2CpgFixture(new KotlinFrontend(withTestResourcePaths = withDefaultJars)) {

  implicit val context: EngineContext = EngineContext()

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)

    if (withOssDataflow) {
      val context = new LayerCreatorContext(cpg)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }

  protected def flowToResultPairs(path: Path): List[(String, Option[Integer])] = path.resultPairs()
}
