package io.joern.kotlin2cpg.testfixtures

import better.files.{File => BFile}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend, TestCpg}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

import java.io.File

trait KotlinFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".kt"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(
        Config(
          // classpath = if (withTestResourcePaths) Set(defaultContentRoot.path.toAbsolutePath.toString) else Set(),
          includeJavaSourceFiles = true
        )
      )
    new Kotlin2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class KotlinTestCpg() extends TestCpg with KotlinFrontend {
  private var _withOssDataflow = false

  def withOssDataflow(value: Boolean = true): this.type = {
    _withOssDataflow = value
    this
  }

  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)

    if (_withOssDataflow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }
}

class KotlinCode2CpgFixture(withOssDataflow: Boolean = false)
    extends Code2CpgFixture(() => new KotlinTestCpg().withOssDataflow(withOssDataflow)) {

  implicit val context: EngineContext = EngineContext()

  protected def flowToResultPairs(path: Path): List[(String, Option[Integer])] = path.resultPairs()

  protected def getTestResourcesPaths(): Set[String] = {
    val defaultContentRoot =
      BFile(ProjectRoot.relativise("joern-cli/frontends/kotlin2cpg/src/test/resources/jars/"))

    Set(defaultContentRoot.path.toAbsolutePath().toString)
  }
}
