package io.joern.kotlin2cpg.testfixtures

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

import java.io.File

class KotlinFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".kt"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = Config()
    new Kotlin2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class KotlinCode2CpgFixture(withOssDataflow: Boolean = false) extends Code2CpgFixture(new KotlinFrontend) {
  val defaultSemantics = {
    val semanticsFilename: String = ProjectRoot.relativise("joern-cli/src/main/resources/default.semantics")
    Semantics.fromList(new Parser().parseFile(semanticsFilename))
  }
  implicit val context: EngineContext = EngineContext(defaultSemantics)

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)

    if (withOssDataflow) {
      val context = new LayerCreatorContext(cpg)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }
}
