package io.joern.php2cpg.testfixtures

import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}

import java.io.File
import io.joern.x2cpg.testfixtures.TestCpg
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.joern.dataflowengineoss.layers.dataflows.OssDataFlowOptions
import io.joern.dataflowengineoss.layers.dataflows.OssDataFlow
import io.joern.php2cpg.passes.PhpSetKnownTypesPass

trait PhpFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".php"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config = getConfig().map(_.asInstanceOf[Config]).getOrElse(Config())
    new Php2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }
}

class PhpTestCpg(runOssDataflow: Boolean) extends TestCpg with PhpFrontend {

  override protected def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    if (runOssDataflow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
    Php2Cpg.postProcessingPasses(this).foreach(_.createAndApply())
  }
}

class PhpCode2CpgFixture(runOssDataflow: Boolean = false)
    extends Code2CpgFixture(() => new PhpTestCpg(runOssDataflow)) {
  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()
}
