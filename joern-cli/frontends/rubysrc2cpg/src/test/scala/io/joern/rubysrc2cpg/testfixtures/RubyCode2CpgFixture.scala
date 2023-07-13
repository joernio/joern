package io.joern.rubysrc2cpg.testfixtures

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend, TestCpg}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.io.File

trait RubyFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".rb"

  override def execute(sourceCodeFile: File): Cpg = {
    implicit val defaultConfig: Config =
      getConfig()
        .map(_.asInstanceOf[Config])
        .getOrElse(Config())
    new RubySrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }

}

class DefaultTestCpgWithRuby(withPostProcessing: Boolean, withDataFlow: Boolean, packageTable: Option[PackageTable])
    extends DefaultTestCpg
    with RubyFrontend {
  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)

    if (withPostProcessing) {
      packageTable match {
        case Some(table) =>
          RubySrc2Cpg.packageTableInfo.clear()
          RubySrc2Cpg.packageTableInfo.set(table)
        case None =>
      }
      RubySrc2Cpg.postProcessingPasses(this).foreach(_.createAndApply())
    }

    if (withDataFlow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }

}

class RubyCode2CpgFixture(
  withPostProcessing: Boolean = false,
  withDataFlow: Boolean = false,
  packageTable: Option[PackageTable] = None
) extends Code2CpgFixture(() => new DefaultTestCpgWithRuby(withPostProcessing, withDataFlow, packageTable)) {

  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()

}
