package io.joern.go2cpg.testfixtures

import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.Inside
trait Go2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile("go2cpg.bin")
    cpgOutFile.deleteOnExit()
    val go2cpg = new GoSrc2Cpg()
    val config = Config()
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    go2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithGo(val fileSuffix: String) extends DefaultTestCpg with Go2CpgFrontend {

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

class GoCodeToCpgSuite(fileSuffix: String = ".go", withOssDataflow: Boolean = false)
    extends Code2CpgFixture(() => new DefaultTestCpgWithGo(fileSuffix).withOssDataflow(withOssDataflow))
    with Inside {
  implicit lazy val engineContext: EngineContext = EngineContext()
}
