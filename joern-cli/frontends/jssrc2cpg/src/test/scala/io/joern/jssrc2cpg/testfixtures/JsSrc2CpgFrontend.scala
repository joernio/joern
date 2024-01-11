package io.joern.jssrc2cpg.testfixtures

import better.files.File
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.Inside

trait JsSrc2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile(suffix = "cpg.bin")
    cpgOutFile.deleteOnExit()
    val jssrc2cpg = new JsSrc2Cpg()
    val config = getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(Config(tsTypes = false))
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    jssrc2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithJsSrc(val fileSuffix: String)
    extends DefaultTestCpg
    with JsSrc2CpgFrontend
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override protected def applyPostProcessingPasses(): Unit =
    JsSrc2Cpg.postProcessingPasses(this).foreach(_.createAndApply())

}

class JsSrc2CpgSuite(
  fileSuffix: String = ".js",
  withOssDataflow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithJsSrc(fileSuffix).withOssDataflow(withOssDataflow).withExtraFlows(extraFlows)
    )
    with SemanticCpgTestFixture(extraFlows)
    with Inside
