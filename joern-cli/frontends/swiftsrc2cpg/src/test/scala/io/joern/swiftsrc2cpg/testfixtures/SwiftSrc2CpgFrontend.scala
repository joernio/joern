package io.joern.swiftsrc2cpg.testfixtures

import better.files.File
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.swiftsrc2cpg.{Config, SwiftSrc2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.Inside

trait SwiftSrc2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile(suffix = "cpg.bin")
    cpgOutFile.deleteOnExit()
    val swiftsrc2cpg = new SwiftSrc2Cpg()
    val config = getConfig()
      .fold(Config())(_.asInstanceOf[Config])
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    swiftsrc2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithSwiftSrc(val fileSuffix: String)
    extends DefaultTestCpg
    with SwiftSrc2CpgFrontend
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

}

class SwiftSrc2CpgSuite(
  fileSuffix: String = ".swift",
  withOssDataflow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty,
  withPostProcessing: Boolean = false
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithSwiftSrc(fileSuffix)
        .withOssDataflow(withOssDataflow)
        .withExtraFlows(extraFlows)
        .withPostProcessingPasses(withPostProcessing)
    )
    with Inside
