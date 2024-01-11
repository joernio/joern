package io.joern.c2cpg.testfixtures

import better.files.File
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.{C2Cpg, Config}
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg

trait C2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile("c2cpg.bin")
    cpgOutFile.deleteOnExit()
    val c2cpg = new C2Cpg()

    val config = getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(Config(includeComments = true))
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)

    c2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithC(val fileSuffix: String) extends DefaultTestCpg with C2CpgFrontend with SemanticTestCpg {
  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }
}

class CCodeToCpgSuite(
  fileSuffix: String = FileDefaults.C_EXT,
  withOssDataflow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithC(fileSuffix).withOssDataflow(withOssDataflow).withExtraFlows(extraFlows)
    )
    with SemanticCpgTestFixture(extraFlows)
