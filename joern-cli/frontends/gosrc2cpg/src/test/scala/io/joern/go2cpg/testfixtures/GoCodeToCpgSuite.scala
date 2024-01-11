package io.joern.go2cpg.testfixtures

import better.files.File
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import org.scalatest.Inside
trait Go2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile("go2cpg.bin")
    cpgOutFile.deleteOnExit()
    val go2cpg = new GoSrc2Cpg()
    val config = getConfig()
      .collectFirst { case x: Config => x }
      .getOrElse(Config())
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    go2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithGo(val fileSuffix: String) extends DefaultTestCpg with Go2CpgFrontend with SemanticTestCpg {
  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }
}

class GoCodeToCpgSuite(
  fileSuffix: String = ".go",
  withOssDataflow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithGo(fileSuffix).withOssDataflow(withOssDataflow).withExtraFlows(extraFlows)
    )
    with SemanticCpgTestFixture(extraFlows)
    with Inside {
  implicit val resolver: ICallResolver = NoResolve

}
