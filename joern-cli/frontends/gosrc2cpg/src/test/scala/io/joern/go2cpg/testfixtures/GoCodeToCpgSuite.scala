package io.joern.go2cpg.testfixtures

import better.files.File
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import org.scalatest.Inside
class DefaultTestCpgWithGo(val fileSuffix: String) extends DefaultTestCpg with SemanticTestCpg {

  private var goGlobal: Option[GoGlobal]   = None
  private var goSrc2Cpg: Option[GoSrc2Cpg] = None
  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  def withGoGlobal(goGlobal: GoGlobal): this.type = {
    setGoGlobal(goGlobal)
    this
  }

  private def setGoGlobal(goGlobal: GoGlobal): Unit = {
    if (this.goGlobal.isDefined) {
      throw new RuntimeException("Frontend GoGlobal may only be set once per test")
    }
    this.goGlobal = Some(goGlobal)
  }

  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile("go2cpg.bin")
    cpgOutFile.deleteOnExit()
    goSrc2Cpg = Some(new GoSrc2Cpg(this.goGlobal))
    val config = getConfig()
      .collectFirst { case x: Config => x }
      .getOrElse(Config())
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    goSrc2Cpg.get.createCpg(config).get
  }

  def getModHelper(): GoModHelper = goSrc2Cpg.get.getGoModHelper
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
