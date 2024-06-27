package io.joern.csharpsrc2cpg.testfixtures

import io.joern.csharpsrc2cpg.{CSharpSrc2Cpg, Config}
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.dataflowengineoss.testfixtures.{SemanticCpgTestFixture, SemanticTestCpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.joern.x2cpg.{ValidationMode, X2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.Inside

import java.io.File

class CSharpCode2CpgFixture(
  withPostProcessing: Boolean = false,
  withDataFlow: Boolean = false,
  extraFlows: List[FlowSemantic] = List.empty
) extends Code2CpgFixture(() =>
      new DefaultTestCpgWithCSharp()
        .withOssDataflow(withDataFlow)
        .withExtraFlows(extraFlows)
        .withPostProcessingPasses(withPostProcessing)
    )
    with SemanticCpgTestFixture(extraFlows)
    with Inside {

  implicit val resolver: ICallResolver = NoResolve

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement, secondElement) =>
      (firstElement, secondElement.get)
    }

  protected def basicBoilerplate(
    contents: String = "Console.WriteLine(\"Hello, world!\");",
    namespace: String = "HelloWorld",
    className: String = "Program",
    globalDeclarations: String = ""
  ): String =
    s"""using System;
       |$globalDeclarations
       |namespace $namespace
       |{
       |  class $className
       |  {
       |    static void Main(string[] args)
       |    {
       |      $contents
       |    }
       |  }
       |
       |}
       |""".stripMargin
}

class DefaultTestCpgWithCSharp extends DefaultTestCpg with CSharpFrontend with SemanticTestCpg {

  override def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }

  override def applyPostProcessingPasses(): Unit = {
    CSharpSrc2Cpg
      .postProcessingPasses(this, getConfig().map(_.asInstanceOf[Config]).getOrElse(defaultConfig))
      .foreach(_.createAndApply())
    super.applyPostProcessingPasses()
  }

}

trait CSharpFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".cs"

  implicit lazy val defaultConfig: Config =
    getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(Config().withSchemaValidation(ValidationMode.Enabled))

  override def execute(sourceCodeFile: File): Cpg = {
    new CSharpSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }

}
