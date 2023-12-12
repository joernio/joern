package io.joern.csharpsrc2cpg.testfixtures

import io.joern.csharpsrc2cpg.{CSharpSrc2Cpg, Config}
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.x2cpg.{ValidationMode, X2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.io.File

class CSharpCode2CpgFixture(withPostProcessing: Boolean = false, withDataFlow: Boolean = false)
    extends Code2CpgFixture(() => new DefaultTestCpgWithCSharp(withPostProcessing, withDataFlow)) {

  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext()

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement: String, secondElement: Option[Integer]) =>
      (firstElement, secondElement.get)
    }

  protected def basicBoilerplate(
    contents: String = "Console.WriteLine(\"Hello, world!\");",
    namespace: String = "HelloWorld",
    className: String = "Program"
  ): String =
    s"""using System;
       |
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

class DefaultTestCpgWithCSharp(withPostProcessing: Boolean, withDataFlow: Boolean)
    extends DefaultTestCpg
    with CSharpFrontend {

  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)

    if (withPostProcessing) {
      CSharpSrc2Cpg.postProcessingPasses(this, config).foreach(_.createAndApply())
    }

    if (withDataFlow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }

}

trait CSharpFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".cs"

  implicit val config: Config =
    getConfig()
      .map(_.asInstanceOf[Config])
      .getOrElse(Config().withSchemaValidation(ValidationMode.Enabled))

  override def execute(sourceCodeFile: File): Cpg = {
    new CSharpSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath).get
  }

}
