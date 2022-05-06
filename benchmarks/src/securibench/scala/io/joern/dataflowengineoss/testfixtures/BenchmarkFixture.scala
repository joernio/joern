package io.joern.dataflowengineoss.testfixtures

import io.joern.console.cpgcreation.guessLanguage
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.joern.javasrc2cpg.{JavaSrc2Cpg, Config => JavaSrcConfig}
import io.joern.jimple2cpg.{Jimple2Cpg, Config => JimpleConfig}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Failure

class BenchmarkFixture extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  val semanticsFile: String = ProjectRoot.relativise("dataflowengineoss/src/test/resources/default.semantics")
  lazy val defaultSemantics: Semantics           = Semantics.fromList(new Parser().parseFile(semanticsFile))
  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext(defaultSemantics, EngineConfig(maxCallDepth = 4))

  val benchmarkName: String = ""
  val pkg: String           = ""
  val category: String      = ""
  val benchmarkNo: String   = "1"
  val fileExt: String       = ""

  lazy val cpg: Cpg = BenchmarkCpgContext.buildCpg(ProjectRoot.relativise(constructTargetFilePath))

  def constructTargetFilePath: String =
    s"benchmarks/src/$benchmarkName/resources/$pkg/$category/${category.capitalize}$benchmarkNo$fileExt"

}

object BenchmarkCpgContext {
  def buildCpg(codePath: String): Cpg = {
    new BenchmarkCpgContext()
      .withSource(codePath)
      .buildCpg()
  }
}

class BenchmarkCpgContext {
  private var inputPath: String = ""

  def buildCpg(): Cpg = {
    val cpg = (guessLanguage(inputPath) match {
      case Some(language: String) =>
        language match {
          case Languages.JAVASRC => JavaSrc2Cpg().createCpg(JavaSrcConfig(inputPaths = Set(inputPath)))
          case Languages.JAVA    => Jimple2Cpg().createCpg(JimpleConfig(inputPaths = Set(inputPath)))
          case _ => Failure(new RuntimeException(s"No supported language frontend for the benchmark at '$inputPath'"))
        }
      case None =>
        Failure(
          new RuntimeException(s"Unable to guess which language frontend to use to parse the benchmark at '$inputPath'")
        )
    })
    applyDefaultOverlays(cpg.get)
    val context = new LayerCreatorContext(cpg.get)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    cpg.get
  }

  private def withSource(codePath: String): BenchmarkCpgContext = {
    this.inputPath = codePath
    this
  }
}
