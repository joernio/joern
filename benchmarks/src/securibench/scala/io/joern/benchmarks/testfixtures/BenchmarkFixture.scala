package io.joern.benchmarks.testfixtures

import io.joern.console.cpgcreation.guessLanguage
import io.joern.benchmarks.BenchmarkTags._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.joern.javasrc2cpg.{JavaSrc2Cpg, Config => JavaSrcConfig}
import io.joern.jimple2cpg.{Jimple2Cpg, Config => JimpleConfig}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import overflowdb.traversal.Traversal

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

  /** Makes sure there are flows between the source and the sink
    */
  def assertIsInsecure(source: Traversal[CfgNode], sink: Traversal[CfgNode]): Assertion =
    if (sink.reachableBy(source).isEmpty) {
      fail("[False Negative] Source was not found to taint the sink")
    } else {
      succeed
    }

  /** Makes sure there are no flows between the source and the sink.
    */
  def assertIsSecure(source: Traversal[CfgNode], sink: Traversal[CfgNode]): Assertion =
    if (sink.reachableBy(source).nonEmpty) {
      fail("[False positive] Source was found to taint the sink")
    } else {
      succeed
    }

  override protected def withFixture(test: NoArgTest): Outcome = {
    val outcome = super.withFixture(test)
    val idxToInc = outcome match {
      case Failed(_) =>
        val falseNegative = test.text.contains("insecure")
        if (falseNegative)
          Some(FN)
        else
          Some(FP)
      case Succeeded =>
        val truePositive = test.text.contains("insecure")
        if (truePositive)
          Some(TP)
        else
          Some(TN)
      case _ => None
    }

    test.tags.foreach { tag =>
      val arr: Array[Int] = confusionMatrix(tag)
      idxToInc match {
        case Some(idx) => arr(idx) += 1
        case None      =>
      }
    }

    outcome
  }

  override def afterAll(): Unit = {
    cpg.close()
  }

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
    val cpgPath = java.io.File.createTempFile("benchmark", ".odb").getAbsolutePath
    val cpg = (guessLanguage(inputPath) match {
      case Some(language: String) =>
        language match {
          case Languages.JAVASRC => JavaSrc2Cpg().createCpg(JavaSrcConfig(Set(inputPath), cpgPath))
          case Languages.JAVA    => Jimple2Cpg().createCpg(JimpleConfig(Set(inputPath), cpgPath))
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
