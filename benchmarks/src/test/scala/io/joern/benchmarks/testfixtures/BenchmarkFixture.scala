package io.joern.benchmarks.testfixtures

import io.joern.benchmarks.BenchmarkTags._
import io.joern.console.cpgcreation.guessLanguage
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.javasrc2cpg.{JavaSrc2Cpg, Config => JavaSrcConfig}
import io.joern.jimple2cpg.{Jimple2Cpg, Config => JimpleConfig}
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Failure

abstract class BenchmarkFixture(
  val pkg: String = "",
  val category: String = "",
  val benchmarkNo: String = "1",
  val fileExt: String = ""
) extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterAll {

  lazy val defaultSemantics: Semantics           = DefaultSemantics()
  implicit val resolver: ICallResolver           = NoResolve
  implicit lazy val engineContext: EngineContext = EngineContext(defaultSemantics)

  private lazy val targetFiles = getListOfFiles(ProjectRoot.relativise(constructTargetFilePath))
  private lazy val targetDir   = moveToTempDir(targetFiles)
  lazy val cpg: Cpg            = BenchmarkCpgContext.buildCpg(targetDir)

  def constructTargetFilePath: String =
    s"benchmarks/src/test/resources/$pkg/${category.toLowerCase}"

  protected def getListOfFiles(dir: String): List[java.io.File] = {
    val d = new java.io.File(dir)
    // Regex is useful for class files containing subclasses
    val regex = s".*$category$benchmarkNo(?:\\$$[A-Za-z]*)?$fileExt"
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(f => f.isFile && f.getAbsolutePath.matches(regex)).toList
    } else {
      List.empty
    }
  }

  private def moveToTempDir(files: List[java.io.File]): String = {
    val tgt = java.nio.file.Files.createTempDirectory("benchmarks")
    files.foreach { f =>
      java.nio.file.Files.copy(f.toPath, java.nio.file.Paths.get(s"$tgt${java.io.File.separator}${f.getName}"))
    }
    tgt.toAbsolutePath.toString
  }

  /** Makes sure there are flows between the source and the sink
    */
  def assertIsInsecure(source: Iterator[CfgNode], sink: Iterator[CfgNode]): Assertion =
    if (sink.reachableBy(source).isEmpty) {
      fail("[False Negative] Source was not found to taint the sink")
    } else {
      succeed
    }

  /** Makes sure there are no flows between the source and the sink.
    */
  def assertIsSecure(source: Iterator[CfgNode], sink: Iterator[CfgNode]): Assertion =
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

object BenchmarkFixture {
  val JAVA_EXT       = ".java"
  val JVM_EXT        = ".class"
  val C_EXT          = ".c"
  val CPP_EXT        = ".cpp"
  val PYTHON_EXT     = ".py"
  val JAVASCRIPT_EXT = ".js"
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
    val cpg = guessLanguage(inputPath) match {
      case Some(language: String) =>
        language match {
          case Languages.JAVASRC =>
            JavaSrc2Cpg().createCpgWithOverlays(JavaSrcConfig().withInputPath(inputPath).withOutputPath(cpgPath))
          case Languages.JAVA =>
            Jimple2Cpg().createCpgWithOverlays(JimpleConfig().withInputPath(inputPath).withOutputPath(cpgPath))
          case _ => Failure(new RuntimeException(s"No supported language frontend for the benchmark at '$inputPath'"))
        }
      case None =>
        Failure(
          new RuntimeException(s"Unable to guess which language frontend to use to parse the benchmark at '$inputPath'")
        )
    }
    val context                          = new LayerCreatorContext(cpg.get)
    val options                          = new OssDataFlowOptions()
    lazy val defaultSemantics: Semantics = DefaultSemantics()
    new OssDataFlow(options)(defaultSemantics).run(context)
    cpg.get
  }

  private def withSource(codePath: String): BenchmarkCpgContext = {
    this.inputPath = codePath
    this
  }
}
