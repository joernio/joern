package io.joern.jimple2cpg

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers._

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.util.Using

class Jimple2CpgTestContext {
  private var code: String = ""
  private var buildResult  = Option.empty[Cpg]

  def buildCpg(runDataflow: Boolean): Cpg = {
    if (buildResult.isEmpty) {
      val jimple2Cpg                     = Jimple2Cpg()
      val inputPath                      = writeCodeToFile(code).getAbsolutePath
      implicit val defaultConfig: Config = Config()
      val cpg                            = jimple2Cpg.createCpg(inputPath).get
      applyDefaultOverlays(cpg)
      if (runDataflow) {
        val context = new LayerCreatorContext(cpg)
        val options = new OssDataFlowOptions()
        new OssDataFlow(options).run(context)
      }
      buildResult = Some(cpg)
    }
    buildResult.get
  }

  private def withSource(code: String): Jimple2CpgTestContext = {
    this.code = code
    this
  }

  private def writeCodeToFile(code: String): File = {
    val tmpDir = Files.createTempDirectory("jimple2cpgTest").toFile
    tmpDir.deleteOnExit()
    val codeFile = File.createTempFile("Test", ".java", tmpDir)
    codeFile.deleteOnExit()
    Using.resource(new PrintWriter(codeFile)) { pw => pw.write(code) }
    JimpleCodeToCpgFixture.compileJava(codeFile)
    tmpDir
  }
}

object Jimple2CpgTestContext {
  def buildCpg(code: String): Cpg = {
    new Jimple2CpgTestContext()
      .withSource(code)
      .buildCpg(runDataflow = false)
  }

  def buildCpgWithDataflow(code: String): Cpg = {
    new Jimple2CpgTestContext()
      .withSource(code)
      .buildCpg(runDataflow = true)
  }
}
