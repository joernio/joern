package io.joern.javasrc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.x2cpg.X2Cpg.{applyDefaultOverlays, writeCodeToFile}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.io.{File, PrintWriter}
import java.nio.file.Files

class JavaSrc2CpgTestContext {
  private var code: String = ""
  private var buildResult  = Option.empty[Cpg]

  def buildCpg(runDataflow: Boolean, inferenceJarPaths: Set[String]): Cpg = {
    if (buildResult.isEmpty) {
      val javaSrc2Cpg = JavaSrc2Cpg()
      val config = Config(
        inputPath = writeCodeToFile(code, "javasrc2cpgTest", ".java").getAbsolutePath,
        outputPath = "",
        inferenceJarPaths = inferenceJarPaths,
        skipDependencyDownload = true
      )
      val cpg = javaSrc2Cpg.createCpg(config)
      applyDefaultOverlays(cpg.get)
      if (runDataflow) {
        val context = new LayerCreatorContext(cpg.get)
        val options = new OssDataFlowOptions()
        new OssDataFlow(options).run(context)
      }
      buildResult = Some(cpg.get)
    }
    buildResult.get
  }

  private def withSource(code: String): JavaSrc2CpgTestContext = {
    this.code = code
    this
  }

}

object JavaSrc2CpgTestContext {
  def buildCpg(code: String, inferenceJarPaths: Set[String] = Set.empty): Cpg = {
    new JavaSrc2CpgTestContext()
      .withSource(code)
      .buildCpg(runDataflow = false, inferenceJarPaths = inferenceJarPaths)
  }

  def buildCpgWithDataflow(code: String, inferenceJarPaths: Set[String] = Set.empty): Cpg = {
    new JavaSrc2CpgTestContext()
      .withSource(code)
      .buildCpg(runDataflow = true, inferenceJarPaths = inferenceJarPaths)
  }
}
