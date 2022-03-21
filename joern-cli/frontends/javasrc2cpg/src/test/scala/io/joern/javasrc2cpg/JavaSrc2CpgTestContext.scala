package io.joern.javasrc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.querying.InferenceJarTests
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
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
        inputPaths = Set(writeCodeToFile(code).getAbsolutePath),
        outputPath = "",
        inferenceJarPaths = inferenceJarPaths
      )
      val cpg     = javaSrc2Cpg.createCpg(config)
      val context = new LayerCreatorContext(cpg.get)
      new Base().run(context)
      new TypeRelations().run(context)
      new ControlFlow().run(context)
      new CallGraph().run(context)
      if (runDataflow) {
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

  private def writeCodeToFile(code: String): File = {
    val tmpDir = Files.createTempDirectory("javasrc2cpgTest").toFile
    tmpDir.deleteOnExit()
    val codeFile = File.createTempFile("Test", ".java", tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(code); close() }
    tmpDir
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
