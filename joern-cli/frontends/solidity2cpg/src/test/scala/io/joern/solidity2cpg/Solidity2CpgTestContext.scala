package io.joern.solidity2cpg

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.io.{File, PrintWriter}
import java.nio.file.Files

class Solidity2CpgTestContext {

  private var code: String = ""
  private var buildResult  = Option.empty[Cpg]

  def buildCpg(runDataflow: Boolean): Cpg = {
    if (buildResult.isEmpty) {
      val solidity2cpg = Solidity2Cpg()
      val cpg          = solidity2cpg.createCpg(writeCodeToFile(code).getAbsolutePath)
      val context      = new LayerCreatorContext(cpg)
      new Base().run(context)
      new TypeRelations().run(context)
      new ControlFlow().run(context)
      new CallGraph().run(context)
      if (runDataflow) {
        val options = new OssDataFlowOptions()
        new OssDataFlow(options).run(context)
      }
      buildResult = Some(cpg)
    }
    buildResult.get
  }

  private def withSource(code: String): Solidity2CpgTestContext = {
    this.code = code
    this
  }

  private def writeCodeToFile(code: String): File = {
    val tmpDir = Files.createTempDirectory("solidity2cpgTest").toFile
    tmpDir.deleteOnExit()
    val codeFile = File.createTempFile("Test", ".sol", tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(code); close() }
    tmpDir
  }
}

object Solidity2CpgTestContext {
  def buildCpg(code: String): Cpg = {
    new Solidity2CpgTestContext()
      .withSource(code)
      .buildCpg(runDataflow = false)
  }

  def buildCpgWithDataflow(code: String): Cpg = {
    new Solidity2CpgTestContext()
      .withSource(code)
      .buildCpg(runDataflow = true)
  }
}
