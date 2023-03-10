package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.shiftleft.codepropertygraph.Cpg
import io.joern.pysrc2cpg.{
  DynamicTypeHintFullNamePass,
  ImportsPass,
  PythonNaiveCallLinker,
  PythonTypeHintCallLinker,
  PythonTypeRecovery
}

import java.nio.file.Path
import scala.util.Try

case class PythonSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("pysrc2cpg.bat") else rootPath.resolve("pysrc2cpg")

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    val arguments = Seq(inputPath, "-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    new ImportsPass(cpg).createAndApply()
    new DynamicTypeHintFullNamePass(cpg).createAndApply()
    new PythonTypeRecovery(cpg).createAndApply()
    new PythonTypeHintCallLinker(cpg).createAndApply()
    new PythonNaiveCallLinker(cpg).createAndApply()
    cpg
  }

  override def isJvmBased = true
}
