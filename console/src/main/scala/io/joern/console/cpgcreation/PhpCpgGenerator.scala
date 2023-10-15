package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.php2cpg.Main
import io.joern.php2cpg.passes.{PhpTypeRecoveryPass, PhpSetKnownTypesPass}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.Cpg

import java.nio.file.Path
import scala.util.Try

case class PhpCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("php2cpg.bat") else rootPath.resolve("php2cpg")

  override def generate(inputPath: String, outputPath: String): Try[String] = {
    val arguments = List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def isJvmBased = true

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    new PhpSetKnownTypesPass(cpg).createAndApply()
    new PhpTypeRecoveryPass(cpg, XTypeRecoveryConfig()).createAndApply()

    cpg
  }
}
