package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.pysrc2cpg.*
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path
import scala.util.Try

case class PythonSrcCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("pysrc2cpg.bat") else rootPath.resolve("pysrc2cpg")
  private var pyConfig: Option[Py2CpgOnFileSystemConfig] = None

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    */
  override def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String] = {
    val arguments = Seq(inputPath, "-o", outputPath) ++ config.cmdLineParams
    pyConfig = X2Cpg.parseCommandLine(arguments.toArray, NewMain.getCmdLineParser, Py2CpgOnFileSystemConfig())
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    new ImportsPass(cpg).createAndApply()
    new PythonImportResolverPass(cpg).createAndApply()
    new DynamicTypeHintFullNamePass(cpg).createAndApply()
    new PythonInheritanceNamePass(cpg).createAndApply()
    val typeRecoveryConfig = pyConfig match
      case Some(config) => XTypeRecoveryConfig(config.typePropagationIterations, !config.disableDummyTypes)
      case None         => XTypeRecoveryConfig()
    new PythonTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate().foreach(_.createAndApply())
    new PythonTypeHintCallLinker(cpg).createAndApply()
    new NaiveCallLinker(cpg).createAndApply()

    // Some of passes above create new methods, so, we
    // need to run the ASTLinkerPass one more time
    new AstLinkerPass(cpg).createAndApply()

    cpg
  }

  override def isJvmBased = true
}
