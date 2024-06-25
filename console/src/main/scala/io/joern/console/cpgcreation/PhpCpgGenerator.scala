package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.php2cpg
import io.joern.x2cpg.passes.frontend.{XTypeRecoveryConfig, XTypeStubsParser, XTypeStubsParserConfig}
import io.shiftleft.codepropertygraph.generated.Cpg
import scopt.OParser

import java.nio.file.Path
import scala.compiletime.uninitialized
import scala.util.Try

case class PhpCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path = if (isWin) rootPath.resolve("php2cpg.bat") else rootPath.resolve("php2cpg")
  private var typeRecoveryConfig: XTypeRecoveryConfig     = uninitialized
  private var setKnownTypesConfig: XTypeStubsParserConfig = uninitialized

  override def generate(inputPath: String, outputPath: String): Try[String] = {
    val cmdLineArgs = config.cmdLineParams.toSeq
    typeRecoveryConfig = XTypeRecoveryConfig.parse(cmdLineArgs)
    setKnownTypesConfig = OParser
      .parse(XTypeStubsParser.parserOptions2, cmdLineArgs, XTypeStubsParserConfig())
      .getOrElse(
        throw new RuntimeException(
          s"unable to parse XTypeStubsParserConfig from commandline arguments ${cmdLineArgs.mkString(" ")}"
        )
      )
    val arguments = List(inputPath) ++ Seq("-o", outputPath) ++ config.cmdLineParams
    runShellCommand(command.toString, arguments).map(_ => outputPath)
  }

  override def isAvailable: Boolean =
    command.toFile.exists

  override def isJvmBased = true

  override def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    php2cpg.postProcessingPasses(cpg, typeRecoveryConfig, setKnownTypesConfig).foreach(_.createAndApply())
    cpg
  }
}
