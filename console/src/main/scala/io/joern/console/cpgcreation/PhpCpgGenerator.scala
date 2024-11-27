package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.php2cpg
import io.joern.x2cpg.passes.frontend.{XTypeRecoveryConfig, XTypeStubsParser, XTypeStubsParserConfig}
import io.shiftleft.codepropertygraph.generated.Cpg
import scopt.OParser

import java.nio.file.Path
import scala.util.Try

case class PhpCpgGenerator(config: FrontendConfig, rootPath: Path) extends CpgGenerator {
  private lazy val command: Path      = if (isWin) rootPath.resolve("php2cpg.bat") else rootPath.resolve("php2cpg")
  private lazy val cmdLineArgs        = config.cmdLineParams.toSeq
  private lazy val typeRecoveryConfig = XTypeRecoveryConfig.parse(cmdLineArgs)
  private lazy val setKnownTypesConfig: XTypeStubsParserConfig = {
    OParser
      .parse(XTypeStubsParser.parserOptions2, cmdLineArgs, XTypeStubsParserConfig())
      .getOrElse(
        throw new RuntimeException(
          s"unable to parse XTypeStubsParserConfig from commandline arguments ${cmdLineArgs.mkString(" ")}"
        )
      )
  }

  override def generate(inputPath: String, outputPath: String): Try[String] = {
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
