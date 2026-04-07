package io.joern.abap2cpg.parser

import io.joern.abap2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

object AbapAstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)

  private object astGenMetaData extends AstGenProgramMetaData(name = "abapgen", configPrefix = "abap2cpg")
}

class AbapAstGenRunner(config: Config) extends AstGenRunner(AbapAstGenRunner.astGenMetaData, config) {
  import AbapAstGenRunner.*

  // abapgen binaries use the standard naming from AstGenRunner base class:
  // abapgen-linux, abapgen-linux-arm, abapgen-macos, abapgen-macos-arm, abapgen-win.exe

  // abapgen has no --version flag, so always use the bundled binary
  override def hasCompatibleAstGenVersion(compatibleVersion: String): Boolean = false

  override def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    astGenOut.collect {
      case line if line.startsWith("ERR ") =>
        val filename = line.stripPrefix("ERR ").takeWhile(_ != ':')
        logger.warn(s"\t- failed to parse '$filename'")
        Some(filename)
      case line =>
        logger.debug(s"\t+ $line")
        None
    }.flatten
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]] = {
    ExternalCommand.run(Seq(astGenCommand, in, out.toString)).toTry
  }
}
