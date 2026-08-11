package io.joern.abap2cpg.parser

import io.joern.abap2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, SkippedFile}
import io.shiftleft.semanticcpg.utils.{ExternalCommand, ExternalCommandResult}

import java.nio.file.Path

object AbapAstGenRunner {
  private object astGenMetaData extends AstGenProgramMetaData(name = "abapgen", configPrefix = "abap2cpg")
}

class AbapAstGenRunner(config: Config) extends AstGenRunner(AbapAstGenRunner.astGenMetaData, config) {
  import AbapAstGenRunner.*

  // abapgen binaries use the standard naming from AstGenRunner base class:
  // abapgen-linux, abapgen-linux-arm, abapgen-macos, abapgen-macos-arm, abapgen-win.exe

  // abapgen has no --version flag, so always use the bundled binary
  override def hasCompatibleAstGenVersion(compatibleVersion: String, path: Option[String]): Boolean = false

  // abapgen writes `ERR <file>` to stdout for files it failed to parse (no reason text, no per-file success lines).
  override def skippedFiles(in: Path, runResult: ExternalCommandResult): List[SkippedFile] = {
    runResult.stdOut.collect { case s"ERR $rest" =>
      SkippedFile(rest.takeWhile(_ != ':').trim, "failed to parse")
    }.toList
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): ExternalCommandResult = {
    ExternalCommand.run(Seq(astGenCommand, in, out.toString))
  }
}
