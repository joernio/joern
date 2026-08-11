package io.joern.rust2cpg.astgen

import io.joern.rust2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.astgen.AstGenRunner.{AstGenProgramMetaData, SkippedFile}
import io.shiftleft.semanticcpg.utils.{ExternalCommand, ExternalCommandResult}
import org.slf4j.LoggerFactory

import java.nio.file.Path

object RustAstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)

  private object astGenMetaData extends AstGenProgramMetaData(name = "rust_ast_gen", configPrefix = "rust2cpg")
}

class RustAstGenRunner(config: Config) extends AstGenRunner(RustAstGenRunner.astGenMetaData, config) {

  // rust_ast_gen prints to stdout "Skipped: <full-file-path>" for files it excluded (e.g. out-of-crate); no reason
  // text is provided. Progress/fatal-error messages go to stderr.
  override def skippedFiles(in: Path, runResult: ExternalCommandResult): List[SkippedFile] = {
    runResult.stdOut.collect { case s"Skipped: $filePath" =>
      SkippedFile(filePath, "not part of the crate/workspace")
    }.toList
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): ExternalCommandResult = {
    val baseArgs       = Seq(astGenCommand, "-i", in, "-o", out.toString)
    val sysRootArgs    = if (config.noSysRoot) Seq("--no-sysroot") else Seq.empty[String]
    val resolveCfgArgs = if (config.noResolveCfg) Seq.empty[String] else Seq("--resolve-cfg")
    val args           = baseArgs ++ sysRootArgs ++ resolveCfgArgs
    val result         = ExternalCommand.run(args)
    if (isSuccess(result)) {
      result.stdErr.foreach(RustAstGenRunner.logger.info)
    }
    result
  }
}
