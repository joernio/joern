package io.joern.rust2cpg.astgen

import io.joern.rust2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData
import io.joern.x2cpg.report.ProblemSink
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.Try

object RustAstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)

  private object astGenMetaData extends AstGenProgramMetaData(name = "rust_ast_gen", configPrefix = "rust2cpg")
}

class RustAstGenRunner(config: Config, problemSink: ProblemSink = ProblemSink.LogOnly)
    extends AstGenRunner(RustAstGenRunner.astGenMetaData, config, problemSink) {

  override def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    // rust_ast_gen prints to stdout "Skipped: <full-file-path>" for files it failed to process.
    astGenOut.collect { case s"Skipped: $filePath" =>
      reportParseProblem(RustAstGenRunner.logger, filePath, "Failed to parse")
      filePath
    }
  }

  override protected def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]] = {
    val baseArgs       = Seq(astGenCommand, "-i", in, "-o", out.toString)
    val sysRootArgs    = if (config.noSysRoot) Seq("--no-sysroot") else Seq.empty[String]
    val resolveCfgArgs = if (config.noResolveCfg) Seq.empty[String] else Seq("--resolve-cfg")
    val procMacroArgs  = if (config.expandProcMacros) Seq("--expand-proc-macros") else Seq.empty[String]
    val args           = baseArgs ++ sysRootArgs ++ resolveCfgArgs ++ procMacroArgs

    // See https://github.com/rust-lang/rust-analyzer/pull/21249.
    // Needed to support the bidirectional proc-macro server protocol.
    // TODO(rust_ast_gen): remove once the api allows setting it programmaticaly.
    val procMacroEnv = if (config.expandProcMacros) {
      Map("RUST_ANALYZER_USE_POSTCARD" -> "1")
    } else {
      Map.empty[String, String]
    }

    val result = ExternalCommand.run(args, extraEnv = procMacroEnv).logIfFailed()
    if (result.successful) {
      result.stdErr.foreach(RustAstGenRunner.logger.info)
    }
    result.toTry
  }
}
