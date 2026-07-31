package io.joern.rust2cpg.astgen

import io.joern.rust2cpg.Config
import io.joern.x2cpg.astgen.AstGenRunner
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData
import io.shiftleft.semanticcpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.Try

object RustAstGenRunner {
  private val logger = LoggerFactory.getLogger(getClass)

  private object astGenMetaData extends AstGenProgramMetaData(name = "rust_ast_gen", configPrefix = "rust2cpg")
}

class RustAstGenRunner(config: Config) extends AstGenRunner(RustAstGenRunner.astGenMetaData, config) {

  override def skippedFiles(in: Path, astGenOut: List[String]): List[String] = {
    // rust_ast_gen prints to stdout "Skipped: <full-file-path>" for files it failed to process.
    astGenOut.collect { case s"Skipped: $filePath" => filePath }
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]] = {
    val baseArgs       = Seq(astGenCommand, "-i", in, "-o", out.toString)
    val sysRootArgs    = if (config.noSysRoot) Seq("--no-sysroot") else Seq.empty[String]
    val resolveCfgArgs = if (config.noResolveCfg) Seq.empty[String] else Seq("--resolve-cfg")
    val args           = baseArgs ++ sysRootArgs ++ resolveCfgArgs
    val result         = ExternalCommand.run(args).logIfFailed()
    result.stdErr.foreach(RustAstGenRunner.logger.info)
    result.toTry
  }
}
