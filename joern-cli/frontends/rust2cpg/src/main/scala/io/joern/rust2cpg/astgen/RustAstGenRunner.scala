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
    // 1. rust_ast_gen does not support skipping files yet.
    // 2. output is controlled by RUST_LOG which can be noisy.
    // TODO: (on rust_ast_gen side) explicitly print success/failure of each file
    List.empty
  }

  override def runAstGenNative(in: String, out: Path, exclude: String, include: String): Try[Seq[String]] = {
    ExternalCommand
      .run(Seq(astGenCommand, "-i", in, "-o", out.toString))
      .toTry
  }
}
