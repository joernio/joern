package io.joern.rust2cpg

import io.joern.rust2cpg.astgen.RustAstGenRunner
import io.joern.rust2cpg.passes.{AstCreationPass, RustConfigFileCreationPass, RustTypeNodePass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.HashUtil
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.Paths
import scala.util.Try

class Rust2Cpg extends X2CpgFrontend {
  override type ConfigType = Config
  override val defaultConfig: Config = Config()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      FileUtil.usingTemporaryDirectory("rust2cpgOut") { tmpDir =>
        val astGenResult = new RustAstGenRunner(config).execute(tmpDir)
        if (astGenResult.generatorFailed) {
          // Without this the frontend writes a CPG holding zero methods and exits 0, so a caller
          // that checks the exit code is told the parse succeeded. Every downstream query then
          // reports that the code contains nothing, which is indistinguishable from a clean result.
          throw new RuntimeException(
            s"rust_ast_gen failed to run on '${config.inputPath}'; see the logged error above. " +
              "Refusing to write an empty CPG."
          )
        }
        val hash = HashUtil.sha256(astGenResult.parsedFiles.map(Paths.get(_)))
        new MetaDataPass(cpg, Languages.RUST, config.inputPath, Option(hash)).createAndApply()
        new AstCreationPass(cpg, astGenResult.parsedFiles, config)(config.schemaValidation).createAndApply()
        new RustTypeNodePass(cpg).createAndApply()
        new RustConfigFileCreationPass(cpg, config).createAndApply()
      }
    }
  }
}
