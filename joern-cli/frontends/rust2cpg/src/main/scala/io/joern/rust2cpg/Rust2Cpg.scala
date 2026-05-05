package io.joern.rust2cpg

import io.joern.rust2cpg.astgen.RustAstGenRunner
import io.joern.rust2cpg.passes.AstCreationPass
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
        val hash         = HashUtil.sha256(astGenResult.parsedFiles.map(Paths.get(_)))
        new MetaDataPass(cpg, Languages.RUST, config.inputPath, Option(hash)).createAndApply()
        new AstCreationPass(cpg, astGenResult.parsedFiles, config)(config.schemaValidation).createAndApply()
      }
    }
  }
}
