package io.joern.swiftsrc2cpg

import io.joern.swiftsrc2cpg.passes.*
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.joern.x2cpg.report.ScanReport
import io.joern.x2cpg.utils.HashUtil
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.Paths
import scala.util.Try

class SwiftSrc2Cpg extends X2CpgFrontend {
  override type ConfigType = Config
  override val defaultConfig = Config()

  def createCpg(config: Config): Try[Cpg] = createCpg(config, new ScanReport())

  override def createCpg(config: Config, report: ScanReport): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      FileUtil.usingTemporaryDirectory("swiftsrc2cpgOut") { tmpDir =>
        val astGenResult = new AstGenRunner(config, report).execute(tmpDir)
        val hash         = HashUtil.sha256(astGenResult.parsedFiles.map(file => Paths.get(file)))

        val astCreationPass = new AstCreationPass(cpg, astGenResult, config, report)(config.schemaValidation)
        astCreationPass.createAndApply()

        new MetaDataPass(cpg, hash, config.inputPath).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
        SwiftTypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()

        new ConfigFileCreationPass(cpg, config, report).createAndApply()
        new DependenciesPass(cpg, report).createAndApply()

        val extensionsPass = new ExtensionsPass(
          cpg,
          astCreationPass.extensionMembers(),
          astCreationPass.extensionMethodFullNameMapping(),
          astCreationPass.memberPropertyMapping(),
          astCreationPass.extensionInherits()
        )
        extensionsPass.createAndApply()
        extensionsPass.setters.createAndApply()

        new ObjcCallFullNamePass(cpg).createAndApply()
        new ClosureBindingsPass(cpg).createAndApply()
        new FullNameUniquenessPass(cpg).createAndApply()
      }
    }
  }
}
