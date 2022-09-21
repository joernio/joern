package io.joern.jssrc2cpg

import better.files.File
import io.joern.jssrc2cpg.passes.{AstCreationPass, BuiltinTypesPass, JsMetaDataPass, TypeNodePass}
import io.joern.jssrc2cpg.passes.ConfigPass
import io.joern.jssrc2cpg.passes.DependenciesPass
import io.joern.jssrc2cpg.passes.PrivateKeyFilePass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.jssrc2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.utils.HashUtil
import io.joern.x2cpg.SourceFiles

import java.nio.file.Path
import scala.util.Try

class JsSrc2Cpg extends X2CpgFrontend[Config] {

  private val report: Report = new Report()

  private def configFiles(config: Config, extensions: Set[String]): Seq[File] =
    SourceFiles.determine(config.inputPath, extensions).map(File(_))

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("jssrc2cpgOut") { tmpDir =>
        val astgenResult = AstGenRunner.execute(config, tmpDir)
        val hash         = HashUtil.sha256(astgenResult.parsedFiles.map { case (_, file) => Path.of(file) })

        val astCreationPass = new AstCreationPass(cpg, astgenResult, config, report)
        astCreationPass.createAndApply()

        new TypeNodePass(astCreationPass.allUsedTypes(), cpg).createAndApply()
        new JsMetaDataPass(cpg, hash, config.inputPath).createAndApply()
        new BuiltinTypesPass(cpg).createAndApply()
        new DependenciesPass(cpg, config).createAndApply()
        new ConfigPass(
          cpg,
          configFiles(config, Set(".json", ".config.js", ".conf.js", ".vue", ".html")),
          config,
          report
        ).createAndApply()
        new PrivateKeyFilePass(cpg, configFiles(config, Set(".key")), config, report).createAndApply()

        report.print()
      }
    }
  }

}
