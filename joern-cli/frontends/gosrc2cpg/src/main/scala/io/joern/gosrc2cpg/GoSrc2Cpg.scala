package io.joern.gosrc2cpg

import better.files.File
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.gosrc2cpg.passes.{
  AstCreationPass,
  DownloadDependenciesPass,
  MethodAndTypeCacheBuilderPass,
  PackageCtorCreationPass
}
import io.joern.gosrc2cpg.utils.AstGenRunner
import io.joern.gosrc2cpg.utils.AstGenRunner.GoAstGenRunnerResult
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages

import java.nio.file.Paths
import scala.util.Try

class GoSrc2Cpg extends X2CpgFrontend[Config] {
  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("gosrc2cpgOut") { tmpDir =>
        val goGlobal = GoGlobal()
        new MetaDataPass(cpg, Languages.GOLANG, config.inputPath).createAndApply()
        val astGenResult = new AstGenRunner(config).execute(tmpDir).asInstanceOf[GoAstGenRunnerResult]
        val goMod = new GoModHelper(
          Some(config),
          astGenResult.parsedModFile.flatMap(modFile => GoAstJsonParser.readModFile(Paths.get(modFile)).map(x => x))
        )
        if (config.fetchDependencies) {
          goGlobal.processingDependencies = true
          new DownloadDependenciesPass(goMod, goGlobal, config).process()
          goGlobal.processingDependencies = false
        }
        val astCreators =
          new MethodAndTypeCacheBuilderPass(Some(cpg), astGenResult.parsedFiles, config, goMod, goGlobal).process()
        new AstCreationPass(cpg, astCreators, report).createAndApply()
        if goGlobal.pkgLevelVarAndConstantAstMap.size() > 0 then
          new PackageCtorCreationPass(cpg, config, goGlobal).createAndApply()
        report.print()
      }
    }
  }
}
