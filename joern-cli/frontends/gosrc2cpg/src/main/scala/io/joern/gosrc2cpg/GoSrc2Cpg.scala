package io.joern.gosrc2cpg

import better.files.File
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.gosrc2cpg.passes.*
import io.joern.gosrc2cpg.utils.AstGenRunner
import io.joern.gosrc2cpg.utils.AstGenRunner.GoAstGenRunnerResult
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.{Report, FileUtil}
import io.joern.x2cpg.utils.FileUtil.*

import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}

import java.nio.file.Paths
import scala.util.Try

class GoSrc2Cpg(goGlobalOption: Option[GoGlobal] = Option(GoGlobal())) extends X2CpgFrontend[Config] {
  private val report: Report = new Report()

  private var goMod: Option[GoModHelper] = None
  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      FileUtil.usingTemporaryDirectory("gosrc2cpgOut") { tmpDir =>
        MetaDataPass(cpg, Languages.GOLANG, config.inputPath).createAndApply()
        val astGenResults = new AstGenRunner(config).executeForGo(tmpDir)
        astGenResults.foreach(astGenResult => {
          goGlobalOption
            .orElse(Option(GoGlobal()))
            .foreach(goGlobal => {
              goMod = Some(
                GoModHelper(
                  Some(astGenResult.modulePath),
                  astGenResult.parsedModFile
                    .flatMap(modFile => GoAstJsonParser.readModFile(Paths.get(modFile)).map(x => x))
                )
              )
              goGlobal.mainModule = goMod.flatMap(modHelper => modHelper.getModMetaData().map(mod => mod.module.name))
              InitialMainSrcPass(cpg, astGenResult.parsedFiles, config, goMod.get, goGlobal, tmpDir).createAndApply()
              if goGlobal.pkgLevelVarAndConstantAstMap.size() > 0 then
                PackageCtorCreationPass(cpg, config, goGlobal).createAndApply()
              if (config.fetchDependencies) {
                goGlobal.processingDependencies = true
                DownloadDependenciesPass(cpg, goMod.get, goGlobal, config).process()
                goGlobal.processingDependencies = false
              }
              AstCreationPass(cpg, astGenResult.parsedFiles, config, goMod.get, goGlobal, tmpDir, report)
                .createAndApply()
              report.print()
            })
        })
      }
    }
  }

  def getGoModHelper: GoModHelper = goMod.get
}
