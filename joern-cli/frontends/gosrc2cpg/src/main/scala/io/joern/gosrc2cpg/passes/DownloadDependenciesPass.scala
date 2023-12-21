package io.joern.gosrc2cpg.passes

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.gosrc2cpg.utils.AstGenRunner
import io.joern.gosrc2cpg.utils.AstGenRunner.{GoAstGenRunnerResult, getClass}
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.io.File as JFile
import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class DownloadDependenciesPass(parentGoMod: GoModHelper, goGlobal: GoGlobal, config: Config) {
  private val logger = LoggerFactory.getLogger(getClass)
  def process(): Unit = {
    File.usingTemporaryDirectory("go-temp-download") { tmpDir =>
      setupDummyProjectAndDownload(tmpDir.toString)
    }
  }

  private def setupDummyProjectAndDownload(prjDir: String): Unit = {
    parentGoMod
      .getModMetaData()
      .map(mod => {
        ExternalCommand.run("go mod init joern.io/temp", prjDir) match
          case Success(_) =>
            mod.dependencies
              .filter(dep => config.includeIndirectDependencies || !dep.indirect)
              .foreach(dependency => {
                val dependencyStr = s"${dependency.module}@${dependency.version}"
                val cmd           = s"go get $dependencyStr"
                ExternalCommand.run(cmd, prjDir) match
                  case Success(_) =>
                    print(". ")
                    processDependency(dependencyStr)
                  case Failure(f) =>
                    logger.error(s"\t- command '${cmd}' failed", f)
              })
          case Failure(f) =>
            logger.error("\t- command 'go mod init joern.io/temp' failed", f)
      })
  }

  private def processDependency(dependencyStr: String): Unit = {
    val gopath             = Try(sys.env("GOPATH")).getOrElse(Seq(os.home, "go").mkString(JFile.separator))
    val dependencyLocation = (Seq(gopath, "pkg", "mod") ++ dependencyStr.split("/")).mkString(JFile.separator)
    File.usingTemporaryDirectory("godep") { astLocation =>
      val config       = Config().withInputPath(dependencyLocation)
      val astGenResult = new AstGenRunner(config).execute(astLocation).asInstanceOf[GoAstGenRunnerResult]
      val goMod = new GoModHelper(
        Some(config),
        astGenResult.parsedModFile.flatMap(modFile => GoAstJsonParser.readModFile(Paths.get(modFile)).map(x => x))
      )
      new MethodAndTypeCacheBuilderPass(None, astGenResult.parsedFiles, config, goMod, goGlobal).process()
    }
  }
}
