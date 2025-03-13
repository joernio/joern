package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.{GoModDependency, GoModHelper}
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.gosrc2cpg.utils.AstGenRunner
import io.joern.gosrc2cpg.utils.AstGenRunner.{GoAstGenRunnerResult, getClass}
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import java.io.File as JFile
import java.nio.file.Paths
import java.util.concurrent.LinkedBlockingQueue
import scala.util.{Failure, Success, Try}

class DownloadDependenciesPass(cpg: Cpg, parentGoMod: GoModHelper, goGlobal: GoGlobal, config: Config) {
  private val logger = LoggerFactory.getLogger(getClass)
  def process(): Unit = {
    val processor       = new DependencyProcessorQueue()
    val processorThread = new Thread(processor)
    processorThread.start()
    FileUtil.usingTemporaryDirectory("go-temp-download") { tmpDir =>
      val projDir = tmpDir.toString
      parentGoMod
        .getModMetaData()
        .foreach(mod => {
          ExternalCommand.run(Seq("go", "mod", "init", "joern.io/temp"), Option(projDir)).toTry match {
            case Success(_) =>
              mod.dependencies
                .filter(dep => dep.beingUsed)
                .map(dependency => {
                  val cmd     = Seq("go", "get", dependency.dependencyStr())
                  val results = ExternalCommand.run(cmd, Option(projDir)).toTry
                  results match {
                    case Success(_) =>
                      print(". ")
                      processor.queue.put(Some(dependency))
                    case Failure(f) =>
                      logger.error(s"\t- command '$cmd' failed", f)
                  }
                })
            case Failure(f) =>
              logger.error("\t- command 'go mod init joern.io/temp' failed", f)
          }
        })
    }
    processor.queue.put(None)
    processorThread.join()
  }

  private class DependencyProcessorQueue extends Runnable {
    val queue =
      new LinkedBlockingQueue[Option[GoModDependency]]()
    override def run(): Unit = {
      try {
        var terminate = false
        while (!terminate) {
          queue.take() match {
            case None =>
              logger.debug("Shutting down WriterThread")
              terminate = true
            case Some(dependencyStr) =>
              processDependency(dependencyStr)
          }
        }
      } catch {
        case exception: InterruptedException => logger.warn("Interrupted WriterThread", exception)
        case exc: Exception =>
          logger.error("error in writer thread, ", exc)
      }
    }

    private def processDependency(dependency: GoModDependency): Unit = {
      val gopath = Try(sys.env("GOPATH")).getOrElse(Seq(os.home, "go").mkString(JFile.separator))
      val dependencyLocation =
        (Seq(gopath, "pkg", "mod") ++ dependency.dependencyStr().split("/")).mkString(JFile.separator)
      FileUtil.usingTemporaryDirectory("godep") { astLocation =>
        val depConfig = Config()
          .withInputPath(dependencyLocation)
          .withIgnoredFilesRegex(config.ignoredFilesRegex.toString())
          .withIgnoredFiles(config.ignoredFiles.toList)
        val astGenResult = new AstGenRunner(depConfig, dependency.getIncludePackagesList())
          .executeForGo(astLocation)
          .headOption
          .getOrElse(GoAstGenRunnerResult())
        val goMod = new GoModHelper(
          Some(dependencyLocation),
          astGenResult.parsedModFile.flatMap(modFile => GoAstJsonParser.readModFile(Paths.get(modFile)).map(x => x))
        )
        DependencySrcProcessorPass(cpg, astGenResult.parsedFiles, depConfig, goMod, goGlobal, astLocation)
          .createAndApply()
      }
    }
  }
}
