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
import java.util.concurrent.LinkedBlockingQueue
import scala.util.{Failure, Success, Try}

class DownloadDependenciesPass(parentGoMod: GoModHelper, goGlobal: GoGlobal, config: Config) {
  private val logger = LoggerFactory.getLogger(getClass)
  def process(): Unit = {
    val writer       = new Writer()
    val writerThread = new Thread(writer)
    writerThread.start()
    File.usingTemporaryDirectory("go-temp-download") { tmpDir =>
      val projDir = tmpDir.pathAsString
      parentGoMod
        .getModMetaData()
        .foreach(mod => {
          ExternalCommand.run("go mod init joern.io/temp", projDir) match {
            case Success(_) =>
              mod.dependencies
                .filter(dep => dep.beingUsed)
                .map(dependency => {
                  val dependencyStr = s"${dependency.module}@${dependency.version}"
                  val cmd           = s"go get $dependencyStr"
                  val results       = ExternalCommand.run(cmd, projDir)
                  results match {
                    case Success(_) =>
                      print(". ")
                      writer.queue.put(Some(dependencyStr))
                    case Failure(f) =>
                      logger.error(s"\t- command '$cmd' failed", f)
                  }
                })
            case Failure(f) =>
              logger.error("\t- command 'go mod init joern.io/temp' failed", f)
          }
        })
    }
    writer.queue.put(None)
    writerThread.join()
  }

  private class Writer() extends Runnable {
    val queue =
      new LinkedBlockingQueue[Option[String]]()
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

    private def processDependency(dependencyStr: String): Unit = {
      val gopath             = Try(sys.env("GOPATH")).getOrElse(Seq(os.home, "go").mkString(JFile.separator))
      val dependencyLocation = (Seq(gopath, "pkg", "mod") ++ dependencyStr.split("/")).mkString(JFile.separator)
      File.usingTemporaryDirectory("godep") { astLocation =>
        val depConfig = Config()
          .withInputPath(dependencyLocation)
          .withIgnoredFilesRegex(config.ignoredFilesRegex.toString())
          .withIgnoredFiles(config.ignoredFiles.toList)
        // TODO: Need to implement mechanism to filter and process only used namespaces(folders) of the dependency.
        // In order to achieve this filtering, we need to add support for inclusive rule with goastgen utility first.
        val astGenResult = new AstGenRunner(depConfig).execute(astLocation).asInstanceOf[GoAstGenRunnerResult]
        val goMod = new GoModHelper(
          Some(depConfig),
          astGenResult.parsedModFile.flatMap(modFile => GoAstJsonParser.readModFile(Paths.get(modFile)).map(x => x))
        )
        new MethodAndTypeCacheBuilderPass(None, astGenResult.parsedFiles, depConfig, goMod, goGlobal).process()
      }
    }
  }
}
