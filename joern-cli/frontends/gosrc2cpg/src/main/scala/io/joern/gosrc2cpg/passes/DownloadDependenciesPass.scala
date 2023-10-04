package io.joern.gosrc2cpg.passes

import better.files.File
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.utils.AstGenRunner.getClass
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}

class DownloadDependenciesPass(parentGoMod: GoModHelper) {
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
            mod.dependencies.foreach(dependency => {
              val cmd = s"go get ${dependency.module}@${dependency.version}"
              ExternalCommand.run(cmd, prjDir) match
                case Success(_) => print(". ")
                case Failure(f) =>
                  logger.error(s"\t- command '${cmd}' failed", f)
            })
          case Failure(f) =>
            logger.error("\t- command 'go mod init joern.io/temp' failed", f)
      })
  }
}
