package io.joern.x2cpg.passes.base

import better.files.File
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

class VersionControlPass(cpg: Cpg, inputDir: Option[String] = None) extends SimpleCpgPass(cpg) {

  private val log          = LoggerFactory.getLogger(classOf[VersionControlPass])
  private val supportedVCS = Map(".git" -> "GIT", ".svn" -> "SVN")

  override def run(dstGraph: DiffGraphBuilder): Unit =
    inputDir match {
      case Some(projectRoot) => findVersionControlSystem(projectRoot, dstGraph)
      case None              => log.debug("No input path provided, cannot look for version control system")
    }

  private def findVersionControlSystem(path: String, dstGraph: DiffGraphBuilder): Unit =
    File(path).list.filter(d => d.isDirectory && supportedVCS.contains(d.name)).collectFirst(d => d) match {
      case Some(vcsDir) if isVCSNativelyPresent(vcsDir, supportedVCS(vcsDir.name)) =>
        createVCSNode(vcsDir, supportedVCS(vcsDir.name), dstGraph)
      case _ => log.debug("No supported version control system found")
    }

  private def isVCSNativelyPresent(vcsDir: File, vcs: String): Boolean = {
    ExternalCommand.run(s"${vcs.toLowerCase} --help", vcsDir.parent.pathAsString, separateStdErr = true) match {
      case Failure(e) =>
        log.warn(s"Unable to start process for $vcs. Cannot to obtain version control information", e)
        false
      case Success(_) => true
    }
  }

  private def createVCSNode(vcsDir: File, vcs: String, dstGraph: DiffGraphBuilder): Unit = {
    val projectPath   = vcsDir.parent.pathAsString
    val remote        = getRemoteUrl(projectPath, vcs)
    val branchDetails = getRevIdAuthorNameRevMsg(projectPath, vcs)
    val changedFiles  = getListOfChangedFilesInLastRevision(projectPath, vcs)
    println(branchDetails)
  }

  private def getRemoteUrl(projectPath: String, vcs: String): Option[String] = {
    vcs match {
      case "GIT" =>
        ExternalCommand.run(s"git config --get remote.origin.url", projectPath, separateStdErr = true) match {
          case Failure(e) =>
            log.error("Unable to retrieve remote Git URL", e)
            None
          case Success(url) => Option(url.head)
        }
      case "SVN" => None
      case _     => None
    }
  }

  private def getRevIdAuthorNameRevMsg(projectPath: String, vcs: String): RevisionDetails =
    vcs match {
      case "GIT" =>
        val d = "_#_"
        ExternalCommand.run(s"git show -s --format='%D$d%H$d%an$d%s' | tee", projectPath, separateStdErr = true) match {
          case Failure(e) =>
            log.error("Unable to retrieve Git branch", e); RevisionDetails()
          case Success(msg) =>
            val msgParts = msg.head.split(d)
            if (msgParts.length != 4)
              RevisionDetails()
            else {
              val branch  = Try(msgParts(0).split("->")(1).split(",").head.strip()).getOrElse("")
              val id      = msgParts(1)
              val author  = msgParts(2)
              val message = msgParts(3)
              RevisionDetails(id, message, author, branch)
            }
        }
      case "SVN" => RevisionDetails()
      case _     => RevisionDetails()
    }

  private def getListOfChangedFilesInLastRevision(projectPath: String, vcs: String): Seq[String] = {
    vcs match {
      case "GIT" =>
        ExternalCommand.run("git diff --name-only HEAD HEAD~1", projectPath, separateStdErr = true) match {
          case Failure(e) =>
            log.error("Unable to retrieve Git branch", e); Seq()
          case Success(msg) => msg
        }
      case "SVN" => Seq()
      case _     => Seq()
    }
  }

  case class RevisionDetails(id: String = "", message: String = "", author: String = "", branch: String = "")

}
