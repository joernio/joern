package io.joern.x2cpg.passes

import better.files.File
import io.joern.x2cpg.passes.base.{NamespaceCreator, VersionControlPass}
import io.joern.x2cpg.testfixtures.EmptyGraphFixture
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}

class VersionControlPassTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private val log              = LoggerFactory.getLogger(classOf[VersionControlPassTests])
  private val joernWebsiteRepo = "https://github.com/joernio/website.git"
  private val tmpDir           = File.newTemporaryDirectory("joern-vcs-tests")

  override def afterAll(): Unit = {
    super.afterAll()

    def deleteRecursively(file: File): Unit = {
      if (file.isDirectory) {
        file.list.foreach(deleteRecursively)
      }
      file.delete(swallowIOExceptions = true, File.LinkOptions.noFollow)
    }
    deleteRecursively(tmpDir)
  }

  "should detect a Git project's details" in EmptyGraphFixture { graph =>
    ExternalCommand.run("git --help", tmpDir.pathAsString) match {
      case Failure(e) => log.warn("Error running `git` during VCS testing, skipping...", e)
      case Success(_) => log.info("Git is present on the system, continuing with Git VCS tests")
    }

    ExternalCommand.run(s"git clone $joernWebsiteRepo ${tmpDir.pathAsString}", tmpDir.parent.pathAsString, separateStdErr = true) match {
      case Failure(e) => log.warn("Error pulling Git repository during VCS testing, skipping...", e)
      case Success(_) => log.info("Test repository successfully pulled")
    }

    val cpg = new Cpg(graph)

    val vcsPass = new VersionControlPass(cpg, Option(tmpDir.pathAsString))
    vcsPass.createAndApply()
  }

}
