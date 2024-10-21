package io.joern.x2cpg.utils

import better.files.File
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Properties.isWin
import scala.util.{Failure, Success}

class ExternalCommandTest extends AnyWordSpec with Matchers {

  private def cwd = File.currentWorkingDirectory.pathAsString

  "ExternalCommand.run" should {
    "be able to run `ls` successfully" in {
      File.usingTemporaryDirectory("sample") { sourceDir =>
        val cmd = Seq("ls", sourceDir.pathAsString)
        ExternalCommand.run(cmd, sourceDir.pathAsString).toTry should be a Symbol("success")
      }
    }

    "report exit code and stdout/stderr for nonzero exit code" in {
      ExternalCommand.run(Seq("ls", "/does/not/exist"), cwd).toTry match {
        case result: Success[_] =>
          fail(s"expected failure, but got $result")
        case Failure(exception) =>
          exception.getMessage should include("Process exited with code")  // exit code `2` on linux, `1` on mac...
          exception.getMessage should include("No such file or directory") // again, different errors on mac and linux
      }
    }

    "report error for io exception (e.g. for nonexisting command)" in {
      ExternalCommand.run(Seq("/command/does/not/exist"), cwd).toTry match {
        case result: Success[_] =>
          fail(s"expected failure, but got $result")
        case Failure(exception) =>
          exception.getMessage should include("""Cannot run program "/command/does/not/exist"""")
          if (isWin)
            exception.getMessage should include("The system cannot find the file")
          else
            exception.getMessage should include("No such file or directory")
      }
    }
  }

}
