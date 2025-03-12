package io.joern.x2cpg.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}

import java.nio.file.Paths
import scala.util.Properties.isWin
import scala.util.{Failure, Success}

class ExternalCommandTest extends AnyWordSpec with Matchers {

  private def cwd = FileUtil.currentWorkingDirectory.toString

  "ExternalCommand.run" should {
    "be able to run `ls` successfully" in {
      FileUtil.usingTemporaryDirectory("sample") { sourceDir =>
        val cmd = Seq("ls", sourceDir.toString)
        ExternalCommand.run(cmd, Option(sourceDir.toString)).toTry should be a Symbol("success")
      }
    }

    "report exit code and stdout/stderr for nonzero exit code" in {
      ExternalCommand.run(Seq("ls", "/does/not/exist"), Option(cwd)).toTry match {
        case result: Success[_] =>
          fail(s"expected failure, but got $result")
        case Failure(exception) =>
          exception.getMessage should include("Process exited with code")  // exit code `2` on linux, `1` on mac...
          exception.getMessage should include("No such file or directory") // again, different errors on mac and linux
      }
    }

    "report error for io exception (e.g. for nonexisting command)" in {
      ExternalCommand.run(Seq("/command/does/not/exist"), Option(cwd)).toTry match {
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
