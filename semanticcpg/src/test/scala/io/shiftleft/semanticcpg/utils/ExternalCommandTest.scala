package io.shiftleft.semanticcpg.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.Properties.isWin
import scala.util.{Failure, Success}
import concurrent.duration.*

class ExternalCommandTest extends AnyWordSpec with Matchers {
  "ExternalCommand.run" should {
    "be able to run `ls` successfully" in {
      FileUtil.usingTemporaryDirectory("sample") { sourceDir =>
        val cmd = Seq("ls", sourceDir.toString)
        ExternalCommand.run(cmd, Option(sourceDir)).toTry should be a Symbol("success")
      }
    }

    "report exit code and stdout/stderr for nonzero exit code" in {
      val result = ExternalCommand.run(Seq("ls", "/does/not/exist"))
      if (result.successful)
        fail(s"expected failure, but got $result")
      else
        result.toTry.failed.get.getMessage should include("No such file or directory")
    }

    "report error for io exception (e.g. for nonexisting command)" in {
      ExternalCommand.run(Seq("/command/does/not/exist")).toTry match {
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

    "support a timeout" in {
      ExternalCommand.run(Seq("sleep", "5"), timeout = 100.millis).toTry match {
        case result: Success[_] =>
          fail(s"expected failure, but got $result")
        case Failure(exception) =>
          exception.getMessage should include("has timed out")
      }

    }
  }

}
