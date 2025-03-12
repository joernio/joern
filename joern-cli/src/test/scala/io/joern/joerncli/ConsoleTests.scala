package io.joern.joerncli

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class ConsoleTests extends AnyWordSpec with Matchers {

  if (scala.util.Properties.isWin) {
    info(
      "console tests don't work on windows - not sure why... running the console manually works though: try the `run` and `help.cpg` commands in joern"
    )
  } else {
    "run" should {
      "provide a human readable overview of overlay creators" in withTestCode { codeDir =>
        RunScriptTests.exec("general/run.sc", codeDir.toString)
      }
    }

    "help" should {
      "allow getting long description via help object" in withTestCode { codeDir =>
        RunScriptTests.exec("general/help.sc", codeDir.toString)
      }
    }
  }

  def withTestCode(fun: Path => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("console") { workspaceDir =>
      FileUtil.usingTemporaryDirectory("console") { codeDir =>
        Files.createDirectory(codeDir / "dir1")
        Files.createDirectory(codeDir / "dir2")

        val fooDir     = (codeDir / "dir1" / "foo.c")
        val fooContent = "int main(int argc, char **argv) { char *ptr = 0x1 + argv; return argc; }"

        Files.writeString(fooDir, fooContent)

        val barDir     = (codeDir / "dir2" / "bar.c")
        val barContent = "int bar(int x) { return x; }"

        Files.writeString(barDir, barContent)

        fun(codeDir)
      }
    }
  }
}
