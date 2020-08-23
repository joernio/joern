package io.shiftleft.joern

import better.files.Dsl.mkdir
import better.files.File
import io.shiftleft.console.{ConsoleConfig, InstallConfig}
import io.shiftleft.console.testing.TestCpgGenerator
import io.shiftleft.joern.console.JoernConsole
import org.scalatest.{Matchers, WordSpec}

class TestJoernConsole(workspaceDir: String) extends JoernConsole {
  override def config = new ConsoleConfig(
    install = new InstallConfig(Map("SHIFTLEFT_CONSOLE_INSTALL_DIR" -> workspaceDir))
  )
  override val cpgGenerator = new TestCpgGenerator(config)
}

object ConsoleFixture {
  def apply(constructor: String => JoernConsole = { x =>
    new TestJoernConsole(x)
  })(fun: (JoernConsole, File) => Unit): Unit = {
    File.usingTemporaryDirectory("console") { workspaceDir =>
      File.usingTemporaryDirectory("console") { codeDir =>
        mkdir(codeDir / "dir1")
        mkdir(codeDir / "dir2")
        (codeDir / "dir1" / "foo.c")
          .write("int main(int argc, char **argv) { char *ptr = 0x1 + argv; return argc; }")
        (codeDir / "dir2" / "bar.c").write("int bar(int x) { return x; }")
        val console = constructor(workspaceDir.toString)
        fun(console, codeDir)
      }
    }
  }
}

class ConsoleTests extends WordSpec with Matchers {

  "run" should {
    "provide a human readable overview of overlay creators" in ConsoleFixture({ dir =>
      new TestJoernConsole(dir)
    }) { (console, codeDir) =>
      File.usingTemporaryFile("console") { myScript =>
        console.importCode(codeDir.toString)
        val cpg = console.cpg
        myScript.write(s"""
                            | if (!run.toString.contains("scpg") || run.toString.contains("semanticcpg"))
                            |     throw new RuntimeException
                            |""".stripMargin)
        console.CpgScriptRunner(cpg).runScript(myScript.toString)
      }
    }
  }

  "help" should {
    "allow getting long description via help object" in ConsoleFixture({ dir =>
      new TestJoernConsole(dir)
    }) { (console, codeDir) =>
      File.usingTemporaryFile("console") { myScript =>
        console.importCode(codeDir.toString)
        val cpg = console.cpg
        myScript.write(s"""
                            | if (help.cpg.toString.isEmpty)
                            |     throw new RuntimeException
                            |""".stripMargin)
        console.CpgScriptRunner(cpg).runScript(myScript.toString)
      }
    }
  }

}
