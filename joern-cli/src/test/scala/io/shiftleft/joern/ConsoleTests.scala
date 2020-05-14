package io.shiftleft.joern

import better.files.File
import io.shiftleft.console.{ConsoleConfig, InstallConfig}
import io.shiftleft.console.testing.{ConsoleFixture, TestCpgGenerator}
import io.shiftleft.joern.console.JoernConsole
import org.scalatest.{Matchers, WordSpec}

class TestJoernConsole(workspaceDir: String) extends JoernConsole {
  override def config = new ConsoleConfig(
    install = new InstallConfig(Map("SHIFTLEFT_CONSOLE_INSTALL_DIR" -> workspaceDir))
  )
  override val cpgGenerator = new TestCpgGenerator(config)
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
