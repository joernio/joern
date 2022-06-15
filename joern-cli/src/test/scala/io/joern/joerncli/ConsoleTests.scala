package io.joern.joerncli

import better.files.Dsl.mkdir
import better.files.File
import io.joern.console.cpgcreation.ImportCode
import io.joern.console.testing.TestCpgGeneratorFactory
import io.joern.console.workspacehandling.Project
import io.joern.console.{Console, ConsoleConfig, InstallConfig}
import io.joern.joerncli.console.JoernConsole
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MyImportCode[T <: Project](console: Console[T]) extends ImportCode(console) {
  override val generatorFactory = new TestCpgGeneratorFactory(console.config)
}

class TestJoernConsole(workspaceDir: String) extends JoernConsole {
  override val config = new ConsoleConfig(
    install = new InstallConfig(Map("SHIFTLEFT_CONSOLE_INSTALL_DIR" -> workspaceDir))
  )
  override val importCode = new MyImportCode(this)
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

class ConsoleTests extends AnyWordSpec with Matchers {

  "run" should {
    "provide a human readable overview of overlay creators" in ConsoleFixture({ dir =>
      new TestJoernConsole(dir)
    }) { (console, codeDir) =>
      File.usingTemporaryFile("console") { myScript =>
        console.importCode(codeDir.toString)
        val cpg = console.cpg
        myScript.write(s"""
                            | if (!run.toString.contains("base"))
                            |     throw new RuntimeException("base layer not applied...?")
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
                            |     throw new RuntimeException("no help text available...?")
                            |""".stripMargin)
        console.CpgScriptRunner(cpg).runScript(myScript.toString)
      }
    }
  }

}
