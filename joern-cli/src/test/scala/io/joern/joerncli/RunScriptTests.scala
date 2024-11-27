package io.joern.joerncli

import better.files._
import java.nio.file.Paths
import io.joern.console.Config
import io.joern.joerncli.console.ReplBridge
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RunScriptTests extends AnyWordSpec with Matchers {
  import RunScriptTests._

  if (scala.util.Properties.isWin) {
    info(
      "scripting tests don't work on windows - not sure why... running them manually works though, e.g. `joern --script joern-cli/src/main/resources/scripts/c/pointer-to-int.sc --param \"\"\"inputPath=joern-cli/src/test/resources/testcode/unsafe-ptr/unsafe-ptr.c\"\"\"`"
    )
  } else {
    Seq(
      ("c/pointer-to-int.sc", "unsafe-ptr"),
      ("c/syscalls.sc", "syscalls"),
      ("c/userspace-memory-access.sc", "syscalls"),
      ("c/malloc-overflow.sc", "malloc-overflow"),
      ("c/malloc-leak.sc", "leak"),
      ("c/const-ish.sc", "const-ish")
    ).foreach { case (scriptFileName, codePathRelative) =>
      s"Executing '$scriptFileName' on '$codePathRelative'" in {
        exec(scriptFileName, s"$testCodeRoot/$codePathRelative")
      }
    }

    "execute a simple script" in new Fixture {
      def test(scriptFile: File, outputFile: File) = {
        val escScriptPath = outputFile.pathAsString.replace("\\", "\\\\")
        scriptFile.write(s"""
        |val fw = new java.io.FileWriter("$escScriptPath", true)
        |fw.write("michael was here")
        |fw.close() 
        """.stripMargin)

        ReplBridge.main(Array("--script", scriptFile.pathAsString))

        withClue(s"$outputFile content: ") {
          outputFile.lines.head shouldBe "michael was here"
        }
      }
    }

    "pass parameters to script" in new Fixture {
      def test(scriptFile: File, outputFile: File) = {
        scriptFile.write(s"""
          |@main def foo(outFile: String, magicNumber: Int) = {
          |  val fw = new java.io.FileWriter(outFile, true)
          |  fw.write(magicNumber.toString)
          |  fw.close() 
          |}
          """.stripMargin)

        ReplBridge.main(
          Array(
            "--script",
            scriptFile.pathAsString,
            "--param",
            s"outFile=${outputFile.pathAsString}",
            "--param",
            "magicNumber=42"
          )
        )

        withClue(s"$outputFile content: ") {
          outputFile.lines.head shouldBe "42"
        }
      }
    }

    "script with multiple @main methods" in new Fixture {
      def test(scriptFile: File, outputFile: File) = {
        val escScriptPath = outputFile.pathAsString.replace("\\", "\\\\")

        scriptFile.write(s"""
          |@main def foo() = {
          |  val fw = new java.io.FileWriter("$escScriptPath", true)
          |  fw.write("foo was called")
          |  fw.close() 
          |}
          |@main def bar() = {
          |  val fw = new java.io.FileWriter("$escScriptPath", true)
          |  fw.write("bar was called")
          |  fw.close() 
          |}
          """.stripMargin)

        ReplBridge.main(Array("--script", scriptFile.pathAsString, "--command", "bar"))

        withClue(s"$outputFile content: ") {
          outputFile.lines.head shouldBe "bar was called"
        }
      }
    }

    "use additional import script: //> using file directive" in new Fixture {
      def test(scriptFile: File, outputFile: File) = {
        val escScriptPath        = outputFile.pathAsString.replace("\\", "\\\\")
        val additionalImportFile = Paths.get("joern-cli/src/test/resources/additional-import.sc").toAbsolutePath

        scriptFile.write(s"""
          |//> using file $additionalImportFile
          |val fw = new java.io.FileWriter("$escScriptPath", true)
          |fw.write(sayHello("michael")) //function defined in additionalImportFile
          |fw.close() 
          """.stripMargin)

        ReplBridge.main(Array("--script", scriptFile.pathAsString))

        withClue(s"$outputFile content: ") {
          outputFile.lines.head shouldBe "hello, michael"
        }
      }
    }

    "use additional import script: --import parameter" in new Fixture {
      def test(scriptFile: File, outputFile: File) = {
        val escScriptPath        = outputFile.pathAsString.replace("\\", "\\\\")
        val additionalImportFile = Paths.get("joern-cli/src/test/resources/additional-import.sc").toAbsolutePath

        scriptFile.write(s"""
          |val fw = new java.io.FileWriter("$escScriptPath", true)
          |fw.write(sayHello("michael")) //function defined in additionalImportFile
          |fw.close() 
          """.stripMargin)

        ReplBridge.main(Array("--script", scriptFile.pathAsString, "--import", additionalImportFile.toString))

        withClue(s"$outputFile content: ") {
          outputFile.lines.head shouldBe "hello, michael"
        }
      }
    }

    "should return Failure if" when {
      "script doesn't exist" in {
        val result = ReplBridge.runScript(Config(scriptFile = Some(scriptsRoot.resolve("does-not-exist.sc"))))
        result.failed.get.getMessage should include("does not exist")
      }

      "script runs ins an exception" in {
        val result = ReplBridge.runScript(Config(scriptFile = Some(scriptsRoot.resolve("trigger-error.sc"))))
        result.failed.get.getMessage should include("exit code was 1")
      }
    }
  }
}

object RunScriptTests {
  val projectRoot  = ProjectRoot.find.path.toAbsolutePath
  val scriptsRoot  = projectRoot.resolve("scripts")
  val testCodeRoot = s"${projectRoot}/joern-cli/src/test/resources/testcode"

  def exec(scriptFileName: String, codePathAbsolute: String): Unit = {
    ReplBridge
      .runScript(
        Config(scriptFile = Some(scriptsRoot.resolve(scriptFileName)), params = Map("inputPath" -> codePathAbsolute))
      )
      .get
  }

  trait Fixture {
    def test(scriptFile: File, outputFile: File): Unit
    for {
      scriptFile <- File.temporaryFile()
      outputFile <- File.temporaryFile()
    } test(scriptFile, outputFile)
  }
}
