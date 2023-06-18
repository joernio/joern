package io.joern.joerncli

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
}
