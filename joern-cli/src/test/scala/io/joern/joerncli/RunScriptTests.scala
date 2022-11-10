package io.joern.joerncli

import io.joern.console.Config
import io.joern.joerncli.console.ReplBridge
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RunScriptTests extends AnyWordSpec with Matchers {
  import RunScriptTests._

  Seq(
    ("c/pointer-to-int.sc", "unsafe-ptr"),
    ("c/syscalls.sc", "syscalls"),
    ("c/userspace-memory-access.sc", "syscalls"),
    ("c/malloc-overflow.sc", "malloc-overflow"),
    ("c/malloc-leak.sc", "leak"),
    ("c/const-ish.sc", "const-ish"),
  ).foreach { case (scriptFileName, codePathRelative) =>
    s"Executing '$scriptFileName' on '$codePathRelative'" in {
      exec(os.RelPath(scriptFileName), s"$testCodeRoot/$codePathRelative")
    }
  }

}

object RunScriptTests {
  val projectRoot = os.Path(ProjectRoot.find.path.toAbsolutePath)
  val scriptsRoot =  projectRoot / "scripts"
  val testCodeRoot = s"${projectRoot.toNIO}/joern-cli/src/test/resources/testcode"

  def exec(scriptPath: os.RelPath, codePathAbsolute: String): Unit = {
    ReplBridge.runScript(
      Config(
        scriptFile = Some(scriptsRoot/scriptPath),
        params = Map("inputPath" -> codePathAbsolute),
        additionalImports = List(scriptsRoot/"assertions.sc")
      )
    )
  }
}
