package io.joern.joerncli

import io.joern.console.Config
import io.joern.joerncli.console.AmmoniteBridge
import io.shiftleft.utils.ProjectRoot
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RunScriptTests extends AnyWordSpec with Matchers {
  val projectRoot = os.Path(ProjectRoot.find.path.toAbsolutePath)
  val scriptsRoot =  projectRoot / "scripts"
  val testCodeRoot = s"${projectRoot.toNIO}/joern-cli/src/test/resources/testcode"

  Seq(
    ("c/pointer-to-int.sc", "unsafe-ptr"),
    ("c/syscalls.sc", "syscalls"),
    ("c/userspace-memory-access.sc", "syscalls"),
    ("c/malloc-overflow.sc", "malloc-overflow"),
    ("c/malloc-leak.sc", "leak"),
    ("c/const-ish.sc", "const-ish"),
  ).foreach { case (scriptFileName, codePathName) =>
    s"Executing '$scriptFileName' on '$codePathName'" in {
      exec(scriptsRoot/os.RelPath(scriptFileName), codePathName)
    }
  }

  def exec(scriptPath: os.Path, codePathRelative: String): Unit = {
    AmmoniteBridge.runScript(
      Config(
        scriptFile = Some(scriptPath),
        params = Map("inputPath" -> s"$testCodeRoot/$codePathRelative"),
        additionalImports = List(scriptsRoot/"assertions.sc")
      )
    )
  }

}
