package io.shiftleft.joern

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.ScriptManager

class JoernScriptManager(executor: JoernScriptExecutor = new JoernScriptExecutor()) extends ScriptManager(executor) {

  implicit class CpgScriptRunner(cpg: Cpg) {

    /**
      * Run an arbitrary script over this CPG.
      *
      * @param name The name of the script to run.
      * @return The result of running the script against this CPG.
      */
    def runScript(name: String): AnyRef =
      JoernScriptManager.this.runScript(name, cpg)
  }
}
