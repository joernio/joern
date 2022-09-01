package io.joern.console

import dotty.tools.dotc.core.Contexts.Context

class JLineTerminal(promptText: String) extends dotty.tools.repl.JLineTerminal {
  override protected def prompt(using Context) =
    blue(s"\n$promptText")
}
