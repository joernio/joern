package ammonite.foo

import dotty.tools.dotc.core.Contexts.Context

class JLineTerminal(promptStr: String) extends dotty.tools.repl.JLineTerminal {

  override protected def prompt(using Context) =
    blue(s"\n$promptStr")

}
