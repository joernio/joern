package io.joern.console

import io.joern.console.scripting.AmmoniteExecutor

object JoernAmmoniteExecutor extends AmmoniteExecutor {

  override lazy val predef: String =
    Predefined.forScripts
}
