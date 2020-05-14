package io.shiftleft.joern.console

import io.shiftleft.console.scripting.AmmoniteExecutor

object JoernAmmoniteExecutor extends AmmoniteExecutor {

  override lazy val predef: String =
    Predefined.forScripts
}
