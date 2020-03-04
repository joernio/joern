package io.shiftleft.joern.scripting

import io.shiftleft.console.scripting.AmmoniteExecutor
import io.shiftleft.joern.Predefined

object JoernAmmoniteExecutor extends AmmoniteExecutor {
  override lazy val predef: String =
    Predefined.forScripts
}
