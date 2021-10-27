package io.joern.console

import io.shiftleft.console.scripting.AmmoniteExecutor

object DefaultAmmoniteExecutor extends AmmoniteExecutor {
  override lazy val predef: String = ""
}
