package io.joern.console

import io.joern.console.scripting.AmmoniteExecutor

object DefaultAmmoniteExecutor extends AmmoniteExecutor {
  override lazy val predef: String = ""
}
