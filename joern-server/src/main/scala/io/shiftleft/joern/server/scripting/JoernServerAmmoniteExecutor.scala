package io.shiftleft.joern.server.scripting

import cats.effect.{ContextShift, IO}
import io.shiftleft.cpgserver.query.ServerAmmoniteExecutor
import io.shiftleft.joern.console.JoernAmmoniteExecutor

class JoernServerAmmoniteExecutor(implicit cs: ContextShift[IO]) extends ServerAmmoniteExecutor {
  override protected lazy val predef: String = JoernAmmoniteExecutor.predef
}
