package io.joern.x2cpg.utils.server

import java.nio.file.Path

trait FrontendExecutor {
  def execute(input: Path, output: Path, extraArgs: String*): Unit

  def isAvailable: Boolean

  def shutdown(): Unit = {}
}
