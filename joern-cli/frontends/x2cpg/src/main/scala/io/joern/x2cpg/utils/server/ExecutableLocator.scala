package io.joern.x2cpg.utils.server

import java.nio.file.Path

trait ExecutableLocator {
  def resolve(): Path

  def isAvailable(): Boolean
}
