package io.joern.x2cpg.utils.server

import java.nio.file.{Files, Path}

class ExecutableLocator(
  val envVar: String,
  fallbackPath: => String,
  val executablePath: String,
  pathOverride: Option[Path] = None
) {

  def resolve(): Path = {
    pathOverride
      .orElse(scala.sys.env.get(envVar).map(Path.of(_, executablePath)))
      .getOrElse(Path.of(fallbackPath, executablePath))
      .toRealPath()
  }

  def isAvailable(): Boolean = {
    Files.exists(resolve())
  }
}
