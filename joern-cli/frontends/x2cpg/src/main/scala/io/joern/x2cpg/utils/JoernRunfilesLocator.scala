package io.joern.x2cpg.utils

import scala.jdk.OptionConverters.RichOptional

// Use this class inside Joern repository and not in repositories
// which bring in the Joern repository as dependency.
// Restricting it to io.joern package does not fully enforce this.
// But it will catch most cases of unintended misuse.
private[joern] object JoernRunfilesLocator {
  def resolve(path: String): Option[String] = {
    JoernRunfilesLocatorJava.resolve(path).toScala
  }
}
