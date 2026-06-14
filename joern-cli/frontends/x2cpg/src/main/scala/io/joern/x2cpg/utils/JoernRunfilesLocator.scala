package io.joern.x2cpg.utils

import scala.jdk.OptionConverters.RichOptional

// Use this class inside Joern repository and not in repositories
// which bring in the Joern repository as dependency.
object JoernRunfilesLocator {
  def resolve(path: String): Option[String] = {
    JoernRunfilesLocatorJava.resolve(path).toScala
  }
}
