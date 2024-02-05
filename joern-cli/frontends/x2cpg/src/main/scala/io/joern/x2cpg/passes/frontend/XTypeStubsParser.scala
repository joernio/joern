package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.X2CpgConfig

import java.nio.file.Paths

/** @param typeStubsFilePath
  *   path to file containing stubs of known types
  */
case class XTypeStubsParserConfig(typeStubsFilePath: Option[String] = None)

trait TypeStubsParserConfig[R <: X2CpgConfig[R]] { this: R =>

  var typeStubsFilePath: Option[String] = None

  def withTypeStubsFilePath(typeStubsFilePath: String): R = {
    this.typeStubsFilePath = Some(Paths.get(typeStubsFilePath).toAbsolutePath.normalize().toString)
    this.asInstanceOf[R]
  }
}
