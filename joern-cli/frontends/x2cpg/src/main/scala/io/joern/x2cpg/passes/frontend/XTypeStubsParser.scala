package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.X2CpgConfig
import scopt.OParser

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

object XTypeStubsParser {

  def parserOptions[R <: X2CpgConfig[R] & TypeStubsParserConfig[R]]: OParser[?, R] = {
    val builder = OParser.builder[R]
    import builder.*
    OParser.sequence(
      opt[String]("type-stubs-file")
        .hidden()
        .action((path, c) => c.withTypeStubsFilePath(path))
        .text("path to file with type signature stubs for known functions")
    )
  }
}
