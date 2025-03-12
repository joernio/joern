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
    _parserOptions[R](configureTypeStubsFilePath = (path, c) => c.withTypeStubsFilePath(path))
  }

  def parserOptions2: OParser[?, XTypeStubsParserConfig] = {
    _parserOptions[XTypeStubsParserConfig](configureTypeStubsFilePath =
      (path, c) => c.copy(typeStubsFilePath = Option(path))
    )
  }

  private def _parserOptions[C](configureTypeStubsFilePath: (String, C) => C): OParser[String, C] = {
    val builder = OParser.builder[C]
    import builder.*
    opt[String]("type-stubs-file")
      .hidden()
      .action((path, c) => configureTypeStubsFilePath(path, c))
      .text("path to file with type signature stubs for known functions")
  }
}
