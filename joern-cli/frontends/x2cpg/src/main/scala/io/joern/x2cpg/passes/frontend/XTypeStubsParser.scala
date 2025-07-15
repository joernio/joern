package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.X2CpgConfig
import scopt.OParser

import java.nio.file.Paths

/** @param typeStubsFilePath
  *   path to file containing stubs of known types
  */
case class XTypeStubsParserConfig(typeStubsFilePath: Option[String] = None)

trait TypeStubsParserConfig { this: X2CpgConfig[?] =>

  def typeStubsFilePath: Option[String]
  final def withTypeStubsFilePath(typeStubsFilePath: String): OwnType = {
    internalWithTypeStubsFilePath(Paths.get(typeStubsFilePath).toAbsolutePath.normalize().toString)
  }
  protected def internalWithTypeStubsFilePath(typeStubsFilePath: String): OwnType
}

object XTypeStubsParser {

  def parserOptions[R <: X2CpgConfig[R] & TypeStubsParserConfig]: OParser[?, R] = {
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
