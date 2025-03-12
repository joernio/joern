package io.joern.x2cpg.typestub

import io.joern.x2cpg.X2CpgConfig
import scopt.OParser

import java.net.URL

/** Extends the config to download type stubs to help resolve type full names.
  */
trait TypeStubConfig[R <: X2CpgConfig[R]] { this: R =>

  /** Whether to use type stubs to help resolve type information or not. Using type stubs may increase memory
    * consumption.
    */
  def useTypeStubs: Boolean

  /** The entrypoint to load the type stubs into the config.
    */
  def withTypeStubs(value: Boolean): R

  /** Creates a meta-data class of information about the type stub management.
    */
  def typeStubMetaData: TypeStubMetaData =
    TypeStubMetaData(useTypeStubs, getClass.getProtectionDomain.getCodeSource.getLocation)

}

/** The meta data around managing type stub resources for this frontend.
  * @param useTypeStubs
  *   a flag to indicate whether types stubs should be used.
  * @param packagePath
  *   the code path for the frontend.
  */
case class TypeStubMetaData(useTypeStubs: Boolean, packagePath: URL)

object TypeStubConfig {

  def parserOptions[R <: X2CpgConfig[R] & TypeStubConfig[R]]: OParser[?, R] = {
    val builder = OParser.builder[R]
    import builder.*
    OParser.sequence(
      opt[Unit]("disable-type-stubs")
        .text(
          "Disables the use type stubs for type information recovery. Using type stubs may increase memory consumption."
        )
        .action((x, c) => c.withTypeStubs(false))
    )
  }

}
