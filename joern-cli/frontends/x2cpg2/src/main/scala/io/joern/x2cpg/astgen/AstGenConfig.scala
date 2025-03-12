package io.joern.x2cpg.astgen

import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData

trait AstGenConfig[R <: X2CpgConfig[R]] { this: R =>

  /** The prefix/name of the AST Gen binary.
    */
  protected val astGenProgramName: String

  /** The root prefix in <code>application.conf</code> that concerns this frontend.
    */
  protected val astGenConfigPrefix: String

  /** Indicates that a single binary handles all architectures.
    */
  protected val multiArchitectureBuilds: Boolean = false

  /** Creates a meta-data class of information about the AST Gen executable.
    */
  def astGenMetaData: AstGenProgramMetaData =
    AstGenProgramMetaData(
      astGenProgramName,
      astGenConfigPrefix,
      multiArchitectureBuilds,
      getClass.getProtectionDomain.getCodeSource.getLocation
    )

}
