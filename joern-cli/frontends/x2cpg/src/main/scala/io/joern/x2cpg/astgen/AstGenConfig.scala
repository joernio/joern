package io.joern.x2cpg.astgen

import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.astgen.AstGenRunner.AstGenProgramMetaData

trait AstGenConfig[R <: X2CpgConfig[R]] { this: R =>

  protected val astGenProgramName: String
  protected val astGenConfigPrefix: String

  def astGenMetaData: AstGenProgramMetaData = AstGenProgramMetaData(
    astGenProgramName,
    astGenConfigPrefix,
    getClass.getProtectionDomain.getCodeSource.getLocation
  )

}
