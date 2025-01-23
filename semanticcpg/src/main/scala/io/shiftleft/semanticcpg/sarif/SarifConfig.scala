package io.shiftleft.semanticcpg.sarif

import io.shiftleft.semanticcpg.sarif.SarifConfig.SarifVersion
import io.shiftleft.semanticcpg.sarif.v2_1_0.Converter

case class SarifConfig(
  toolName: String = "Joern",
  organization: String = "Joern.io",
  semanticVersion: String = "4.x.x",
  sarifVersion: SarifVersion = SarifVersion.V2_1_0,
  resultConverter: SarifResultConverter = Converter()
)

object SarifConfig {

  enum SarifVersion {
    case V2_1_0
  }

}
