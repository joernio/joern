package io.shiftleft.semanticcpg.sarif

import io.shiftleft.semanticcpg.sarif.SarifConfig.SarifVersion
import io.shiftleft.semanticcpg.sarif.v2_1_0.JoernScanResultToSarifConverter
import org.json4s.Serializer

import java.net.URI

case class SarifConfig(
  toolName: String = "Joern",
  toolFullName: String = "Joern - The Bug Hunter's Workbench",
  toolInformationUri: URI = URI("https://joern.io"),
  organization: String = "Joern.io",
  semanticVersion: String = "4.x.x",
  sarifVersion: SarifVersion = SarifVersion.V2_1_0,
  resultConverter: ScanResultToSarifConverter = JoernScanResultToSarifConverter(),
  customSerializers: List[Serializer[?]] = SarifSchema.serializers
)

object SarifConfig {

  enum SarifVersion {
    case V2_1_0
  }

}
