package io.shiftleft.semanticcpg.sarif

import io.shiftleft.semanticcpg.sarif.SarifConfig.SarifVersion
import io.shiftleft.semanticcpg.sarif.v2_1_0.JoernScanResultToSarifConverter
import org.json4s.Serializer

import java.net.URI

/** A configuration for tool-specific information and arguments on transforming how findings are to be converted to
  * SARIF.
  *
  * @param toolName
  *   The name of the tool component.
  * @param toolFullName
  *   The name of the tool component along with its version and any other useful identifying information, such as its
  *   locale.
  * @param toolInformationUri
  *   The absolute URI at which information about this version of the tool component can be found.
  * @param organization
  *   The organization or company that produced the tool component.
  * @param semanticVersion
  *   The tool component version in the format specified by Semantic Versioning 2.0.
  * @param sarifVersion
  *   The SARIF format version of the resulting log file.
  * @param resultConverter
  *   A transformer class to map from Finding nodes to a SARIF `Result`.
  * @param customSerializers
  *   Additional JSON serializers for any additional properties for [[io.shiftleft.semanticcpg.sarif.Sarif]] derived
  *   classes.
  */
case class SarifConfig(
  toolName: String = "Joern",
  toolFullName: Option[String] = Option("Joern - The Bug Hunter's Workbench"),
  toolInformationUri: Option[URI] = Option(URI("https://joern.io")),
  organization: Option[String] = Option("Joern.io"),
  semanticVersion: Option[String] = None,
  sarifVersion: SarifVersion = SarifVersion.V2_1_0,
  resultConverter: ScanResultToSarifConverter = JoernScanResultToSarifConverter(),
  customSerializers: List[Serializer[?]] = SarifSchema.serializers
)

object SarifConfig {

  enum SarifVersion {
    case V2_1_0
  }

}
