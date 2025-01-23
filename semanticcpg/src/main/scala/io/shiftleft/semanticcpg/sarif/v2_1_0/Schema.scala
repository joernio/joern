package io.shiftleft.semanticcpg.sarif.v2_1_0

import io.shiftleft.semanticcpg.sarif.SarifSchema
import org.json4s.{CustomSerializer, Extraction}

import java.net.URI

object Schema {

  final case class ArtifactContent(text: String) extends SarifSchema.ArtifactContent

  /** Specifies the location of an artifact.
    *
    * @param uri
    *   A string containing a valid relative or absolute URI.
    * @param uriBaseId
    *   A string which indirectly specifies the absolute URI with respect to which a relative URI in the "uri" property
    *   is interpreted.
    */
  final case class ArtifactLocation(uri: URI, uriBaseId: String = "PROJECT_ROOT") extends SarifSchema.ArtifactLocation

  final case class CodeFlow(message: Message, threadFlows: List[ThreadFlow]) extends SarifSchema.CodeFlow

  final case class Location(physicalLocation: PhysicalLocation) extends SarifSchema.Location

  final case class Message(text: String) extends SarifSchema.Message

  final case class PhysicalLocation(artifactLocation: ArtifactLocation, region: Region)
      extends SarifSchema.PhysicalLocation

  final case class Region(
    startLine: Option[Int],
    startColumn: Option[Int],
    endLine: Option[Int] = None,
    endColumn: Option[Int] = None,
    snippet: ArtifactContent
  ) extends SarifSchema.Region

  final case class Result(
    ruleId: String,
    message: Message,
    level: String,
    locations: List[Location],
    codeFlows: List[CodeFlow]
  ) extends SarifSchema.Result

  final case class Run(tool: Tool, results: List[SarifSchema.Result], originalUriBaseId: Option[URI])
      extends SarifSchema.Run

  final case class ThreadFlow(locations: List[ThreadFlowLocation]) extends SarifSchema.ThreadFlow

  final case class ThreadFlowLocation(location: Location) extends SarifSchema.ThreadFlowLocation

  final case class Tool(driver: ToolComponent) extends SarifSchema.Tool

  final case class ToolComponent(
    name: String,
    fullName: String,
    organization: String,
    semanticVersion: String,
    informationUri: URI
  ) extends SarifSchema.ToolComponent

}
