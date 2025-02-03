package io.shiftleft.semanticcpg.sarif.v2_1_0

import io.shiftleft.semanticcpg.sarif.SarifSchema
import io.shiftleft.semanticcpg.sarif.SarifSchema.Location
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
  final case class ArtifactLocation(uri: Option[URI] = None, uriBaseId: Option[String] = Option("PROJECT_ROOT"))
      extends SarifSchema.ArtifactLocation

  final case class CodeFlow(threadFlows: List[ThreadFlow], message: Option[Message] = None) extends SarifSchema.CodeFlow

  final case class Location(physicalLocation: PhysicalLocation) extends SarifSchema.Location

  final case class Message(text: String, markdown: Option[String] = None) extends SarifSchema.Message

  final case class PhysicalLocation(artifactLocation: ArtifactLocation, region: Region)
      extends SarifSchema.PhysicalLocation

  final case class Region(
    startLine: Option[Int],
    startColumn: Option[Int] = None,
    endLine: Option[Int] = None,
    endColumn: Option[Int] = None,
    snippet: Option[ArtifactContent] = None
  ) extends SarifSchema.Region

  final case class ReportingDescriptor(
    id: String,
    name: String,
    shortDescription: Option[Message] = None,
    fullDescription: Option[Message] = None,
    helpUri: Option[URI] = None
  ) extends SarifSchema.ReportingDescriptor

  final case class Result(
    ruleId: String,
    message: Message,
    level: String,
    locations: List[Location],
    relatedLocations: List[Location],
    codeFlows: List[CodeFlow],
    partialFingerprints: Map[String, String] = Map.empty
  ) extends SarifSchema.Result

  final case class Run(tool: Tool, results: List[SarifSchema.Result], originalUriBaseIds: Map[String, ArtifactLocation])
      extends SarifSchema.Run

  final case class ThreadFlow(locations: List[ThreadFlowLocation]) extends SarifSchema.ThreadFlow

  final case class ThreadFlowLocation(location: Location) extends SarifSchema.ThreadFlowLocation

  final case class Tool(driver: ToolComponent) extends SarifSchema.Tool

  final case class ToolComponent(
    name: String,
    fullName: Option[String] = None,
    organization: Option[String] = None,
    semanticVersion: Option[String] = None,
    informationUri: Option[URI] = None,
    rules: List[SarifSchema.ReportingDescriptor] = Nil
  ) extends SarifSchema.ToolComponent

}
