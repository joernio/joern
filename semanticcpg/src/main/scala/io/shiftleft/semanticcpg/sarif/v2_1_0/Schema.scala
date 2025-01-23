package io.shiftleft.semanticcpg.sarif.v2_1_0

import io.shiftleft.semanticcpg.sarif.SarifSchema

import java.net.URI

object Schema {

  final case class ArtifactLocation(uri: URI, region: Region) extends SarifSchema.ArtifactLocation

  final case class CodeFlow(message: Message, threadFlows: List[ThreadFlow]) extends SarifSchema.CodeFlow

  final case class Location(physicalLocation: PhysicalLocation) extends SarifSchema.Location

  final case class Message(text: String) extends SarifSchema.Message

  final case class PhysicalLocation(artifactLocation: ArtifactLocation) extends SarifSchema.PhysicalLocation

  final case class Region(
    startLine: Option[Int],
    startColumn: Option[Int],
    endLine: Option[Int] = None,
    endColumn: Option[Int] = None,
    snippet: String
  ) extends SarifSchema.Region

  final case class Result(
    ruleId: String,
    message: Message,
    level: String,
    locations: List[Location],
    codeFlows: List[CodeFlow]
  ) extends SarifSchema.Result

  final case class Run(tool: Tool, results: List[SarifSchema.Result]) extends SarifSchema.Run

  final case class ThreadFlow(locations: List[ThreadFlowLocation]) extends SarifSchema.ThreadFlow

  final case class ThreadFlowLocation(location: Location) extends SarifSchema.ThreadFlowLocation

  final case class Tool(driver: ToolComponent) extends SarifSchema.Tool

  final case class ToolComponent(name: String, organization: String, semanticVersion: String)
      extends SarifSchema.ToolComponent

}
