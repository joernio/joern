package io.shiftleft.semanticcpg.sarif

import io.shiftleft.semanticcpg.sarif.v2_1_0.Schema
import org.slf4j.LoggerFactory

import java.net.URI

object SarifSchema {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Provides a basic Sarif trait under which possibly multiple defined schemata would be defined.
    */
  sealed trait Sarif {
    def version: String

    def `$schema`: String

    def runs: List[Run]
  }

  case class Sarif2_1_0(runs: List[Run]) extends Sarif {
    def version: String = "2.1.0"

    def `$schema`: String = "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.4.json"
  }

  // Minimal properties we want to use across versions:

  trait ArtifactLocation private[sarif] {
    def uri: URI
    def region: Region
  }

  trait CodeFlow private[sarif] {
    def message: Message
    def threadFlows: List[ThreadFlow]
  }

  trait Location private[sarif] {
    def physicalLocation: PhysicalLocation
  }

  trait Message private[sarif] {
    def text: String
  }

  trait PhysicalLocation private[sarif] {
    def artifactLocation: ArtifactLocation
  }

  trait Region private[sarif] {
    def startLine: Option[Int]

    def startColumn: Option[Int]

    def endLine: Option[Int]

    def endColumn: Option[Int]

    def snippet: String
  }

  trait Result private[sarif] {
    def ruleId: String
    def message: Message
    def level: String
    def locations: List[Location]
    def codeFlows: List[CodeFlow]
  }

  trait Run private[sarif] {
    def tool: Tool
    def results: List[Result]
  }

  trait ThreadFlow private[sarif] {
    def locations: List[ThreadFlowLocation]
  }

  trait ThreadFlowLocation private[sarif] {
    def location: Location
  }

  trait Tool private[sarif] {
    def driver: ToolComponent
  }

  trait ToolComponent private[sarif] {
    def name: String
    def organization: String
    def semanticVersion: String
  }

  object Level {
    val None    = "none"
    val Note    = "note"
    val Warning = "warning"
    val Error   = "error"

    def cvssToLevel(cvssScore: Double): String = {
      cvssScore match {
        case score if score == 0.0  => None
        case score if score <= 3.9  => Note
        case score if score <= 6.9  => Warning
        case score if score <= 10.0 => Error
        case score =>
          logger.error(s"Score '$score' is not a valid CVSS score! Defaulting to 'none' SARIF level.")
          None
      }
    }

  }

}
