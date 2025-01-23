package io.shiftleft.semanticcpg.sarif

import org.json4s.{CustomSerializer, Extraction, Serializer}
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

    def `$schema`: String = "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"
  }

  // Minimal properties we want to use across versions:

  trait ArtifactContent private[sarif] {
    def text: String
  }

  trait ArtifactLocation private[sarif] {
    def uri: URI
    def uriBaseId: String
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
    def region: Region
  }

  trait Region private[sarif] {
    def startLine: Option[Int]

    def startColumn: Option[Int]

    def endLine: Option[Int]

    def endColumn: Option[Int]

    def snippet: ArtifactContent
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
    def originalUriBaseId: Option[URI] // Unofficial, but useful for setting uriBaseId in a single location
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
    def fullName: String
    def organization: String
    def semanticVersion: String
    def informationUri: URI
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

  val serializers: List[Serializer[?]] = List(
    new CustomSerializer[SarifSchema.Sarif](implicit format =>
      (
        { case _ =>
          ???
        },
        { case sarif: SarifSchema.Sarif =>
          Extraction.decompose(Map("version" -> sarif.version, "$schema" -> sarif.`$schema`, "runs" -> sarif.runs))
        }
      )
    ),
    new CustomSerializer[SarifSchema.ArtifactLocation](implicit format =>
      (
        { case _ =>
          ???
        },
        { case location: SarifSchema.ArtifactLocation =>
          Extraction.decompose(Map("uri" -> location.uri, "uriBaseId" -> location.uriBaseId))
        }
      )
    ),
    new CustomSerializer[SarifSchema.Region](implicit format =>
      (
        { case _ =>
          ???
        },
        { case region: SarifSchema.Region =>
          val elementMap = Map.newBuilder[String, Any]
          region.startLine.foreach(x => elementMap.addOne("startLine" -> x))
          region.startColumn.foreach(x => elementMap.addOne("startColumn" -> x))
          region.endLine.foreach(x => elementMap.addOne("endLine" -> x))
          region.endColumn.foreach(x => elementMap.addOne("endColumn" -> x))
          elementMap.addOne("snippet" -> region.snippet)
          Extraction.decompose(elementMap.result())
        }
      )
    ),
    new CustomSerializer[SarifSchema.Run](implicit format =>
      (
        { case _ =>
          ???
        },
        { case run: SarifSchema.Run =>
          val elementMap = Map.newBuilder[String, Any]
          elementMap.addOne("tool" -> run.tool)
          run.originalUriBaseId.foreach(x =>
            elementMap.addOne("originalUriBaseIds" -> Map("PROJECT_ROOT" -> Map("uri" -> x.toString)))
          )
          elementMap.addOne("results" -> run.results)
          Extraction.decompose(elementMap.result())
        }
      )
    ),
    new CustomSerializer[URI](implicit format =>
      (
        { case _ =>
          ???
        },
        { case uri: URI =>
          Extraction.decompose(uri.toString)
        }
      )
    )
  )

}
