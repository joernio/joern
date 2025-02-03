package io.shiftleft.semanticcpg.sarif

import org.json4s.{CustomSerializer, Extraction, Serializer}
import org.slf4j.LoggerFactory

import java.net.URI

object SarifSchema {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Provides a basic Sarif trait under which possibly multiple defined schemata would be defined.
    */
  sealed trait Sarif {

    /** @return
      *   The SARIF format version of this log file.
      */
    def version: String

    /** @return
      *   The URI of the JSON schema corresponding to the version.
      */
    def schema: String

    /** @return
      *   The set of runs contained in this log file.
      */
    def runs: List[Run]
  }

  case class Sarif2_1_0(runs: List[Run]) extends Sarif {
    def version: String = "2.1.0"

    def schema: String = "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"
  }

  // Minimal properties we want to use across versions:

  /** Represents the contents of an artifact.
    */
  trait ArtifactContent private[sarif] {

    /** @return
      *   UTF-8-encoded content from a text artifact.
      */
    def text: String
  }

  /** Specifies the location of an artifact.
    */
  trait ArtifactLocation private[sarif] {

    /** @return
      *   A string containing a valid relative or absolute URI.
      */
    def uri: Option[URI]

    /** @return
      *   A string which indirectly specifies the absolute URI with respect to which a relative URI in the "uri"
      *   property is interpreted.
      */
    def uriBaseId: Option[String]
  }

  /** A set of threadFlows which together describe a pattern of code execution relevant to detecting a result.
    */
  trait CodeFlow private[sarif] {

    /** @return
      *   A message relevant to the code flow.
      */
    def message: Option[Message]

    /** @return
      *   An array of one or more unique threadFlow objects, each of which describes the progress of a program through a
      *   thread of execution.
      */
    def threadFlows: List[ThreadFlow]
  }

  /** A location within a programming artifact.
    */
  trait Location private[sarif] {

    /** @return
      *   Identifies the artifact and region.
      */
    def physicalLocation: PhysicalLocation
  }

  /** Encapsulates a message intended to be read by the end user.
    */
  trait Message private[sarif] {

    /** @return
      *   A plain text message string.
      */
    def text: String

    /** @return
      *   A Markdown message string.
      */
    def markdown: Option[String]
  }

  /** A physical location relevant to a result. Specifies a reference to a programming artifact together with a range of
    * bytes or characters within that artifact.
    */
  trait PhysicalLocation private[sarif] {

    /** @return
      *   The location of the artifact.
      */
    def artifactLocation: ArtifactLocation

    /** @return
      *   Specifies a portion of the artifact.
      */
    def region: Region
  }

  /** A region within an artifact where a result was detected.
    */
  trait Region private[sarif] {

    /** @return
      *   The line number of the first character in the region.
      */
    def startLine: Option[Int]

    /** @return
      *   The column number of the first character in the region.
      */
    def startColumn: Option[Int]

    /** @return
      *   The line number of the last character in the region.
      */
    def endLine: Option[Int]

    /** @return
      *   The column number of the character following the end of the region.
      */
    def endColumn: Option[Int]

    /** @return
      *   The portion of the artifact contents within the specified region.
      */
    def snippet: Option[ArtifactContent]

    /** @return
      *   true if startLine is empty and larger than 0, as this is the main required property.
      */
    def isEmpty: Boolean = startLine.forall(_ <= 0)
  }

  /** Metadata that describes a specific report produced by the tool, as part of the analysis it provides or its runtime
    * reporting.
    */
  trait ReportingDescriptor private[sarif] {

    /** @return
      *   A stable, opaque identifier for the report.
      */
    def id: String

    /** @return
      *   A report identifier that is understandable to an end user.
      */
    def name: String

    /** @return
      *   A concise description of the report. Should be a single sentence that is understandable when visible space is
      *   limited to a single line of text.
      */
    def shortDescription: Option[Message]

    /** @return
      *   A description of the report. Should, as far as possible, provide details sufficient to enable resolution of
      *   any problem indicated by the result.
      */
    def fullDescription: Option[Message]

    /** @return
      *   A URI where the primary documentation for the report can be found.
      */
    def helpUri: Option[URI]

  }

  /** A result produced by an analysis tool.
    */
  trait Result private[sarif] {

    /** @return
      *   The stable, unique identifier of the rule, if any, to which this result is relevant.
      */
    def ruleId: String

    /** @return
      *   A message that describes the result. The first sentence of the message only will be displayed when visible
      *   space is limited.
      */
    def message: Message

    /** @return
      *   A value specifying the severity level of the result.
      */
    def level: String

    /** @return
      *   The set of locations where the result was detected. Specify only one location unless the problem indicated by
      *   the result can only be corrected by making a change at every specified location.
      */
    def locations: List[Location]

    /** @return
      *   A set of locations relevant to this result.
      */
    def relatedLocations: List[Location]

    /** @return
      *   An array of 'codeFlow' objects relevant to the result.
      */
    def codeFlows: List[CodeFlow]

    /** GitHub makes use of this property to track effectively the same finding across files between versions.
      * @return
      *   A set of strings that contribute to the stable, unique identity of the result.
      */
    def partialFingerprints: Map[String, String]
  }

  /** Describes a single run of an analysis tool, and contains the reported output of that run.
    */
  trait Run private[sarif] {

    /** @return
      *   Information about the tool or tool pipeline that generated the results in this run. A run can only contain
      *   results produced by a single tool or tool pipeline. A run can aggregate results from multiple log files, as
      *   long as context around the tool run (tool command-line arguments and the like) is identical for all aggregated
      *   files.
      */
    def tool: Tool

    /** @return
      *   The set of results contained in an SARIF log. The results array can be omitted when a run is solely exporting
      *   rules metadata. It must be present (but may be empty) if a log file represents an actual scan.
      */
    def results: List[Result]

    /** @return
      *   The artifact location specified by each uriBaseId symbol on the machine where the tool originally ran.
      */
    def originalUriBaseIds: Map[String, ArtifactLocation]
  }

  /** Describes a sequence of code locations that specify a path through a single thread of execution such as an
    * operating system or fiber.
    */
  trait ThreadFlow private[sarif] {

    /** @return
      *   A temporally ordered array of 'threadFlowLocation' objects, each of which describes a location visited by the
      *   tool while producing the result.
      */
    def locations: List[ThreadFlowLocation]
  }

  /** A location visited by an analysis tool while simulating or monitoring the execution of a program.
    */
  trait ThreadFlowLocation private[sarif] {

    /** @return
      *   The code location.
      */
    def location: Location
  }

  /** The analysis tool that was run.
    */
  trait Tool private[sarif] {
    def driver: ToolComponent
  }

  /** A component, such as a plug-in or the driver, of the analysis tool that was run.
    */
  trait ToolComponent private[sarif] {

    /** @return
      *   The name of the tool component.
      */
    def name: String

    /** @return
      *   The name of the tool component along with its version and any other useful identifying information, such as
      *   its locale.
      */
    def fullName: Option[String]

    /** @return
      *   The organization or company that produced the tool component.
      */
    def organization: Option[String]

    /** @return
      *   The tool component version in the format specified by Semantic Versioning 2.0.
      */
    def semanticVersion: Option[String]

    /** @return
      *   The absolute URI at which information about this version of the tool component can be found.
      */
    def informationUri: Option[URI]

    /** @return
      *   An array of reportingDescriptor objects relevant to the analysis performed by the tool component.
      */
    def rules: List[ReportingDescriptor]
  }

  /** A value specifying the severity level of the result.
    */
  object Level {
    val None    = "none"
    val Note    = "note"
    val Warning = "warning"
    val Error   = "error"

    def cvssToLevel(cvssScore: Double): String = {
      cvssScore match {
        case score if score < 0.0 || score > 10.0 =>
          logger.error(s"Score '$score' is not a valid CVSS score! Defaulting to 'warning' SARIF level.")
          Warning
        case score if score == 0.0  => None
        case score if score <= 3.9  => Note
        case score if score <= 6.9  => Warning
        case score if score <= 10.0 => Error
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
          Extraction.decompose(Map("version" -> sarif.version, "$schema" -> sarif.schema, "runs" -> sarif.runs))
        }
      )
    ),
    new CustomSerializer[SarifSchema.ArtifactLocation](implicit format =>
      (
        { case _ =>
          ???
        },
        { case location: SarifSchema.ArtifactLocation =>
          val elementMap = Map.newBuilder[String, Any]
          location.uri.foreach(x => elementMap.addOne("uri" -> x))
          elementMap.addOne("uriBaseId" -> location.uriBaseId)
          Extraction.decompose(elementMap.result())
        }
      )
    ),
    new CustomSerializer[SarifSchema.CodeFlow](implicit format =>
      (
        { case _ =>
          ???
        },
        { case flow: SarifSchema.CodeFlow =>
          val elementMap = Map.newBuilder[String, Any]
          flow.message.foreach(x => elementMap.addOne("message" -> x))
          elementMap.addOne("threadFlows" -> flow.threadFlows)
          Extraction.decompose(elementMap.result())
        }
      )
    ),
    new CustomSerializer[SarifSchema.PhysicalLocation](implicit format =>
      (
        { case _ =>
          ???
        },
        { case location: SarifSchema.PhysicalLocation =>
          val elementMap = Map.newBuilder[String, Any]
          elementMap.addOne("artifactLocation" -> location.artifactLocation)
          if !location.region.isEmpty then elementMap.addOne("region" -> Extraction.decompose(location.region))
          Extraction.decompose(elementMap.result())
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
          region.startLine.filterNot(x => x <= 0).foreach(x => elementMap.addOne("startLine" -> x))
          region.startColumn.filterNot(x => x <= 0).foreach(x => elementMap.addOne("startColumn" -> x))
          region.endLine.filterNot(x => x <= 0).foreach(x => elementMap.addOne("endLine" -> x))
          region.endColumn.filterNot(x => x <= 0).foreach(x => elementMap.addOne("endColumn" -> x))
          region.snippet.foreach(x => elementMap.addOne("snippet" -> x))
          Extraction.decompose(elementMap.result())
        }
      )
    ),
    new CustomSerializer[ReportingDescriptor](implicit format =>
      (
        { case _ =>
          ???
        },
        { case x: ReportingDescriptor =>
          val elementMap = Map.newBuilder[String, Any]
          elementMap.addOne("id"   -> x.id)
          elementMap.addOne("name" -> x.name)
          x.shortDescription.foreach(x => elementMap.addOne("shortDescription" -> x))
          x.fullDescription.foreach(x => elementMap.addOne("fullDescription" -> x))
          x.helpUri.foreach(x => elementMap.addOne("helpUri" -> x))
          Extraction.decompose(elementMap.result())
        }
      )
    ),
    new CustomSerializer[SarifSchema.Result](implicit format =>
      (
        { case _ =>
          ???
        },
        { case result: SarifSchema.Result =>
          val elementMap = Map.newBuilder[String, Any]
          elementMap.addOne("ruleId"  -> result.ruleId)
          elementMap.addOne("message" -> result.message)
          elementMap.addOne("level"   -> result.level)
          // Locations & related locations have no minimum, but do not allow duplicates
          elementMap.addOne("locations"        -> result.locations.distinct)
          elementMap.addOne("relatedLocations" -> result.relatedLocations.distinct)
          // codeFlows may be empty, but thread flows may not have empty arrays
          elementMap.addOne("codeFlows" -> result.codeFlows.filterNot(_.threadFlows.isEmpty))

          if result.partialFingerprints.nonEmpty then
            elementMap.addOne("partialFingerprints" -> result.partialFingerprints)

          Extraction.decompose(elementMap.result())
        }
      )
    ),
    new CustomSerializer[ToolComponent](implicit format =>
      (
        { case _ =>
          ???
        },
        { case x: ToolComponent =>
          val elementMap = Map.newBuilder[String, Any]
          elementMap.addOne("name" -> x.name)
          x.fullName.foreach(x => elementMap.addOne("fullName" -> x))
          x.organization.foreach(x => elementMap.addOne("organization" -> x))
          x.semanticVersion.foreach(x => elementMap.addOne("semanticVersion" -> x))
          x.informationUri.foreach(x => elementMap.addOne("informationUri" -> x))
          elementMap.addOne("rules" -> x.rules)
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
