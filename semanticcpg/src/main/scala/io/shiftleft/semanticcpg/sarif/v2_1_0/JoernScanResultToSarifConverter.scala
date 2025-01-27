package io.shiftleft.semanticcpg.sarif.v2_1_0

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.sarif.{ScanResultToSarifConverter, SarifSchema}

import java.net.URI

/** Convert finding node to a SARIF v2.1.0 model.
  */
class JoernScanResultToSarifConverter extends ScanResultToSarifConverter {

  import JoernScanResultToSarifConverter.*

  override def convertFindingToResult(finding: Finding): SarifSchema.Result = {
    val locations        = finding.evidence.lastOption.map(nodeToLocation).toList
    val relatedLocations = finding.evidence.headOption.map(nodeToLocation).toList
    Schema.Result(
      ruleId = finding.name,
      message = Schema.Message(text = finding.title),
      level = SarifSchema.Level.cvssToLevel(finding.score),
      locations = locations,
      relatedLocations = relatedLocations,
      codeFlows = evidenceToCodeFlow(finding) :: Nil
    )
  }

  protected def evidenceToCodeFlow(finding: Finding): Schema.CodeFlow = {
    Schema.CodeFlow(
      message = Schema.Message(text = finding.description),
      threadFlows = Schema.ThreadFlow(
        finding.evidence.map(node => Schema.ThreadFlowLocation(location = nodeToLocation(node))).l
      ) :: Nil
    )
  }

  protected def nodeToLocation(node: StoredNode): Schema.Location = {
    Schema.Location(physicalLocation =
      Schema.PhysicalLocation(
        artifactLocation = Schema.ArtifactLocation(uri = nodeToUri(node)),
        region = nodeToRegion(node)
      )
    )
  }

  protected def nodeToUri(node: StoredNode): Option[URI] = Option {
    node match {
      case t: TypeDecl if !t.isExternal => URI(t.filename)
      case m: Method if !m.isExternal   => URI(m.filename)
      case expr: Expression             => expr.file.map(x => URI(x.name)).headOption.orNull
      case _                            => null
    }
  }

  protected def nodeToRegion(node: StoredNode): Schema.Region = {
    node match {
      case t: TypeDecl =>
        Schema.Region(
          startLine = t.lineNumber,
          startColumn = t.columnNumber,
          snippet = Option(Schema.ArtifactContent(t.code))
        )
      case m: Method =>
        Schema.Region(
          startLine = m.lineNumber,
          startColumn = m.columnNumber,
          endLine = m.lineNumberEnd,
          endColumn = m.columnNumberEnd,
          snippet = Option(Schema.ArtifactContent(m.code))
        )
      case n: CfgNode =>
        Schema.Region(
          startLine = n.lineNumber,
          startColumn = n.columnNumber,
          snippet = Option(Schema.ArtifactContent(n.code))
        )
      case _ => null
    }
  }

}

/** Due to module dependencies, the following code is lifted from `io.joern.console.scan`.
  */
object JoernScanResultToSarifConverter {

  private object FindingKeys {
    val name        = "name"
    val title       = "title"
    val description = "description"
    val score       = "score"
  }

  implicit class FindingExtension(val node: Finding) extends AnyRef {

    def name: String = getValue(FindingKeys.name)

    def title: String = getValue(FindingKeys.title)

    def description: String = getValue(FindingKeys.description)

    def score: Double = getValue(FindingKeys.score).toDoubleOption.getOrElse(0d)

    protected def getValue(key: String, default: String = "<empty>"): String =
      node.keyValuePairs.find(_.key == key).map(_.value).getOrElse(default)

  }
}
