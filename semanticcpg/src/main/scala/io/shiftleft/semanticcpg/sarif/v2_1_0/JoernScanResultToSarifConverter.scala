package io.shiftleft.semanticcpg.sarif.v2_1_0

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.{NodeExtensionFinder, *}
import io.shiftleft.semanticcpg.sarif.{SarifSchema, ScanResultToSarifConverter}

import java.net.URI

/** Convert finding node to a SARIF v2.1.0 model.
  */
class JoernScanResultToSarifConverter extends ScanResultToSarifConverter {

  import JoernScanResultToSarifConverter.*

  override def convertFindingToReportingDescriptor(finding: Finding): Option[SarifSchema.ReportingDescriptor] = {
    val description = createMessage(finding.description)
    Option(Schema.ReportingDescriptor(id = finding.name, name = finding.title, fullDescription = Option(description)))
  }

  override def convertFindingToResult(finding: Finding): SarifSchema.Result = {
    val locations        = finding.evidence.lastOption.map(nodeToLocation).toList
    val relatedLocations = finding.evidence.headOption.map(nodeToLocation).toList
    val codeFlows = evidenceToCodeFlow(finding) match {
      case codeFlow if codeFlow.threadFlows.isEmpty => Nil
      case codeFlow                                 => codeFlow :: Nil
    }
    Schema.Result(
      ruleId = finding.name,
      message = Schema.Message(text = finding.title),
      level = SarifSchema.Level.cvssToLevel(finding.score),
      locations = locations,
      relatedLocations = relatedLocations,
      codeFlows = codeFlows
    )
  }

  protected def evidenceToCodeFlow(finding: Finding): Schema.CodeFlow = {
    val locations = finding.evidence.map(node => Schema.ThreadFlowLocation(location = nodeToLocation(node))).l
    if (locations.isEmpty) {
      Schema.CodeFlow(threadFlows = Nil)
    } else {
      Schema.CodeFlow(threadFlows = Schema.ThreadFlow(locations) :: Nil)
    }
  }

  protected def createMessage(text: String): Schema.Message = {
    val plain    = text.replace("`", "")              // todo: use better markdown stripping
    val markdown = Option(text).filterNot(_ == plain) // if these are equal, ignore
    Schema.Message(text = plain, markdown = markdown)
  }

  protected def nodeToLocation(node: StoredNode): Schema.Location = {
    Schema.Location(physicalLocation =
      Schema.PhysicalLocation(
        artifactLocation = Schema.ArtifactLocation(uri = nodeToUri(node)),
        region = nodeToRegion(node)
      )
    )
  }

  protected def nodeToUri(node: StoredNode): Option[URI] = {
    node match {
      case t: TypeDecl if !t.isExternal => Option(t.filename).filterNot(_ == "<empty>").map(URI(_))
      case m: Method if !m.isExternal   => Option(m.filename).filterNot(_ == "<empty>").map(URI(_))
      case expr: Expression             => expr.file.map(x => URI(x.name)).headOption
      case _                            => None
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
      case _ => Schema.Region(None, None, None)
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

    def description: String = getValue(FindingKeys.description).trim

    def score: Double = getValue(FindingKeys.score).toDoubleOption.getOrElse(-1d)

    protected def getValue(key: String, default: String = "<empty>"): String =
      node.keyValuePairs.find(_.key == key).map(_.value).filterNot(_ == "-").getOrElse(default)

  }
}
