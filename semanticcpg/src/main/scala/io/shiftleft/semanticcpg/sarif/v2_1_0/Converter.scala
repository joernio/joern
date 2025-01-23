package io.shiftleft.semanticcpg.sarif.v2_1_0

import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  CfgNode,
  Declaration,
  Expression,
  Finding,
  Method,
  StoredNode,
  TypeDecl
}
import io.shiftleft.semanticcpg.sarif.v2_1_0.Schema
import io.shiftleft.semanticcpg.sarif.{SarifResultConverter, SarifSchema}
import io.shiftleft.semanticcpg.language.*

import java.net.URI

/** Convert finding node to a SARIF v2.1.0 model.
  */
class Converter extends SarifResultConverter {

  import SarifResultConverter.*

  override def convertFindingToResult(finding: Finding): SarifSchema.Result = {

    def evidenceToCodeFlow(evidence: IndexedSeq[StoredNode]): Schema.CodeFlow = {
      Schema.CodeFlow(
        message = Schema.Message(text = finding.description),
        threadFlows =
          Schema.ThreadFlow(evidence.map(node => Schema.ThreadFlowLocation(location = nodeToLocation(node))).l) :: Nil
      )
    }

    Schema.Result(
      ruleId = finding.name,
      message = Schema.Message(text = finding.title),
      level = SarifSchema.Level.cvssToLevel(finding.score),
      locations = finding.evidence.lastOption.map(nodeToLocation).toList, // use the sink for this location property
      codeFlows = evidenceToCodeFlow(finding.evidence) :: Nil
    )
  }

  protected def nodeToLocation(node: StoredNode): Schema.Location = {
    Schema.Location(physicalLocation =
      Schema.PhysicalLocation(artifactLocation =
        Schema.ArtifactLocation(uri = nodeToUri(node), region = nodeToRegion(node))
      )
    )
  }

  protected def nodeToUri(node: StoredNode): URI = {
    node match {
      case t: TypeDecl      => URI(t.filename)
      case m: Method        => URI(m.filename)
      case expr: Expression => expr.file.map(x => URI(x.name)).headOption.orNull
      case _                => null
    }
  }

  protected def nodeToRegion(node: StoredNode): Schema.Region = {
    node match {
      case t: TypeDecl =>
        Schema.Region(startLine = t.lineNumber, startColumn = t.columnNumber, snippet = t.code)
      case m: Method =>
        Schema.Region(
          startLine = m.lineNumber,
          startColumn = m.columnNumber,
          endLine = m.lineNumberEnd,
          endColumn = m.columnNumberEnd,
          snippet = m.code
        )
      case n: CfgNode =>
        Schema.Region(startLine = n.lineNumber, startColumn = n.columnNumber, snippet = n.code)
      case _ => null
    }
  }

}
