package io.shiftleft.codepropertygraph.generated.v2.nodes
import io.joern.odb2

trait StaticType[+T]

trait AbstractNode extends odb2.DNodeOrNode with StaticType[AnyRef] {
  def label: String
  def propertiesMap: java.util.Map[String, Any]
}

abstract class StoredNode(graph_4762: odb2.Graph, kind_4762: Short, seq_4762: Int) extends odb2.GNode(graph_4762, kind_4762, seq_4762) with AbstractNode {
final def _aliasOfOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 0).asInstanceOf[IndexedSeq[StoredNode]]
final def _aliasOfIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 0).asInstanceOf[IndexedSeq[StoredNode]]

final def _argumentOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 1).asInstanceOf[IndexedSeq[StoredNode]]
final def _argumentIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 1).asInstanceOf[IndexedSeq[StoredNode]]

final def _astOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 2).asInstanceOf[IndexedSeq[StoredNode]]
final def _astIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 2).asInstanceOf[IndexedSeq[StoredNode]]

final def _bindsOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 3).asInstanceOf[IndexedSeq[StoredNode]]
final def _bindsIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 3).asInstanceOf[IndexedSeq[StoredNode]]

final def _bindsToOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 4).asInstanceOf[IndexedSeq[StoredNode]]
final def _bindsToIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 4).asInstanceOf[IndexedSeq[StoredNode]]

final def _callOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 5).asInstanceOf[IndexedSeq[StoredNode]]
final def _callIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 5).asInstanceOf[IndexedSeq[StoredNode]]

final def _captureOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 6).asInstanceOf[IndexedSeq[StoredNode]]
final def _captureIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 6).asInstanceOf[IndexedSeq[StoredNode]]

final def _capturedByOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 7).asInstanceOf[IndexedSeq[StoredNode]]
final def _capturedByIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 7).asInstanceOf[IndexedSeq[StoredNode]]

final def _cdgOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 8).asInstanceOf[IndexedSeq[StoredNode]]
final def _cdgIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 8).asInstanceOf[IndexedSeq[StoredNode]]

final def _cfgOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 9).asInstanceOf[IndexedSeq[StoredNode]]
final def _cfgIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 9).asInstanceOf[IndexedSeq[StoredNode]]

final def _conditionOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 10).asInstanceOf[IndexedSeq[StoredNode]]
final def _conditionIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 10).asInstanceOf[IndexedSeq[StoredNode]]

final def _containsOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 11).asInstanceOf[IndexedSeq[StoredNode]]
final def _containsIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 11).asInstanceOf[IndexedSeq[StoredNode]]

final def _dominateOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 12).asInstanceOf[IndexedSeq[StoredNode]]
final def _dominateIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 12).asInstanceOf[IndexedSeq[StoredNode]]

final def _evalTypeOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 13).asInstanceOf[IndexedSeq[StoredNode]]
final def _evalTypeIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 13).asInstanceOf[IndexedSeq[StoredNode]]

final def _importsOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 14).asInstanceOf[IndexedSeq[StoredNode]]
final def _importsIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 14).asInstanceOf[IndexedSeq[StoredNode]]

final def _inheritsFromOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 15).asInstanceOf[IndexedSeq[StoredNode]]
final def _inheritsFromIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 15).asInstanceOf[IndexedSeq[StoredNode]]

final def _isCallForImportOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 16).asInstanceOf[IndexedSeq[StoredNode]]
final def _isCallForImportIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 16).asInstanceOf[IndexedSeq[StoredNode]]

final def _parameterLinkOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 17).asInstanceOf[IndexedSeq[StoredNode]]
final def _parameterLinkIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 17).asInstanceOf[IndexedSeq[StoredNode]]

final def _postDominateOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 18).asInstanceOf[IndexedSeq[StoredNode]]
final def _postDominateIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 18).asInstanceOf[IndexedSeq[StoredNode]]

final def _reachingDefOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 19).asInstanceOf[IndexedSeq[StoredNode]]
final def _reachingDefIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 19).asInstanceOf[IndexedSeq[StoredNode]]

final def _receiverOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 20).asInstanceOf[IndexedSeq[StoredNode]]
final def _receiverIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 20).asInstanceOf[IndexedSeq[StoredNode]]

final def _refOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 21).asInstanceOf[IndexedSeq[StoredNode]]
final def _refIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 21).asInstanceOf[IndexedSeq[StoredNode]]

final def _sourceFileOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 22).asInstanceOf[IndexedSeq[StoredNode]]
final def _sourceFileIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 22).asInstanceOf[IndexedSeq[StoredNode]]

final def _taggedByOut: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsOut(this.graph, this.nodeKind, this.seq, 23).asInstanceOf[IndexedSeq[StoredNode]]
final def _taggedByIn: IndexedSeq[StoredNode] = odb2.Accessors.getNeighborsIn(this.graph, this.nodeKind, this.seq, 23).asInstanceOf[IndexedSeq[StoredNode]]

}

abstract class NewNode(val nodeKind:Short) extends AbstractNode with odb2.DNode {
type RelatedStored <: StoredNode
private /* volatile? */ var _storedRef: RelatedStored = null.asInstanceOf[RelatedStored]
override def storedRef:Option[RelatedStored] = Option(this._storedRef)
override def storedRef_=(stored: Option[odb2.GNode]):Unit = this._storedRef = stored.orNull.asInstanceOf[RelatedStored]
}
