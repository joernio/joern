package io.shiftleft.codepropertygraph.generated.v2
import io.joern.odb2

object Cpg{
  def empty: Cpg = new Cpg(new odb2.Graph(GraphSchema))
}
class Cpg(val graph: odb2.Graph){
assert(graph.schema == GraphSchema)
}

class CpgGeneratedNodeStarters(val wrappedGraph: Cpg) extends AnyVal {
def annotation: Iterator[nodes.Annotation] = wrappedGraph.graph.nodes(0).asInstanceOf[Iterator[nodes.Annotation]]
def annotationLiteral: Iterator[nodes.AnnotationLiteral] = wrappedGraph.graph.nodes(1).asInstanceOf[Iterator[nodes.AnnotationLiteral]]
def annotationParameter: Iterator[nodes.AnnotationParameter] = wrappedGraph.graph.nodes(2).asInstanceOf[Iterator[nodes.AnnotationParameter]]
def annotationParameterAssign: Iterator[nodes.AnnotationParameterAssign] = wrappedGraph.graph.nodes(3).asInstanceOf[Iterator[nodes.AnnotationParameterAssign]]
def arrayInitializer: Iterator[nodes.ArrayInitializer] = wrappedGraph.graph.nodes(4).asInstanceOf[Iterator[nodes.ArrayInitializer]]
def binding: Iterator[nodes.Binding] = wrappedGraph.graph.nodes(5).asInstanceOf[Iterator[nodes.Binding]]
def block: Iterator[nodes.Block] = wrappedGraph.graph.nodes(6).asInstanceOf[Iterator[nodes.Block]]
def call: Iterator[nodes.Call] = wrappedGraph.graph.nodes(7).asInstanceOf[Iterator[nodes.Call]]
def closureBinding: Iterator[nodes.ClosureBinding] = wrappedGraph.graph.nodes(8).asInstanceOf[Iterator[nodes.ClosureBinding]]
def comment: Iterator[nodes.Comment] = wrappedGraph.graph.nodes(9).asInstanceOf[Iterator[nodes.Comment]]
def configFile: Iterator[nodes.ConfigFile] = wrappedGraph.graph.nodes(10).asInstanceOf[Iterator[nodes.ConfigFile]]
def controlStructure: Iterator[nodes.ControlStructure] = wrappedGraph.graph.nodes(11).asInstanceOf[Iterator[nodes.ControlStructure]]
def dependency: Iterator[nodes.Dependency] = wrappedGraph.graph.nodes(12).asInstanceOf[Iterator[nodes.Dependency]]
def fieldIdentifier: Iterator[nodes.FieldIdentifier] = wrappedGraph.graph.nodes(13).asInstanceOf[Iterator[nodes.FieldIdentifier]]
def file: Iterator[nodes.File] = wrappedGraph.graph.nodes(14).asInstanceOf[Iterator[nodes.File]]
def finding: Iterator[nodes.Finding] = wrappedGraph.graph.nodes(15).asInstanceOf[Iterator[nodes.Finding]]
def identifier: Iterator[nodes.Identifier] = wrappedGraph.graph.nodes(16).asInstanceOf[Iterator[nodes.Identifier]]
def imports: Iterator[nodes.Import] = wrappedGraph.graph.nodes(17).asInstanceOf[Iterator[nodes.Import]]
def jumpLabel: Iterator[nodes.JumpLabel] = wrappedGraph.graph.nodes(18).asInstanceOf[Iterator[nodes.JumpLabel]]
def jumpTarget: Iterator[nodes.JumpTarget] = wrappedGraph.graph.nodes(19).asInstanceOf[Iterator[nodes.JumpTarget]]
def keyValuePair: Iterator[nodes.KeyValuePair] = wrappedGraph.graph.nodes(20).asInstanceOf[Iterator[nodes.KeyValuePair]]
def literal: Iterator[nodes.Literal] = wrappedGraph.graph.nodes(21).asInstanceOf[Iterator[nodes.Literal]]
def local: Iterator[nodes.Local] = wrappedGraph.graph.nodes(22).asInstanceOf[Iterator[nodes.Local]]
def location: Iterator[nodes.Location] = wrappedGraph.graph.nodes(23).asInstanceOf[Iterator[nodes.Location]]
def member: Iterator[nodes.Member] = wrappedGraph.graph.nodes(24).asInstanceOf[Iterator[nodes.Member]]
def metaData: Iterator[nodes.MetaData] = wrappedGraph.graph.nodes(25).asInstanceOf[Iterator[nodes.MetaData]]
def method: Iterator[nodes.Method] = wrappedGraph.graph.nodes(26).asInstanceOf[Iterator[nodes.Method]]
def methodParameterIn: Iterator[nodes.MethodParameterIn] = wrappedGraph.graph.nodes(27).asInstanceOf[Iterator[nodes.MethodParameterIn]]
def methodParameterOut: Iterator[nodes.MethodParameterOut] = wrappedGraph.graph.nodes(28).asInstanceOf[Iterator[nodes.MethodParameterOut]]
def methodRef: Iterator[nodes.MethodRef] = wrappedGraph.graph.nodes(29).asInstanceOf[Iterator[nodes.MethodRef]]
def methodReturn: Iterator[nodes.MethodReturn] = wrappedGraph.graph.nodes(30).asInstanceOf[Iterator[nodes.MethodReturn]]
def modifier: Iterator[nodes.Modifier] = wrappedGraph.graph.nodes(31).asInstanceOf[Iterator[nodes.Modifier]]
def namespace: Iterator[nodes.Namespace] = wrappedGraph.graph.nodes(32).asInstanceOf[Iterator[nodes.Namespace]]
def namespaceBlock: Iterator[nodes.NamespaceBlock] = wrappedGraph.graph.nodes(33).asInstanceOf[Iterator[nodes.NamespaceBlock]]
def ret: Iterator[nodes.Return] = wrappedGraph.graph.nodes(34).asInstanceOf[Iterator[nodes.Return]]
def tag: Iterator[nodes.Tag] = wrappedGraph.graph.nodes(35).asInstanceOf[Iterator[nodes.Tag]]
def tagNodePair: Iterator[nodes.TagNodePair] = wrappedGraph.graph.nodes(36).asInstanceOf[Iterator[nodes.TagNodePair]]
def templateDom: Iterator[nodes.TemplateDom] = wrappedGraph.graph.nodes(37).asInstanceOf[Iterator[nodes.TemplateDom]]
def typ: Iterator[nodes.Type] = wrappedGraph.graph.nodes(38).asInstanceOf[Iterator[nodes.Type]]
def typeArgument: Iterator[nodes.TypeArgument] = wrappedGraph.graph.nodes(39).asInstanceOf[Iterator[nodes.TypeArgument]]
def typeDecl: Iterator[nodes.TypeDecl] = wrappedGraph.graph.nodes(40).asInstanceOf[Iterator[nodes.TypeDecl]]
def typeParameter: Iterator[nodes.TypeParameter] = wrappedGraph.graph.nodes(41).asInstanceOf[Iterator[nodes.TypeParameter]]
def typeRef: Iterator[nodes.TypeRef] = wrappedGraph.graph.nodes(42).asInstanceOf[Iterator[nodes.TypeRef]]
def unknown: Iterator[nodes.Unknown] = wrappedGraph.graph.nodes(43).asInstanceOf[Iterator[nodes.Unknown]]

def astNode: Iterator[nodes.AstNode] = Iterator(this.annotation, this.annotationLiteral, this.annotationParameter, this.annotationParameterAssign, this.arrayInitializer, this.block, this.call, this.comment, this.controlStructure, this.fieldIdentifier, this.file, this.identifier, this.imports, this.jumpLabel, this.jumpTarget, this.literal, this.local, this.member, this.method, this.methodParameterIn, this.methodParameterOut, this.methodRef, this.methodReturn, this.modifier, this.namespace, this.namespaceBlock, this.ret, this.templateDom, this.typeArgument, this.typeDecl, this.typeParameter, this.typeRef, this.unknown).flatten
def callRepr: Iterator[nodes.CallRepr] = Iterator(this.call).flatten
def cfgNode: Iterator[nodes.CfgNode] = Iterator(this.annotation, this.annotationLiteral, this.arrayInitializer, this.block, this.call, this.controlStructure, this.fieldIdentifier, this.identifier, this.jumpTarget, this.literal, this.method, this.methodParameterIn, this.methodParameterOut, this.methodRef, this.methodReturn, this.ret, this.templateDom, this.typeRef, this.unknown).flatten
def declaration: Iterator[nodes.Declaration] = Iterator(this.local, this.member, this.method, this.methodParameterIn, this.methodParameterOut).flatten
def expression: Iterator[nodes.Expression] = Iterator(this.annotation, this.annotationLiteral, this.arrayInitializer, this.block, this.call, this.controlStructure, this.fieldIdentifier, this.identifier, this.literal, this.methodRef, this.ret, this.templateDom, this.typeRef, this.unknown).flatten
}
