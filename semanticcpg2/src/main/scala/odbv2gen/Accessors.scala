package io.shiftleft.codepropertygraph.generated.v2.accessors
import io.joern.odb2
import io.shiftleft.codepropertygraph.generated.v2.nodes
import scala.collection.immutable.IndexedSeq

object Lang extends ConcreteStoredConversions {}

object Accessors {
  final class Access_Property_ALIAS_TYPE_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def aliasTypeFullName: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 0, node.seq)
}
final class Access_Property_ARGUMENT_INDEX(val node: nodes.StoredNode) extends AnyVal {
  def argumentIndex: Int  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 1, node.seq(), -1: Int)
}
final class Access_Property_ARGUMENT_NAME(val node: nodes.StoredNode) extends AnyVal {
  def argumentName: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 2, node.seq)
}
final class Access_Property_AST_PARENT_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def astParentFullName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 3, node.seq(), "<empty>": String)
}
final class Access_Property_AST_PARENT_TYPE(val node: nodes.StoredNode) extends AnyVal {
  def astParentType: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 4, node.seq(), "<empty>": String)
}
final class Access_Property_CANONICAL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def canonicalName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 5, node.seq(), "<empty>": String)
}
final class Access_Property_CLASS_NAME(val node: nodes.StoredNode) extends AnyVal {
  def className: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 6, node.seq(), "<empty>": String)
}
final class Access_Property_CLASS_SHORT_NAME(val node: nodes.StoredNode) extends AnyVal {
  def classShortName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 7, node.seq(), "<empty>": String)
}
final class Access_Property_CLOSURE_BINDING_ID(val node: nodes.StoredNode) extends AnyVal {
  def closureBindingId: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 8, node.seq)
}
final class Access_Property_CLOSURE_ORIGINAL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def closureOriginalName: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 9, node.seq)
}
final class Access_Property_CODE(val node: nodes.StoredNode) extends AnyVal {
  def code: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 10, node.seq(), "<empty>": String)
}
final class Access_Property_COLUMN_NUMBER(val node: nodes.StoredNode) extends AnyVal {
  def columnNumber: Option[Int]  = odb2.Accessors.getNodePropertyOption[Int](node.graph, node.nodeKind, 11, node.seq)
}
final class Access_Property_COLUMN_NUMBER_END(val node: nodes.StoredNode) extends AnyVal {
  def columnNumberEnd: Option[Int]  = odb2.Accessors.getNodePropertyOption[Int](node.graph, node.nodeKind, 12, node.seq)
}
final class Access_Property_CONTAINED_REF(val node: nodes.StoredNode) extends AnyVal {
  def containedRef: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 13, node.seq(), "<empty>": String)
}
final class Access_Property_CONTENT(val node: nodes.StoredNode) extends AnyVal {
  def content: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 14, node.seq(), "<empty>": String)
}
final class Access_Property_CONTROL_STRUCTURE_TYPE(val node: nodes.StoredNode) extends AnyVal {
  def controlStructureType: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 15, node.seq(), "<empty>": String)
}
final class Access_Property_DEPENDENCY_GROUP_ID(val node: nodes.StoredNode) extends AnyVal {
  def dependencyGroupId: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 16, node.seq)
}
final class Access_Property_DISPATCH_TYPE(val node: nodes.StoredNode) extends AnyVal {
  def dispatchType: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 17, node.seq(), "<empty>": String)
}
final class Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def dynamicTypeHintFullName: IndexedSeq[String]  = odb2.Accessors.getNodePropertyMulti[String](node.graph, node.nodeKind, 18, node.seq)
}
final class Access_Property_EVALUATION_STRATEGY(val node: nodes.StoredNode) extends AnyVal {
  def evaluationStrategy: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 19, node.seq(), "<empty>": String)
}
final class Access_Property_EXPLICIT_AS(val node: nodes.StoredNode) extends AnyVal {
  def explicitAs: Option[Boolean]  = odb2.Accessors.getNodePropertyOption[Boolean](node.graph, node.nodeKind, 20, node.seq)
}
final class Access_Property_FILENAME(val node: nodes.StoredNode) extends AnyVal {
  def filename: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 21, node.seq(), "<empty>": String)
}
final class Access_Property_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def fullName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 22, node.seq(), "<empty>": String)
}
final class Access_Property_HASH(val node: nodes.StoredNode) extends AnyVal {
  def hash: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 23, node.seq)
}
final class Access_Property_IMPORTED_AS(val node: nodes.StoredNode) extends AnyVal {
  def importedAs: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 24, node.seq)
}
final class Access_Property_IMPORTED_ENTITY(val node: nodes.StoredNode) extends AnyVal {
  def importedEntity: Option[String]  = odb2.Accessors.getNodePropertyOption[String](node.graph, node.nodeKind, 25, node.seq)
}
final class Access_Property_INDEX(val node: nodes.StoredNode) extends AnyVal {
  def index: Int  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 26, node.seq(), -1: Int)
}
final class Access_Property_INHERITS_FROM_TYPE_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def inheritsFromTypeFullName: IndexedSeq[String]  = odb2.Accessors.getNodePropertyMulti[String](node.graph, node.nodeKind, 27, node.seq)
}
final class Access_Property_IS_EXPLICIT(val node: nodes.StoredNode) extends AnyVal {
  def isExplicit: Option[Boolean]  = odb2.Accessors.getNodePropertyOption[Boolean](node.graph, node.nodeKind, 28, node.seq)
}
final class Access_Property_IS_EXTERNAL(val node: nodes.StoredNode) extends AnyVal {
  def isExternal: Boolean  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 29, node.seq(), false: Boolean)
}
final class Access_Property_IS_VARIADIC(val node: nodes.StoredNode) extends AnyVal {
  def isVariadic: Boolean  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 30, node.seq(), false: Boolean)
}
final class Access_Property_IS_WILDCARD(val node: nodes.StoredNode) extends AnyVal {
  def isWildcard: Option[Boolean]  = odb2.Accessors.getNodePropertyOption[Boolean](node.graph, node.nodeKind, 31, node.seq)
}
final class Access_Property_KEY(val node: nodes.StoredNode) extends AnyVal {
  def key: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 32, node.seq(), "<empty>": String)
}
final class Access_Property_LANGUAGE(val node: nodes.StoredNode) extends AnyVal {
  def language: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 33, node.seq(), "<empty>": String)
}
final class Access_Property_LINE_NUMBER(val node: nodes.StoredNode) extends AnyVal {
  def lineNumber: Option[Int]  = odb2.Accessors.getNodePropertyOption[Int](node.graph, node.nodeKind, 34, node.seq)
}
final class Access_Property_LINE_NUMBER_END(val node: nodes.StoredNode) extends AnyVal {
  def lineNumberEnd: Option[Int]  = odb2.Accessors.getNodePropertyOption[Int](node.graph, node.nodeKind, 35, node.seq)
}
final class Access_Property_METHOD_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def methodFullName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 36, node.seq(), "<empty>": String)
}
final class Access_Property_METHOD_SHORT_NAME(val node: nodes.StoredNode) extends AnyVal {
  def methodShortName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 37, node.seq(), "<empty>": String)
}
final class Access_Property_MODIFIER_TYPE(val node: nodes.StoredNode) extends AnyVal {
  def modifierType: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 38, node.seq(), "<empty>": String)
}
final class Access_Property_NAME(val node: nodes.StoredNode) extends AnyVal {
  def name: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 39, node.seq(), "<empty>": String)
}
final class Access_Property_NODE_LABEL(val node: nodes.StoredNode) extends AnyVal {
  def nodeLabel: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 40, node.seq(), "<empty>": String)
}
final class Access_Property_ORDER(val node: nodes.StoredNode) extends AnyVal {
  def order: Int  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 41, node.seq(), -1: Int)
}
final class Access_Property_OVERLAYS(val node: nodes.StoredNode) extends AnyVal {
  def overlays: IndexedSeq[String]  = odb2.Accessors.getNodePropertyMulti[String](node.graph, node.nodeKind, 42, node.seq)
}
final class Access_Property_PACKAGE_NAME(val node: nodes.StoredNode) extends AnyVal {
  def packageName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 43, node.seq(), "<empty>": String)
}
final class Access_Property_PARSER_TYPE_NAME(val node: nodes.StoredNode) extends AnyVal {
  def parserTypeName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 44, node.seq(), "<empty>": String)
}
final class Access_Property_POSSIBLE_TYPES(val node: nodes.StoredNode) extends AnyVal {
  def possibleTypes: IndexedSeq[String]  = odb2.Accessors.getNodePropertyMulti[String](node.graph, node.nodeKind, 45, node.seq)
}
final class Access_Property_ROOT(val node: nodes.StoredNode) extends AnyVal {
  def root: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 46, node.seq(), "<empty>": String)
}
final class Access_Property_SIGNATURE(val node: nodes.StoredNode) extends AnyVal {
  def signature: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 47, node.seq(), "": String)
}
final class Access_Property_SYMBOL(val node: nodes.StoredNode) extends AnyVal {
  def symbol: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 48, node.seq(), "<empty>": String)
}
final class Access_Property_TYPE_DECL_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def typeDeclFullName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 49, node.seq(), "<empty>": String)
}
final class Access_Property_TYPE_FULL_NAME(val node: nodes.StoredNode) extends AnyVal {
  def typeFullName: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 50, node.seq(), "<empty>": String)
}
final class Access_Property_VALUE(val node: nodes.StoredNode) extends AnyVal {
  def value: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 51, node.seq(), "": String)
}
final class Access_Property_VERSION(val node: nodes.StoredNode) extends AnyVal {
  def version: String  = odb2.Accessors.getNodePropertySingle(node.graph, node.nodeKind, 52, node.seq(), "<empty>": String)
}
  //
  final class Access_AnnotationBase(val node: nodes.AnnotationBase) extends AnyVal {
def fullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FULL_NAME(stored).fullName
 case newNode: nodes.NewAnnotation => newNode.fullName
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewAnnotation => newNode.name
}
}
final class Access_AnnotationLiteralBase(val node: nodes.AnnotationLiteralBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewAnnotationLiteral => newNode.name
}
}
final class Access_AnnotationParameterBase(val node: nodes.AnnotationParameterBase) extends AnyVal {

}
final class Access_AnnotationParameterAssignBase(val node: nodes.AnnotationParameterAssignBase) extends AnyVal {

}
final class Access_ArrayInitializerBase(val node: nodes.ArrayInitializerBase) extends AnyVal {

}
final class Access_BindingBase(val node: nodes.BindingBase) extends AnyVal {
def methodFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_METHOD_FULL_NAME(stored).methodFullName
 case newNode: nodes.NewBinding => newNode.methodFullName
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewBinding => newNode.name
}
def signature: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_SIGNATURE(stored).signature
 case newNode: nodes.NewBinding => newNode.signature
}
}
final class Access_BlockBase(val node: nodes.BlockBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewBlock => newNode.dynamicTypeHintFullName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewBlock => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewBlock => newNode.typeFullName
}
}
final class Access_CallBase(val node: nodes.CallBase) extends AnyVal {
def dispatchType: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_DISPATCH_TYPE(stored).dispatchType
 case newNode: nodes.NewCall => newNode.dispatchType
}
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewCall => newNode.dynamicTypeHintFullName
}
def methodFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_METHOD_FULL_NAME(stored).methodFullName
 case newNode: nodes.NewCall => newNode.methodFullName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewCall => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewCall => newNode.typeFullName
}
}
final class Access_ClosureBindingBase(val node: nodes.ClosureBindingBase) extends AnyVal {
def closureBindingId: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_CLOSURE_BINDING_ID(stored).closureBindingId
 case newNode: nodes.NewClosureBinding => newNode.closureBindingId
}
def closureOriginalName: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_CLOSURE_ORIGINAL_NAME(stored).closureOriginalName
 case newNode: nodes.NewClosureBinding => newNode.closureOriginalName
}
def evaluationStrategy: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_EVALUATION_STRATEGY(stored).evaluationStrategy
 case newNode: nodes.NewClosureBinding => newNode.evaluationStrategy
}
}
final class Access_CommentBase(val node: nodes.CommentBase) extends AnyVal {
def filename: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FILENAME(stored).filename
 case newNode: nodes.NewComment => newNode.filename
}
}
final class Access_ConfigFileBase(val node: nodes.ConfigFileBase) extends AnyVal {
def content: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_CONTENT(stored).content
 case newNode: nodes.NewConfigFile => newNode.content
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewConfigFile => newNode.name
}
}
final class Access_ControlStructureBase(val node: nodes.ControlStructureBase) extends AnyVal {
def controlStructureType: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_CONTROL_STRUCTURE_TYPE(stored).controlStructureType
 case newNode: nodes.NewControlStructure => newNode.controlStructureType
}
def parserTypeName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_PARSER_TYPE_NAME(stored).parserTypeName
 case newNode: nodes.NewControlStructure => newNode.parserTypeName
}
}
final class Access_DependencyBase(val node: nodes.DependencyBase) extends AnyVal {
def dependencyGroupId: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DEPENDENCY_GROUP_ID(stored).dependencyGroupId
 case newNode: nodes.NewDependency => newNode.dependencyGroupId
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewDependency => newNode.name
}
def version: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_VERSION(stored).version
 case newNode: nodes.NewDependency => newNode.version
}
}
final class Access_FieldIdentifierBase(val node: nodes.FieldIdentifierBase) extends AnyVal {
def canonicalName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_CANONICAL_NAME(stored).canonicalName
 case newNode: nodes.NewFieldIdentifier => newNode.canonicalName
}
}
final class Access_FileBase(val node: nodes.FileBase) extends AnyVal {
def hash: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_HASH(stored).hash
 case newNode: nodes.NewFile => newNode.hash
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewFile => newNode.name
}
}
final class Access_FindingBase(val node: nodes.FindingBase) extends AnyVal {

}
final class Access_IdentifierBase(val node: nodes.IdentifierBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewIdentifier => newNode.dynamicTypeHintFullName
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewIdentifier => newNode.name
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewIdentifier => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewIdentifier => newNode.typeFullName
}
}
final class Access_ImportBase(val node: nodes.ImportBase) extends AnyVal {
def explicitAs: Option[Boolean]  = node match {
 case stored: nodes.StoredNode => new Access_Property_EXPLICIT_AS(stored).explicitAs
 case newNode: nodes.NewImport => newNode.explicitAs
}
def importedAs: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_IMPORTED_AS(stored).importedAs
 case newNode: nodes.NewImport => newNode.importedAs
}
def importedEntity: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_IMPORTED_ENTITY(stored).importedEntity
 case newNode: nodes.NewImport => newNode.importedEntity
}
def isExplicit: Option[Boolean]  = node match {
 case stored: nodes.StoredNode => new Access_Property_IS_EXPLICIT(stored).isExplicit
 case newNode: nodes.NewImport => newNode.isExplicit
}
def isWildcard: Option[Boolean]  = node match {
 case stored: nodes.StoredNode => new Access_Property_IS_WILDCARD(stored).isWildcard
 case newNode: nodes.NewImport => newNode.isWildcard
}
}
final class Access_JumpLabelBase(val node: nodes.JumpLabelBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewJumpLabel => newNode.name
}
def parserTypeName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_PARSER_TYPE_NAME(stored).parserTypeName
 case newNode: nodes.NewJumpLabel => newNode.parserTypeName
}
}
final class Access_JumpTargetBase(val node: nodes.JumpTargetBase) extends AnyVal {
def argumentIndex: Int  = node match {
 case stored: nodes.StoredNode => new Access_Property_ARGUMENT_INDEX(stored).argumentIndex
 case newNode: nodes.NewJumpTarget => newNode.argumentIndex
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewJumpTarget => newNode.name
}
def parserTypeName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_PARSER_TYPE_NAME(stored).parserTypeName
 case newNode: nodes.NewJumpTarget => newNode.parserTypeName
}
}
final class Access_KeyValuePairBase(val node: nodes.KeyValuePairBase) extends AnyVal {
def key: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_KEY(stored).key
 case newNode: nodes.NewKeyValuePair => newNode.key
}
def value: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_VALUE(stored).value
 case newNode: nodes.NewKeyValuePair => newNode.value
}
}
final class Access_LiteralBase(val node: nodes.LiteralBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewLiteral => newNode.dynamicTypeHintFullName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewLiteral => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewLiteral => newNode.typeFullName
}
}
final class Access_LocalBase(val node: nodes.LocalBase) extends AnyVal {
def closureBindingId: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_CLOSURE_BINDING_ID(stored).closureBindingId
 case newNode: nodes.NewLocal => newNode.closureBindingId
}
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewLocal => newNode.dynamicTypeHintFullName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewLocal => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewLocal => newNode.typeFullName
}
}
final class Access_LocationBase(val node: nodes.LocationBase) extends AnyVal {
def className: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_CLASS_NAME(stored).className
 case newNode: nodes.NewLocation => newNode.className
}
def classShortName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_CLASS_SHORT_NAME(stored).classShortName
 case newNode: nodes.NewLocation => newNode.classShortName
}
def filename: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FILENAME(stored).filename
 case newNode: nodes.NewLocation => newNode.filename
}
def lineNumber: Option[Int]  = node match {
 case stored: nodes.StoredNode => new Access_Property_LINE_NUMBER(stored).lineNumber
 case newNode: nodes.NewLocation => newNode.lineNumber
}
def methodFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_METHOD_FULL_NAME(stored).methodFullName
 case newNode: nodes.NewLocation => newNode.methodFullName
}
def methodShortName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_METHOD_SHORT_NAME(stored).methodShortName
 case newNode: nodes.NewLocation => newNode.methodShortName
}
def nodeLabel: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NODE_LABEL(stored).nodeLabel
 case newNode: nodes.NewLocation => newNode.nodeLabel
}
def packageName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_PACKAGE_NAME(stored).packageName
 case newNode: nodes.NewLocation => newNode.packageName
}
def symbol: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_SYMBOL(stored).symbol
 case newNode: nodes.NewLocation => newNode.symbol
}
}
final class Access_MemberBase(val node: nodes.MemberBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewMember => newNode.dynamicTypeHintFullName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewMember => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewMember => newNode.typeFullName
}
}
final class Access_MetaDataBase(val node: nodes.MetaDataBase) extends AnyVal {
def hash: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_HASH(stored).hash
 case newNode: nodes.NewMetaData => newNode.hash
}
def language: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_LANGUAGE(stored).language
 case newNode: nodes.NewMetaData => newNode.language
}
def overlays: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_OVERLAYS(stored).overlays
 case newNode: nodes.NewMetaData => newNode.overlays
}
def root: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_ROOT(stored).root
 case newNode: nodes.NewMetaData => newNode.root
}
def version: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_VERSION(stored).version
 case newNode: nodes.NewMetaData => newNode.version
}
}
final class Access_MethodBase(val node: nodes.MethodBase) extends AnyVal {
def astParentFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_AST_PARENT_FULL_NAME(stored).astParentFullName
 case newNode: nodes.NewMethod => newNode.astParentFullName
}
def astParentType: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_AST_PARENT_TYPE(stored).astParentType
 case newNode: nodes.NewMethod => newNode.astParentType
}
def columnNumberEnd: Option[Int]  = node match {
 case stored: nodes.StoredNode => new Access_Property_COLUMN_NUMBER_END(stored).columnNumberEnd
 case newNode: nodes.NewMethod => newNode.columnNumberEnd
}
def filename: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FILENAME(stored).filename
 case newNode: nodes.NewMethod => newNode.filename
}
def fullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FULL_NAME(stored).fullName
 case newNode: nodes.NewMethod => newNode.fullName
}
def hash: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_HASH(stored).hash
 case newNode: nodes.NewMethod => newNode.hash
}
def isExternal: Boolean  = node match {
 case stored: nodes.StoredNode => new Access_Property_IS_EXTERNAL(stored).isExternal
 case newNode: nodes.NewMethod => newNode.isExternal
}
def lineNumberEnd: Option[Int]  = node match {
 case stored: nodes.StoredNode => new Access_Property_LINE_NUMBER_END(stored).lineNumberEnd
 case newNode: nodes.NewMethod => newNode.lineNumberEnd
}
def signature: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_SIGNATURE(stored).signature
 case newNode: nodes.NewMethod => newNode.signature
}
}
final class Access_MethodParameterInBase(val node: nodes.MethodParameterInBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewMethodParameterIn => newNode.dynamicTypeHintFullName
}
def evaluationStrategy: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_EVALUATION_STRATEGY(stored).evaluationStrategy
 case newNode: nodes.NewMethodParameterIn => newNode.evaluationStrategy
}
def index: Int  = node match {
 case stored: nodes.StoredNode => new Access_Property_INDEX(stored).index
 case newNode: nodes.NewMethodParameterIn => newNode.index
}
def isVariadic: Boolean  = node match {
 case stored: nodes.StoredNode => new Access_Property_IS_VARIADIC(stored).isVariadic
 case newNode: nodes.NewMethodParameterIn => newNode.isVariadic
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewMethodParameterIn => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewMethodParameterIn => newNode.typeFullName
}
}
final class Access_MethodParameterOutBase(val node: nodes.MethodParameterOutBase) extends AnyVal {
def evaluationStrategy: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_EVALUATION_STRATEGY(stored).evaluationStrategy
 case newNode: nodes.NewMethodParameterOut => newNode.evaluationStrategy
}
def index: Int  = node match {
 case stored: nodes.StoredNode => new Access_Property_INDEX(stored).index
 case newNode: nodes.NewMethodParameterOut => newNode.index
}
def isVariadic: Boolean  = node match {
 case stored: nodes.StoredNode => new Access_Property_IS_VARIADIC(stored).isVariadic
 case newNode: nodes.NewMethodParameterOut => newNode.isVariadic
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewMethodParameterOut => newNode.typeFullName
}
}
final class Access_MethodRefBase(val node: nodes.MethodRefBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewMethodRef => newNode.dynamicTypeHintFullName
}
def methodFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_METHOD_FULL_NAME(stored).methodFullName
 case newNode: nodes.NewMethodRef => newNode.methodFullName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewMethodRef => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewMethodRef => newNode.typeFullName
}
}
final class Access_MethodReturnBase(val node: nodes.MethodReturnBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewMethodReturn => newNode.dynamicTypeHintFullName
}
def evaluationStrategy: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_EVALUATION_STRATEGY(stored).evaluationStrategy
 case newNode: nodes.NewMethodReturn => newNode.evaluationStrategy
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewMethodReturn => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewMethodReturn => newNode.typeFullName
}
}
final class Access_ModifierBase(val node: nodes.ModifierBase) extends AnyVal {
def modifierType: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_MODIFIER_TYPE(stored).modifierType
 case newNode: nodes.NewModifier => newNode.modifierType
}
}
final class Access_NamespaceBase(val node: nodes.NamespaceBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewNamespace => newNode.name
}
}
final class Access_NamespaceBlockBase(val node: nodes.NamespaceBlockBase) extends AnyVal {
def filename: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FILENAME(stored).filename
 case newNode: nodes.NewNamespaceBlock => newNode.filename
}
def fullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FULL_NAME(stored).fullName
 case newNode: nodes.NewNamespaceBlock => newNode.fullName
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewNamespaceBlock => newNode.name
}
}
final class Access_ReturnBase(val node: nodes.ReturnBase) extends AnyVal {

}
final class Access_TagBase(val node: nodes.TagBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewTag => newNode.name
}
def value: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_VALUE(stored).value
 case newNode: nodes.NewTag => newNode.value
}
}
final class Access_TagNodePairBase(val node: nodes.TagNodePairBase) extends AnyVal {

}
final class Access_TemplateDomBase(val node: nodes.TemplateDomBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewTemplateDom => newNode.name
}
}
final class Access_TypeBase(val node: nodes.TypeBase) extends AnyVal {
def fullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FULL_NAME(stored).fullName
 case newNode: nodes.NewType => newNode.fullName
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewType => newNode.name
}
def typeDeclFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_DECL_FULL_NAME(stored).typeDeclFullName
 case newNode: nodes.NewType => newNode.typeDeclFullName
}
}
final class Access_TypeArgumentBase(val node: nodes.TypeArgumentBase) extends AnyVal {

}
final class Access_TypeDeclBase(val node: nodes.TypeDeclBase) extends AnyVal {
def aliasTypeFullName: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_ALIAS_TYPE_FULL_NAME(stored).aliasTypeFullName
 case newNode: nodes.NewTypeDecl => newNode.aliasTypeFullName
}
def astParentFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_AST_PARENT_FULL_NAME(stored).astParentFullName
 case newNode: nodes.NewTypeDecl => newNode.astParentFullName
}
def astParentType: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_AST_PARENT_TYPE(stored).astParentType
 case newNode: nodes.NewTypeDecl => newNode.astParentType
}
def filename: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FILENAME(stored).filename
 case newNode: nodes.NewTypeDecl => newNode.filename
}
def fullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_FULL_NAME(stored).fullName
 case newNode: nodes.NewTypeDecl => newNode.fullName
}
def inheritsFromTypeFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_INHERITS_FROM_TYPE_FULL_NAME(stored).inheritsFromTypeFullName
 case newNode: nodes.NewTypeDecl => newNode.inheritsFromTypeFullName
}
def isExternal: Boolean  = node match {
 case stored: nodes.StoredNode => new Access_Property_IS_EXTERNAL(stored).isExternal
 case newNode: nodes.NewTypeDecl => newNode.isExternal
}
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewTypeDecl => newNode.name
}
}
final class Access_TypeParameterBase(val node: nodes.TypeParameterBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.NewTypeParameter => newNode.name
}
}
final class Access_TypeRefBase(val node: nodes.TypeRefBase) extends AnyVal {
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewTypeRef => newNode.dynamicTypeHintFullName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewTypeRef => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewTypeRef => newNode.typeFullName
}
}
final class Access_UnknownBase(val node: nodes.UnknownBase) extends AnyVal {
def containedRef: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_CONTAINED_REF(stored).containedRef
 case newNode: nodes.NewUnknown => newNode.containedRef
}
def dynamicTypeHintFullName: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(stored).dynamicTypeHintFullName
 case newNode: nodes.NewUnknown => newNode.dynamicTypeHintFullName
}
def parserTypeName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_PARSER_TYPE_NAME(stored).parserTypeName
 case newNode: nodes.NewUnknown => newNode.parserTypeName
}
def possibleTypes: IndexedSeq[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_POSSIBLE_TYPES(stored).possibleTypes
 case newNode: nodes.NewUnknown => newNode.possibleTypes
}
def typeFullName: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_TYPE_FULL_NAME(stored).typeFullName
 case newNode: nodes.NewUnknown => newNode.typeFullName
}
}
final class Access_AstNodeBase(val node: nodes.AstNodeBase) extends AnyVal {
def code: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_CODE(stored).code
 case newNode: nodes.AstNodeNew => newNode.code
}
def columnNumber: Option[Int]  = node match {
 case stored: nodes.StoredNode => new Access_Property_COLUMN_NUMBER(stored).columnNumber
 case newNode: nodes.AstNodeNew => newNode.columnNumber
}
def lineNumber: Option[Int]  = node match {
 case stored: nodes.StoredNode => new Access_Property_LINE_NUMBER(stored).lineNumber
 case newNode: nodes.AstNodeNew => newNode.lineNumber
}
def order: Int  = node match {
 case stored: nodes.StoredNode => new Access_Property_ORDER(stored).order
 case newNode: nodes.AstNodeNew => newNode.order
}
}
final class Access_CallReprBase(val node: nodes.CallReprBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.CallReprNew => newNode.name
}
def signature: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_SIGNATURE(stored).signature
 case newNode: nodes.CallReprNew => newNode.signature
}
}
final class Access_CfgNodeBase(val node: nodes.CfgNodeBase) extends AnyVal {

}
final class Access_ExpressionBase(val node: nodes.ExpressionBase) extends AnyVal {
def argumentIndex: Int  = node match {
 case stored: nodes.StoredNode => new Access_Property_ARGUMENT_INDEX(stored).argumentIndex
 case newNode: nodes.ExpressionNew => newNode.argumentIndex
}
def argumentName: Option[String]  = node match {
 case stored: nodes.StoredNode => new Access_Property_ARGUMENT_NAME(stored).argumentName
 case newNode: nodes.ExpressionNew => newNode.argumentName
}
}
final class Access_DeclarationBase(val node: nodes.DeclarationBase) extends AnyVal {
def name: String  = node match {
 case stored: nodes.StoredNode => new Access_Property_NAME(stored).name
 case newNode: nodes.DeclarationNew => newNode.name
}
}
}

trait ConcreteStoredConversions  extends ConcreteBaseConversions {
import Accessors._
implicit def accessPropertyAliasTypeFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasAliasTypeFullNameT]): Access_Property_ALIAS_TYPE_FULL_NAME = new Access_Property_ALIAS_TYPE_FULL_NAME(node)
implicit def accessPropertyArgumentIndex(node: nodes.StoredNode with nodes.StaticType[nodes.HasArgumentIndexT]): Access_Property_ARGUMENT_INDEX = new Access_Property_ARGUMENT_INDEX(node)
implicit def accessPropertyArgumentName(node: nodes.StoredNode with nodes.StaticType[nodes.HasArgumentNameT]): Access_Property_ARGUMENT_NAME = new Access_Property_ARGUMENT_NAME(node)
implicit def accessPropertyAstParentFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasAstParentFullNameT]): Access_Property_AST_PARENT_FULL_NAME = new Access_Property_AST_PARENT_FULL_NAME(node)
implicit def accessPropertyAstParentType(node: nodes.StoredNode with nodes.StaticType[nodes.HasAstParentTypeT]): Access_Property_AST_PARENT_TYPE = new Access_Property_AST_PARENT_TYPE(node)
implicit def accessPropertyCanonicalName(node: nodes.StoredNode with nodes.StaticType[nodes.HasCanonicalNameT]): Access_Property_CANONICAL_NAME = new Access_Property_CANONICAL_NAME(node)
implicit def accessPropertyClassName(node: nodes.StoredNode with nodes.StaticType[nodes.HasClassNameT]): Access_Property_CLASS_NAME = new Access_Property_CLASS_NAME(node)
implicit def accessPropertyClassShortName(node: nodes.StoredNode with nodes.StaticType[nodes.HasClassShortNameT]): Access_Property_CLASS_SHORT_NAME = new Access_Property_CLASS_SHORT_NAME(node)
implicit def accessPropertyClosureBindingId(node: nodes.StoredNode with nodes.StaticType[nodes.HasClosureBindingIdT]): Access_Property_CLOSURE_BINDING_ID = new Access_Property_CLOSURE_BINDING_ID(node)
implicit def accessPropertyClosureOriginalName(node: nodes.StoredNode with nodes.StaticType[nodes.HasClosureOriginalNameT]): Access_Property_CLOSURE_ORIGINAL_NAME = new Access_Property_CLOSURE_ORIGINAL_NAME(node)
implicit def accessPropertyCode(node: nodes.StoredNode with nodes.StaticType[nodes.HasCodeT]): Access_Property_CODE = new Access_Property_CODE(node)
implicit def accessPropertyColumnNumber(node: nodes.StoredNode with nodes.StaticType[nodes.HasColumnNumberT]): Access_Property_COLUMN_NUMBER = new Access_Property_COLUMN_NUMBER(node)
implicit def accessPropertyColumnNumberEnd(node: nodes.StoredNode with nodes.StaticType[nodes.HasColumnNumberEndT]): Access_Property_COLUMN_NUMBER_END = new Access_Property_COLUMN_NUMBER_END(node)
implicit def accessPropertyContainedRef(node: nodes.StoredNode with nodes.StaticType[nodes.HasContainedRefT]): Access_Property_CONTAINED_REF = new Access_Property_CONTAINED_REF(node)
implicit def accessPropertyContent(node: nodes.StoredNode with nodes.StaticType[nodes.HasContentT]): Access_Property_CONTENT = new Access_Property_CONTENT(node)
implicit def accessPropertyControlStructureType(node: nodes.StoredNode with nodes.StaticType[nodes.HasControlStructureTypeT]): Access_Property_CONTROL_STRUCTURE_TYPE = new Access_Property_CONTROL_STRUCTURE_TYPE(node)
implicit def accessPropertyDependencyGroupId(node: nodes.StoredNode with nodes.StaticType[nodes.HasDependencyGroupIdT]): Access_Property_DEPENDENCY_GROUP_ID = new Access_Property_DEPENDENCY_GROUP_ID(node)
implicit def accessPropertyDispatchType(node: nodes.StoredNode with nodes.StaticType[nodes.HasDispatchTypeT]): Access_Property_DISPATCH_TYPE = new Access_Property_DISPATCH_TYPE(node)
implicit def accessPropertyDynamicTypeHintFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasDynamicTypeHintFullNameT]): Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME = new Access_Property_DYNAMIC_TYPE_HINT_FULL_NAME(node)
implicit def accessPropertyEvaluationStrategy(node: nodes.StoredNode with nodes.StaticType[nodes.HasEvaluationStrategyT]): Access_Property_EVALUATION_STRATEGY = new Access_Property_EVALUATION_STRATEGY(node)
implicit def accessPropertyExplicitAs(node: nodes.StoredNode with nodes.StaticType[nodes.HasExplicitAsT]): Access_Property_EXPLICIT_AS = new Access_Property_EXPLICIT_AS(node)
implicit def accessPropertyFilename(node: nodes.StoredNode with nodes.StaticType[nodes.HasFilenameT]): Access_Property_FILENAME = new Access_Property_FILENAME(node)
implicit def accessPropertyFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasFullNameT]): Access_Property_FULL_NAME = new Access_Property_FULL_NAME(node)
implicit def accessPropertyHash(node: nodes.StoredNode with nodes.StaticType[nodes.HasHashT]): Access_Property_HASH = new Access_Property_HASH(node)
implicit def accessPropertyImportedAs(node: nodes.StoredNode with nodes.StaticType[nodes.HasImportedAsT]): Access_Property_IMPORTED_AS = new Access_Property_IMPORTED_AS(node)
implicit def accessPropertyImportedEntity(node: nodes.StoredNode with nodes.StaticType[nodes.HasImportedEntityT]): Access_Property_IMPORTED_ENTITY = new Access_Property_IMPORTED_ENTITY(node)
implicit def accessPropertyIndex(node: nodes.StoredNode with nodes.StaticType[nodes.HasIndexT]): Access_Property_INDEX = new Access_Property_INDEX(node)
implicit def accessPropertyInheritsFromTypeFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasInheritsFromTypeFullNameT]): Access_Property_INHERITS_FROM_TYPE_FULL_NAME = new Access_Property_INHERITS_FROM_TYPE_FULL_NAME(node)
implicit def accessPropertyIsExplicit(node: nodes.StoredNode with nodes.StaticType[nodes.HasIsExplicitT]): Access_Property_IS_EXPLICIT = new Access_Property_IS_EXPLICIT(node)
implicit def accessPropertyIsExternal(node: nodes.StoredNode with nodes.StaticType[nodes.HasIsExternalT]): Access_Property_IS_EXTERNAL = new Access_Property_IS_EXTERNAL(node)
implicit def accessPropertyIsVariadic(node: nodes.StoredNode with nodes.StaticType[nodes.HasIsVariadicT]): Access_Property_IS_VARIADIC = new Access_Property_IS_VARIADIC(node)
implicit def accessPropertyIsWildcard(node: nodes.StoredNode with nodes.StaticType[nodes.HasIsWildcardT]): Access_Property_IS_WILDCARD = new Access_Property_IS_WILDCARD(node)
implicit def accessPropertyKey(node: nodes.StoredNode with nodes.StaticType[nodes.HasKeyT]): Access_Property_KEY = new Access_Property_KEY(node)
implicit def accessPropertyLanguage(node: nodes.StoredNode with nodes.StaticType[nodes.HasLanguageT]): Access_Property_LANGUAGE = new Access_Property_LANGUAGE(node)
implicit def accessPropertyLineNumber(node: nodes.StoredNode with nodes.StaticType[nodes.HasLineNumberT]): Access_Property_LINE_NUMBER = new Access_Property_LINE_NUMBER(node)
implicit def accessPropertyLineNumberEnd(node: nodes.StoredNode with nodes.StaticType[nodes.HasLineNumberEndT]): Access_Property_LINE_NUMBER_END = new Access_Property_LINE_NUMBER_END(node)
implicit def accessPropertyMethodFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasMethodFullNameT]): Access_Property_METHOD_FULL_NAME = new Access_Property_METHOD_FULL_NAME(node)
implicit def accessPropertyMethodShortName(node: nodes.StoredNode with nodes.StaticType[nodes.HasMethodShortNameT]): Access_Property_METHOD_SHORT_NAME = new Access_Property_METHOD_SHORT_NAME(node)
implicit def accessPropertyModifierType(node: nodes.StoredNode with nodes.StaticType[nodes.HasModifierTypeT]): Access_Property_MODIFIER_TYPE = new Access_Property_MODIFIER_TYPE(node)
implicit def accessPropertyName(node: nodes.StoredNode with nodes.StaticType[nodes.HasNameT]): Access_Property_NAME = new Access_Property_NAME(node)
implicit def accessPropertyNodeLabel(node: nodes.StoredNode with nodes.StaticType[nodes.HasNodeLabelT]): Access_Property_NODE_LABEL = new Access_Property_NODE_LABEL(node)
implicit def accessPropertyOrder(node: nodes.StoredNode with nodes.StaticType[nodes.HasOrderT]): Access_Property_ORDER = new Access_Property_ORDER(node)
implicit def accessPropertyOverlays(node: nodes.StoredNode with nodes.StaticType[nodes.HasOverlaysT]): Access_Property_OVERLAYS = new Access_Property_OVERLAYS(node)
implicit def accessPropertyPackageName(node: nodes.StoredNode with nodes.StaticType[nodes.HasPackageNameT]): Access_Property_PACKAGE_NAME = new Access_Property_PACKAGE_NAME(node)
implicit def accessPropertyParserTypeName(node: nodes.StoredNode with nodes.StaticType[nodes.HasParserTypeNameT]): Access_Property_PARSER_TYPE_NAME = new Access_Property_PARSER_TYPE_NAME(node)
implicit def accessPropertyPossibleTypes(node: nodes.StoredNode with nodes.StaticType[nodes.HasPossibleTypesT]): Access_Property_POSSIBLE_TYPES = new Access_Property_POSSIBLE_TYPES(node)
implicit def accessPropertyRoot(node: nodes.StoredNode with nodes.StaticType[nodes.HasRootT]): Access_Property_ROOT = new Access_Property_ROOT(node)
implicit def accessPropertySignature(node: nodes.StoredNode with nodes.StaticType[nodes.HasSignatureT]): Access_Property_SIGNATURE = new Access_Property_SIGNATURE(node)
implicit def accessPropertySymbol(node: nodes.StoredNode with nodes.StaticType[nodes.HasSymbolT]): Access_Property_SYMBOL = new Access_Property_SYMBOL(node)
implicit def accessPropertyTypeDeclFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasTypeDeclFullNameT]): Access_Property_TYPE_DECL_FULL_NAME = new Access_Property_TYPE_DECL_FULL_NAME(node)
implicit def accessPropertyTypeFullName(node: nodes.StoredNode with nodes.StaticType[nodes.HasTypeFullNameT]): Access_Property_TYPE_FULL_NAME = new Access_Property_TYPE_FULL_NAME(node)
implicit def accessPropertyValue(node: nodes.StoredNode with nodes.StaticType[nodes.HasValueT]): Access_Property_VALUE = new Access_Property_VALUE(node)
implicit def accessPropertyVersion(node: nodes.StoredNode with nodes.StaticType[nodes.HasVersionT]): Access_Property_VERSION = new Access_Property_VERSION(node)
}

trait ConcreteBaseConversions  extends AbstractBaseConversions0 {
import Accessors._
implicit def access_AnnotationBase(node: nodes.AnnotationBase): Access_AnnotationBase = new Access_AnnotationBase(node)
implicit def access_AnnotationLiteralBase(node: nodes.AnnotationLiteralBase): Access_AnnotationLiteralBase = new Access_AnnotationLiteralBase(node)
implicit def access_AnnotationParameterBase(node: nodes.AnnotationParameterBase): Access_AnnotationParameterBase = new Access_AnnotationParameterBase(node)
implicit def access_AnnotationParameterAssignBase(node: nodes.AnnotationParameterAssignBase): Access_AnnotationParameterAssignBase = new Access_AnnotationParameterAssignBase(node)
implicit def access_ArrayInitializerBase(node: nodes.ArrayInitializerBase): Access_ArrayInitializerBase = new Access_ArrayInitializerBase(node)
implicit def access_BindingBase(node: nodes.BindingBase): Access_BindingBase = new Access_BindingBase(node)
implicit def access_BlockBase(node: nodes.BlockBase): Access_BlockBase = new Access_BlockBase(node)
implicit def access_CallBase(node: nodes.CallBase): Access_CallBase = new Access_CallBase(node)
implicit def access_ClosureBindingBase(node: nodes.ClosureBindingBase): Access_ClosureBindingBase = new Access_ClosureBindingBase(node)
implicit def access_CommentBase(node: nodes.CommentBase): Access_CommentBase = new Access_CommentBase(node)
implicit def access_ConfigFileBase(node: nodes.ConfigFileBase): Access_ConfigFileBase = new Access_ConfigFileBase(node)
implicit def access_ControlStructureBase(node: nodes.ControlStructureBase): Access_ControlStructureBase = new Access_ControlStructureBase(node)
implicit def access_DependencyBase(node: nodes.DependencyBase): Access_DependencyBase = new Access_DependencyBase(node)
implicit def access_FieldIdentifierBase(node: nodes.FieldIdentifierBase): Access_FieldIdentifierBase = new Access_FieldIdentifierBase(node)
implicit def access_FileBase(node: nodes.FileBase): Access_FileBase = new Access_FileBase(node)
implicit def access_FindingBase(node: nodes.FindingBase): Access_FindingBase = new Access_FindingBase(node)
implicit def access_IdentifierBase(node: nodes.IdentifierBase): Access_IdentifierBase = new Access_IdentifierBase(node)
implicit def access_ImportBase(node: nodes.ImportBase): Access_ImportBase = new Access_ImportBase(node)
implicit def access_JumpLabelBase(node: nodes.JumpLabelBase): Access_JumpLabelBase = new Access_JumpLabelBase(node)
implicit def access_JumpTargetBase(node: nodes.JumpTargetBase): Access_JumpTargetBase = new Access_JumpTargetBase(node)
implicit def access_KeyValuePairBase(node: nodes.KeyValuePairBase): Access_KeyValuePairBase = new Access_KeyValuePairBase(node)
implicit def access_LiteralBase(node: nodes.LiteralBase): Access_LiteralBase = new Access_LiteralBase(node)
implicit def access_LocalBase(node: nodes.LocalBase): Access_LocalBase = new Access_LocalBase(node)
implicit def access_LocationBase(node: nodes.LocationBase): Access_LocationBase = new Access_LocationBase(node)
implicit def access_MemberBase(node: nodes.MemberBase): Access_MemberBase = new Access_MemberBase(node)
implicit def access_MetaDataBase(node: nodes.MetaDataBase): Access_MetaDataBase = new Access_MetaDataBase(node)
implicit def access_MethodBase(node: nodes.MethodBase): Access_MethodBase = new Access_MethodBase(node)
implicit def access_MethodParameterInBase(node: nodes.MethodParameterInBase): Access_MethodParameterInBase = new Access_MethodParameterInBase(node)
implicit def access_MethodParameterOutBase(node: nodes.MethodParameterOutBase): Access_MethodParameterOutBase = new Access_MethodParameterOutBase(node)
implicit def access_MethodRefBase(node: nodes.MethodRefBase): Access_MethodRefBase = new Access_MethodRefBase(node)
implicit def access_MethodReturnBase(node: nodes.MethodReturnBase): Access_MethodReturnBase = new Access_MethodReturnBase(node)
implicit def access_ModifierBase(node: nodes.ModifierBase): Access_ModifierBase = new Access_ModifierBase(node)
implicit def access_NamespaceBase(node: nodes.NamespaceBase): Access_NamespaceBase = new Access_NamespaceBase(node)
implicit def access_NamespaceBlockBase(node: nodes.NamespaceBlockBase): Access_NamespaceBlockBase = new Access_NamespaceBlockBase(node)
implicit def access_ReturnBase(node: nodes.ReturnBase): Access_ReturnBase = new Access_ReturnBase(node)
implicit def access_TagBase(node: nodes.TagBase): Access_TagBase = new Access_TagBase(node)
implicit def access_TagNodePairBase(node: nodes.TagNodePairBase): Access_TagNodePairBase = new Access_TagNodePairBase(node)
implicit def access_TemplateDomBase(node: nodes.TemplateDomBase): Access_TemplateDomBase = new Access_TemplateDomBase(node)
implicit def access_TypeBase(node: nodes.TypeBase): Access_TypeBase = new Access_TypeBase(node)
implicit def access_TypeArgumentBase(node: nodes.TypeArgumentBase): Access_TypeArgumentBase = new Access_TypeArgumentBase(node)
implicit def access_TypeDeclBase(node: nodes.TypeDeclBase): Access_TypeDeclBase = new Access_TypeDeclBase(node)
implicit def access_TypeParameterBase(node: nodes.TypeParameterBase): Access_TypeParameterBase = new Access_TypeParameterBase(node)
implicit def access_TypeRefBase(node: nodes.TypeRefBase): Access_TypeRefBase = new Access_TypeRefBase(node)
implicit def access_UnknownBase(node: nodes.UnknownBase): Access_UnknownBase = new Access_UnknownBase(node)
}

trait AbstractBaseConversions0  extends AbstractBaseConversions1 {
import Accessors._
implicit def access_AstNodeBase(node: nodes.AstNodeBase): Access_AstNodeBase = new Access_AstNodeBase(node)
implicit def access_CallReprBase(node: nodes.CallReprBase): Access_CallReprBase = new Access_CallReprBase(node)
implicit def access_CfgNodeBase(node: nodes.CfgNodeBase): Access_CfgNodeBase = new Access_CfgNodeBase(node)
implicit def access_ExpressionBase(node: nodes.ExpressionBase): Access_ExpressionBase = new Access_ExpressionBase(node)
}

trait AbstractBaseConversions1  {
import Accessors._
implicit def access_DeclarationBase(node: nodes.DeclarationBase): Access_DeclarationBase = new Access_DeclarationBase(node)
}
