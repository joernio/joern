package io.shiftleft.codepropertygraph.generated.v2.nodes
import io.joern.odb2


trait AstNodeT extends AnyRef with HasCodeT with HasColumnNumberT with HasLineNumberT with HasOrderT

trait AstNodeBase extends AbstractNode with StaticType[AstNodeT]
 // new properties: CODE, COLUMN_NUMBER, LINE_NUMBER, ORDER
 // inherited properties: 
 // inherited interfaces: 
 // implementing nodes: ANNOTATION, ANNOTATION_LITERAL, ANNOTATION_PARAMETER, ANNOTATION_PARAMETER_ASSIGN, ARRAY_INITIALIZER, BLOCK, CALL, COMMENT, CONTROL_STRUCTURE, FIELD_IDENTIFIER, FILE, IDENTIFIER, IMPORT, JUMP_LABEL, JUMP_TARGET, LITERAL, LOCAL, MEMBER, METHOD, METHOD_PARAMETER_IN, METHOD_PARAMETER_OUT, METHOD_REF, METHOD_RETURN, MODIFIER, NAMESPACE, NAMESPACE_BLOCK, RETURN, TEMPLATE_DOM, TYPE_ARGUMENT, TYPE_DECL, TYPE_PARAMETER, TYPE_REF, UNKNOWN
trait AstNode extends StoredNode with AstNodeBase with StaticType[AstNodeT]

trait AstNodeNew extends NewNode with AstNodeBase with StaticType[AstNodeT]{
  type RelatedStored <:  AstNode
  def code: String
def code_=(value: String): Unit
def code(value: String): this.type
def columnNumber: Option[Int]
def columnNumber_=(value: Option[Int]): Unit
def columnNumber(value: Option[Int]): this.type
def columnNumber(value: Int): this.type
def lineNumber: Option[Int]
def lineNumber_=(value: Option[Int]): Unit
def lineNumber(value: Option[Int]): this.type
def lineNumber(value: Int): this.type
def order: Int
def order_=(value: Int): Unit
def order(value: Int): this.type
}


trait CallReprT extends AnyRef with CfgNodeT with HasNameT with HasSignatureT

trait CallReprBase extends AbstractNode with CfgNodeBase with StaticType[CallReprT]
 // new properties: NAME, SIGNATURE
 // inherited properties: CODE, COLUMN_NUMBER, LINE_NUMBER, ORDER
 // inherited interfaces: AST_NODE
 // implementing nodes: CALL
trait CallRepr extends StoredNode with CallReprBase with CfgNode with StaticType[CallReprT]

trait CallReprNew extends NewNode with CallReprBase with CfgNodeNew with StaticType[CallReprT]{
  type RelatedStored <:  CallRepr
  def name: String
def name_=(value: String): Unit
def name(value: String): this.type
def signature: String
def signature_=(value: String): Unit
def signature(value: String): this.type
}


trait CfgNodeT extends AnyRef with AstNodeT

trait CfgNodeBase extends AbstractNode with AstNodeBase with StaticType[CfgNodeT]
 // new properties: 
 // inherited properties: CODE, COLUMN_NUMBER, LINE_NUMBER, ORDER
 // inherited interfaces: 
 // implementing nodes: ANNOTATION, ANNOTATION_LITERAL, ARRAY_INITIALIZER, BLOCK, CALL, CONTROL_STRUCTURE, FIELD_IDENTIFIER, IDENTIFIER, JUMP_TARGET, LITERAL, METHOD, METHOD_PARAMETER_IN, METHOD_PARAMETER_OUT, METHOD_REF, METHOD_RETURN, RETURN, TEMPLATE_DOM, TYPE_REF, UNKNOWN
trait CfgNode extends StoredNode with CfgNodeBase with AstNode with StaticType[CfgNodeT]

trait CfgNodeNew extends NewNode with CfgNodeBase with AstNodeNew with StaticType[CfgNodeT]{
  type RelatedStored <:  CfgNode
  
}


trait DeclarationT extends AnyRef with HasNameT

trait DeclarationBase extends AbstractNode with StaticType[DeclarationT]
 // new properties: NAME
 // inherited properties: 
 // inherited interfaces: 
 // implementing nodes: LOCAL, MEMBER, METHOD, METHOD_PARAMETER_IN, METHOD_PARAMETER_OUT
trait Declaration extends StoredNode with DeclarationBase with StaticType[DeclarationT]

trait DeclarationNew extends NewNode with DeclarationBase with StaticType[DeclarationT]{
  type RelatedStored <:  Declaration
  def name: String
def name_=(value: String): Unit
def name(value: String): this.type
}


trait ExpressionT extends AnyRef with CfgNodeT with HasArgumentIndexT with HasArgumentNameT

trait ExpressionBase extends AbstractNode with CfgNodeBase with StaticType[ExpressionT]
 // new properties: ARGUMENT_INDEX, ARGUMENT_NAME
 // inherited properties: CODE, COLUMN_NUMBER, LINE_NUMBER, ORDER
 // inherited interfaces: AST_NODE
 // implementing nodes: ANNOTATION, ANNOTATION_LITERAL, ARRAY_INITIALIZER, BLOCK, CALL, CONTROL_STRUCTURE, FIELD_IDENTIFIER, IDENTIFIER, LITERAL, METHOD_REF, RETURN, TEMPLATE_DOM, TYPE_REF, UNKNOWN
trait Expression extends StoredNode with ExpressionBase with CfgNode with StaticType[ExpressionT]

trait ExpressionNew extends NewNode with ExpressionBase with AstNodeNew with CfgNodeNew with StaticType[ExpressionT]{
  type RelatedStored <:  Expression
  def argumentIndex: Int
def argumentIndex_=(value: Int): Unit
def argumentIndex(value: Int): this.type
def argumentName: Option[String]
def argumentName_=(value: Option[String]): Unit
def argumentName(value: Option[String]): this.type
def argumentName(value: String): this.type
}


trait HasAliasTypeFullNameT
trait HasArgumentIndexT
trait HasArgumentNameT
trait HasAstParentFullNameT
trait HasAstParentTypeT
trait HasCanonicalNameT
trait HasClassNameT
trait HasClassShortNameT
trait HasClosureBindingIdT
trait HasClosureOriginalNameT
trait HasCodeT
trait HasColumnNumberT
trait HasColumnNumberEndT
trait HasContainedRefT
trait HasContentT
trait HasControlStructureTypeT
trait HasDependencyGroupIdT
trait HasDispatchTypeT
trait HasDynamicTypeHintFullNameT
trait HasEvaluationStrategyT
trait HasExplicitAsT
trait HasFilenameT
trait HasFullNameT
trait HasHashT
trait HasImportedAsT
trait HasImportedEntityT
trait HasIndexT
trait HasInheritsFromTypeFullNameT
trait HasIsExplicitT
trait HasIsExternalT
trait HasIsVariadicT
trait HasIsWildcardT
trait HasKeyT
trait HasLanguageT
trait HasLineNumberT
trait HasLineNumberEndT
trait HasMethodFullNameT
trait HasMethodShortNameT
trait HasModifierTypeT
trait HasNameT
trait HasNodeLabelT
trait HasOrderT
trait HasOverlaysT
trait HasPackageNameT
trait HasParserTypeNameT
trait HasPossibleTypesT
trait HasRootT
trait HasSignatureT
trait HasSymbolT
trait HasTypeDeclFullNameT
trait HasTypeFullNameT
trait HasValueT
trait HasVersionT
