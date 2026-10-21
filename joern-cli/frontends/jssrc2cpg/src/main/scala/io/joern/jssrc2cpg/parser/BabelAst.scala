package io.joern.jssrc2cpg.parser

object BabelAst {

  object Unknown extends BabelNode

  private val nodeMap: Map[String, BabelNode] = Map(
    "AnyTypeAnnotation"               -> AnyTypeAnnotation,
    "ArgumentPlaceholder"             -> ArgumentPlaceholder,
    "ArrayExpression"                 -> ArrayExpression,
    "ArrayPattern"                    -> ArrayPattern,
    "ArrayTypeAnnotation"             -> ArrayTypeAnnotation,
    "ArrowFunctionExpression"         -> ArrowFunctionExpression,
    "AssignmentExpression"            -> AssignmentExpression,
    "AssignmentPattern"               -> AssignmentPattern,
    "AwaitExpression"                 -> AwaitExpression,
    "BigIntLiteral"                   -> BigIntLiteral,
    "BinaryExpression"                -> BinaryExpression,
    "BindExpression"                  -> BindExpression,
    "BlockStatement"                  -> BlockStatement,
    "BooleanLiteral"                  -> BooleanLiteral,
    "BooleanLiteralTypeAnnotation"    -> BooleanLiteralTypeAnnotation,
    "BooleanTypeAnnotation"           -> BooleanTypeAnnotation,
    "BreakStatement"                  -> BreakStatement,
    "CallExpression"                  -> CallExpression,
    "CatchClause"                     -> CatchClause,
    "ClassAccessorProperty"           -> ClassAccessorProperty,
    "ClassBody"                       -> ClassBody,
    "ClassDeclaration"                -> ClassDeclaration,
    "ClassExpression"                 -> ClassExpression,
    "ClassImplements"                 -> ClassImplements,
    "ClassMethod"                     -> ClassMethod,
    "ClassPrivateMethod"              -> ClassPrivateMethod,
    "ClassPrivateProperty"            -> ClassPrivateProperty,
    "ClassProperty"                   -> ClassProperty,
    "ConditionalExpression"           -> ConditionalExpression,
    "ContinueStatement"               -> ContinueStatement,
    "DebuggerStatement"               -> DebuggerStatement,
    "DecimalLiteral"                  -> DecimalLiteral,
    "DeclareClass"                    -> DeclareClass,
    "DeclareExportAllDeclaration"     -> DeclareExportAllDeclaration,
    "DeclareExportDeclaration"        -> DeclareExportDeclaration,
    "DeclareFunction"                 -> DeclareFunction,
    "DeclareInterface"                -> DeclareInterface,
    "DeclareModule"                   -> DeclareModule,
    "DeclareModuleExports"            -> DeclareModuleExports,
    "DeclareOpaqueType"               -> DeclareOpaqueType,
    "DeclareTypeAlias"                -> DeclareTypeAlias,
    "DeclareVariable"                 -> DeclareVariable,
    "DeclaredPredicate"               -> DeclaredPredicate,
    "Decorator"                       -> Decorator,
    "Directive"                       -> Directive,
    "DirectiveLiteral"                -> DirectiveLiteral,
    "DoExpression"                    -> DoExpression,
    "DoWhileStatement"                -> DoWhileStatement,
    "EmptyStatement"                  -> EmptyStatement,
    "EmptyTypeAnnotation"             -> EmptyTypeAnnotation,
    "EnumBooleanBody"                 -> EnumBooleanBody,
    "EnumBooleanMember"               -> EnumBooleanMember,
    "EnumDeclaration"                 -> EnumDeclaration,
    "EnumDefaultedMember"             -> EnumDefaultedMember,
    "EnumNumberBody"                  -> EnumNumberBody,
    "EnumNumberMember"                -> EnumNumberMember,
    "EnumStringBody"                  -> EnumStringBody,
    "EnumStringMember"                -> EnumStringMember,
    "EnumSymbolBody"                  -> EnumSymbolBody,
    "ExistsTypeAnnotation"            -> ExistsTypeAnnotation,
    "ExportAllDeclaration"            -> ExportAllDeclaration,
    "ExportDefaultDeclaration"        -> ExportDefaultDeclaration,
    "ExportDefaultSpecifier"          -> ExportDefaultSpecifier,
    "ExportNamedDeclaration"          -> ExportNamedDeclaration,
    "ExportNamespaceSpecifier"        -> ExportNamespaceSpecifier,
    "ExportSpecifier"                 -> ExportSpecifier,
    "ExpressionStatement"             -> ExpressionStatement,
    "File"                            -> File,
    "ForInStatement"                  -> ForInStatement,
    "ForOfStatement"                  -> ForOfStatement,
    "ForStatement"                    -> ForStatement,
    "FunctionDeclaration"             -> FunctionDeclaration,
    "FunctionExpression"              -> FunctionExpression,
    "FunctionTypeAnnotation"          -> FunctionTypeAnnotation,
    "FunctionTypeParam"               -> FunctionTypeParam,
    "GenericTypeAnnotation"           -> GenericTypeAnnotation,
    "Identifier"                      -> Identifier,
    "IfStatement"                     -> IfStatement,
    "Import"                          -> Import,
    "ImportAttribute"                 -> ImportAttribute,
    "ImportDeclaration"               -> ImportDeclaration,
    "ImportDefaultSpecifier"          -> ImportDefaultSpecifier,
    "ImportNamespaceSpecifier"        -> ImportNamespaceSpecifier,
    "ImportSpecifier"                 -> ImportSpecifier,
    "IndexedAccessType"               -> IndexedAccessType,
    "InferredPredicate"               -> InferredPredicate,
    "InterfaceDeclaration"            -> InterfaceDeclaration,
    "InterfaceExtends"                -> InterfaceExtends,
    "InterfaceTypeAnnotation"         -> InterfaceTypeAnnotation,
    "InterpreterDirective"            -> InterpreterDirective,
    "IntersectionTypeAnnotation"      -> IntersectionTypeAnnotation,
    "JSXAttribute"                    -> JSXAttribute,
    "JSXClosingElement"               -> JSXClosingElement,
    "JSXClosingFragment"              -> JSXClosingFragment,
    "JSXElement"                      -> JSXElement,
    "JSXEmptyExpression"              -> JSXEmptyExpression,
    "JSXExpressionContainer"          -> JSXExpressionContainer,
    "JSXFragment"                     -> JSXFragment,
    "JSXIdentifier"                   -> JSXIdentifier,
    "JSXMemberExpression"             -> JSXMemberExpression,
    "JSXNamespacedName"               -> JSXNamespacedName,
    "JSXOpeningElement"               -> JSXOpeningElement,
    "JSXOpeningFragment"              -> JSXOpeningFragment,
    "JSXSpreadAttribute"              -> JSXSpreadAttribute,
    "JSXSpreadChild"                  -> JSXSpreadChild,
    "JSXText"                         -> JSXText,
    "LabeledStatement"                -> LabeledStatement,
    "LogicalExpression"               -> LogicalExpression,
    "MemberExpression"                -> MemberExpression,
    "MetaProperty"                    -> MetaProperty,
    "MixedTypeAnnotation"             -> MixedTypeAnnotation,
    "ModuleExpression"                -> ModuleExpression,
    "NewExpression"                   -> NewExpression,
    "Noop"                            -> Noop,
    "NullLiteral"                     -> NullLiteral,
    "NullLiteralTypeAnnotation"       -> NullLiteralTypeAnnotation,
    "NullableTypeAnnotation"          -> NullableTypeAnnotation,
    "NumberLiteral"                   -> NumberLiteral,
    "NumberLiteralTypeAnnotation"     -> NumberLiteralTypeAnnotation,
    "NumberTypeAnnotation"            -> NumberTypeAnnotation,
    "NumericLiteral"                  -> NumericLiteral,
    "ObjectExpression"                -> ObjectExpression,
    "ObjectMethod"                    -> ObjectMethod,
    "ObjectPattern"                   -> ObjectPattern,
    "ObjectProperty"                  -> ObjectProperty,
    "ObjectTypeAnnotation"            -> ObjectTypeAnnotation,
    "ObjectTypeCallProperty"          -> ObjectTypeCallProperty,
    "ObjectTypeIndexer"               -> ObjectTypeIndexer,
    "ObjectTypeInternalSlot"          -> ObjectTypeInternalSlot,
    "ObjectTypeProperty"              -> ObjectTypeProperty,
    "ObjectTypeSpreadProperty"        -> ObjectTypeSpreadProperty,
    "OpaqueType"                      -> OpaqueType,
    "OptionalCallExpression"          -> OptionalCallExpression,
    "OptionalIndexedAccessType"       -> OptionalIndexedAccessType,
    "OptionalMemberExpression"        -> OptionalMemberExpression,
    "ParenthesizedExpression"         -> ParenthesizedExpression,
    "PipelineBareFunction"            -> PipelineBareFunction,
    "PipelinePrimaryTopicReference"   -> PipelinePrimaryTopicReference,
    "PipelineTopicExpression"         -> PipelineTopicExpression,
    "Placeholder"                     -> Placeholder,
    "PrivateName"                     -> PrivateName,
    "Program"                         -> Program,
    "QualifiedTypeIdentifier"         -> QualifiedTypeIdentifier,
    "RecordExpression"                -> RecordExpression,
    "RegExpLiteral"                   -> RegExpLiteral,
    "RegexLiteral"                    -> RegexLiteral,
    "RestElement"                     -> RestElement,
    "RestProperty"                    -> RestProperty,
    "ReturnStatement"                 -> ReturnStatement,
    "SequenceExpression"              -> SequenceExpression,
    "SpreadElement"                   -> SpreadElement,
    "SpreadProperty"                  -> SpreadProperty,
    "StaticBlock"                     -> StaticBlock,
    "StringLiteral"                   -> StringLiteral,
    "StringLiteralTypeAnnotation"     -> StringLiteralTypeAnnotation,
    "StringTypeAnnotation"            -> StringTypeAnnotation,
    "Super"                           -> Super,
    "SwitchCase"                      -> SwitchCase,
    "SwitchStatement"                 -> SwitchStatement,
    "SymbolTypeAnnotation"            -> SymbolTypeAnnotation,
    "TSAnyKeyword"                    -> TSAnyKeyword,
    "TSArrayType"                     -> TSArrayType,
    "TSAsExpression"                  -> TSAsExpression,
    "TSBigIntKeyword"                 -> TSBigIntKeyword,
    "TSBooleanKeyword"                -> TSBooleanKeyword,
    "TSCallSignatureDeclaration"      -> TSCallSignatureDeclaration,
    "TSConditionalType"               -> TSConditionalType,
    "TSConstructSignatureDeclaration" -> TSConstructSignatureDeclaration,
    "TSConstructorType"               -> TSConstructorType,
    "TSDeclareFunction"               -> TSDeclareFunction,
    "TSDeclareMethod"                 -> TSDeclareMethod,
    "TSEnumDeclaration"               -> TSEnumDeclaration,
    "TSEnumMember"                    -> TSEnumMember,
    "TSExportAssignment"              -> TSExportAssignment,
    "TSExpressionWithTypeArguments"   -> TSExpressionWithTypeArguments,
    "TSExternalModuleReference"       -> TSExternalModuleReference,
    "TSFunctionType"                  -> TSFunctionType,
    "TSImportEqualsDeclaration"       -> TSImportEqualsDeclaration,
    "TSImportType"                    -> TSImportType,
    "TSIndexSignature"                -> TSIndexSignature,
    "TSIndexedAccessType"             -> TSIndexedAccessType,
    "TSInferType"                     -> TSInferType,
    "TSInstantiationExpression"       -> TSInstantiationExpression,
    "TSInterfaceBody"                 -> TSInterfaceBody,
    "TSInterfaceDeclaration"          -> TSInterfaceDeclaration,
    "TSIntersectionType"              -> TSIntersectionType,
    "TSIntrinsicKeyword"              -> TSIntrinsicKeyword,
    "TSLiteralType"                   -> TSLiteralType,
    "TSMappedType"                    -> TSMappedType,
    "TSMethodSignature"               -> TSMethodSignature,
    "TSModuleBlock"                   -> TSModuleBlock,
    "TSModuleDeclaration"             -> TSModuleDeclaration,
    "TSNamedTupleMember"              -> TSNamedTupleMember,
    "TSNamespaceExportDeclaration"    -> TSNamespaceExportDeclaration,
    "TSNeverKeyword"                  -> TSNeverKeyword,
    "TSNonNullExpression"             -> TSNonNullExpression,
    "TSNullKeyword"                   -> TSNullKeyword,
    "TSNumberKeyword"                 -> TSNumberKeyword,
    "TSObjectKeyword"                 -> TSObjectKeyword,
    "TSOptionalType"                  -> TSOptionalType,
    "TSParameterProperty"             -> TSParameterProperty,
    "TSParenthesizedType"             -> TSParenthesizedType,
    "TSPropertySignature"             -> TSPropertySignature,
    "TSQualifiedName"                 -> TSQualifiedName,
    "TSRestType"                      -> TSRestType,
    "TSSatisfiesExpression"           -> TSSatisfiesExpression,
    "TSStringKeyword"                 -> TSStringKeyword,
    "TSSymbolKeyword"                 -> TSSymbolKeyword,
    "TSThisType"                      -> TSThisType,
    "TSTupleType"                     -> TSTupleType,
    "TSTypeAliasDeclaration"          -> TSTypeAliasDeclaration,
    "TSTypeAnnotation"                -> TSTypeAnnotation,
    "TSTypeAssertion"                 -> TSTypeAssertion,
    "TSTypeExpression"                -> TSTypeExpression,
    "TSTypeLiteral"                   -> TSTypeLiteral,
    "TSTypeOperator"                  -> TSTypeOperator,
    "TSTypeParameter"                 -> TSTypeParameter,
    "TSTypeParameterDeclaration"      -> TSTypeParameterDeclaration,
    "TSTypeParameterInstantiation"    -> TSTypeParameterInstantiation,
    "TSTypePredicate"                 -> TSTypePredicate,
    "TSTypeQuery"                     -> TSTypeQuery,
    "TSTypeReference"                 -> TSTypeReference,
    "TSUndefinedKeyword"              -> TSUndefinedKeyword,
    "TSUnionType"                     -> TSUnionType,
    "TSUnknownKeyword"                -> TSUnknownKeyword,
    "TSVoidKeyword"                   -> TSVoidKeyword,
    "TaggedTemplateExpression"        -> TaggedTemplateExpression,
    "TemplateElement"                 -> TemplateElement,
    "TemplateLiteral"                 -> TemplateLiteral,
    "ThisExpression"                  -> ThisExpression,
    "ThisTypeAnnotation"              -> ThisTypeAnnotation,
    "ThrowStatement"                  -> ThrowStatement,
    "TopicReference"                  -> TopicReference,
    "TryStatement"                    -> TryStatement,
    "TupleExpression"                 -> TupleExpression,
    "TupleTypeAnnotation"             -> TupleTypeAnnotation,
    "TypeAlias"                       -> TypeAlias,
    "TypeAnnotation"                  -> TypeAnnotation,
    "TypeCastExpression"              -> TypeCastExpression,
    "TSTypeCastExpression"            -> TSTypeCastExpression,
    "TypeParameter"                   -> TypeParameter,
    "TypeParameterDeclaration"        -> TypeParameterDeclaration,
    "TypeParameterInstantiation"      -> TypeParameterInstantiation,
    "TypeofTypeAnnotation"            -> TypeofTypeAnnotation,
    "UnaryExpression"                 -> UnaryExpression,
    "UnionTypeAnnotation"             -> UnionTypeAnnotation,
    "UpdateExpression"                -> UpdateExpression,
    "V8IntrinsicIdentifier"           -> V8IntrinsicIdentifier,
    "VariableDeclaration"             -> VariableDeclaration,
    "VariableDeclarator"              -> VariableDeclarator,
    "Variance"                        -> Variance,
    "VoidTypeAnnotation"              -> VoidTypeAnnotation,
    "WhileStatement"                  -> WhileStatement,
    "WithStatement"                   -> WithStatement,
    "YieldExpression"                 -> YieldExpression
  )

  def fromString(nodeName: String): BabelNode =
    nodeMap.getOrElse(nodeName, Unknown)

  // extracted from:
  // https://github.com/babel/babel/blob/main/packages/babel-types/src/ast-types/generated/index.ts

  sealed trait BabelNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  sealed trait FlowType extends BabelNode

  sealed trait TSType extends BabelNode

  sealed trait Expression extends BabelNode

  sealed trait FunctionLike extends BabelNode

  object AnyTypeAnnotation               extends FlowType
  object ArgumentPlaceholder             extends BabelNode
  object ArrayExpression                 extends BabelNode
  object ArrayPattern                    extends BabelNode
  object ArrayTypeAnnotation             extends FlowType
  object ArrowFunctionExpression         extends FunctionLike
  object AssignmentExpression            extends BabelNode
  object AssignmentPattern               extends BabelNode
  object AwaitExpression                 extends BabelNode
  object BigIntLiteral                   extends BabelNode
  object BinaryExpression                extends BabelNode
  object BindExpression                  extends BabelNode
  object BlockStatement                  extends BabelNode
  object BooleanLiteral                  extends BabelNode
  object BooleanLiteralTypeAnnotation    extends FlowType
  object BooleanTypeAnnotation           extends FlowType
  object BreakStatement                  extends BabelNode
  object CallExpression                  extends Expression
  object CatchClause                     extends BabelNode
  object ClassAccessorProperty           extends BabelNode
  object ClassBody                       extends BabelNode
  object ClassDeclaration                extends BabelNode
  object ClassExpression                 extends BabelNode
  object ClassImplements                 extends BabelNode
  object ClassMethod                     extends BabelNode
  object ClassPrivateMethod              extends BabelNode
  object ClassPrivateProperty            extends BabelNode
  object ClassProperty                   extends BabelNode
  object ConditionalExpression           extends BabelNode
  object ContinueStatement               extends BabelNode
  object DebuggerStatement               extends BabelNode
  object DecimalLiteral                  extends BabelNode
  object DeclareClass                    extends BabelNode
  object DeclareExportAllDeclaration     extends BabelNode
  object DeclareExportDeclaration        extends BabelNode
  object DeclareFunction                 extends BabelNode
  object DeclareInterface                extends BabelNode
  object DeclareModule                   extends BabelNode
  object DeclareModuleExports            extends BabelNode
  object DeclareOpaqueType               extends BabelNode
  object DeclareTypeAlias                extends BabelNode
  object DeclareVariable                 extends BabelNode
  object DeclaredPredicate               extends BabelNode
  object Decorator                       extends BabelNode
  object Directive                       extends BabelNode
  object DirectiveLiteral                extends BabelNode
  object DoExpression                    extends BabelNode
  object DoWhileStatement                extends BabelNode
  object EmptyStatement                  extends BabelNode
  object EmptyTypeAnnotation             extends FlowType
  object EnumBooleanBody                 extends BabelNode
  object EnumBooleanMember               extends BabelNode
  object EnumDeclaration                 extends BabelNode
  object EnumDefaultedMember             extends BabelNode
  object EnumNumberBody                  extends BabelNode
  object EnumNumberMember                extends BabelNode
  object EnumStringBody                  extends BabelNode
  object EnumStringMember                extends BabelNode
  object EnumSymbolBody                  extends BabelNode
  object ExistsTypeAnnotation            extends FlowType
  object ExportAllDeclaration            extends BabelNode
  object ExportDefaultDeclaration        extends BabelNode
  object ExportDefaultSpecifier          extends BabelNode
  object ExportNamedDeclaration          extends BabelNode
  object ExportNamespaceSpecifier        extends BabelNode
  object ExportSpecifier                 extends BabelNode
  object ExpressionStatement             extends BabelNode
  object File                            extends BabelNode
  object ForInStatement                  extends BabelNode
  object ForOfStatement                  extends BabelNode
  object ForStatement                    extends BabelNode
  object FunctionDeclaration             extends FunctionLike
  object FunctionExpression              extends FunctionLike
  object FunctionTypeAnnotation          extends FlowType
  object FunctionTypeParam               extends BabelNode
  object GenericTypeAnnotation           extends FlowType
  object Identifier                      extends BabelNode
  object IfStatement                     extends BabelNode
  object Import                          extends BabelNode
  object ImportAttribute                 extends BabelNode
  object ImportDeclaration               extends BabelNode
  object ImportDefaultSpecifier          extends BabelNode
  object ImportNamespaceSpecifier        extends BabelNode
  object ImportSpecifier                 extends BabelNode
  object IndexedAccessType               extends FlowType
  object InferredPredicate               extends BabelNode
  object InterfaceDeclaration            extends BabelNode
  object InterfaceExtends                extends BabelNode
  object InterfaceTypeAnnotation         extends FlowType
  object InterpreterDirective            extends BabelNode
  object IntersectionTypeAnnotation      extends FlowType
  object JSXAttribute                    extends BabelNode
  object JSXClosingElement               extends BabelNode
  object JSXClosingFragment              extends BabelNode
  object JSXElement                      extends BabelNode
  object JSXEmptyExpression              extends BabelNode
  object JSXExpressionContainer          extends BabelNode
  object JSXFragment                     extends BabelNode
  object JSXIdentifier                   extends BabelNode
  object JSXMemberExpression             extends Expression
  object JSXNamespacedName               extends BabelNode
  object JSXOpeningElement               extends BabelNode
  object JSXOpeningFragment              extends BabelNode
  object JSXSpreadAttribute              extends BabelNode
  object JSXSpreadChild                  extends BabelNode
  object JSXText                         extends BabelNode
  object LabeledStatement                extends BabelNode
  object LogicalExpression               extends BabelNode
  object MemberExpression                extends Expression
  object MetaProperty                    extends BabelNode
  object MixedTypeAnnotation             extends FlowType
  object ModuleExpression                extends BabelNode
  object NewExpression                   extends Expression
  object Noop                            extends BabelNode
  object NullLiteral                     extends BabelNode
  object NullLiteralTypeAnnotation       extends FlowType
  object NullableTypeAnnotation          extends FlowType
  object NumberLiteral                   extends BabelNode
  object NumberLiteralTypeAnnotation     extends FlowType
  object NumberTypeAnnotation            extends FlowType
  object NumericLiteral                  extends BabelNode
  object ObjectExpression                extends BabelNode
  object ObjectMethod                    extends BabelNode
  object ObjectPattern                   extends BabelNode
  object ObjectProperty                  extends BabelNode
  object ObjectTypeAnnotation            extends FlowType
  object ObjectTypeCallProperty          extends BabelNode
  object ObjectTypeIndexer               extends BabelNode
  object ObjectTypeInternalSlot          extends BabelNode
  object ObjectTypeProperty              extends BabelNode
  object ObjectTypeSpreadProperty        extends BabelNode
  object OpaqueType                      extends BabelNode
  object OptionalCallExpression          extends Expression
  object OptionalIndexedAccessType       extends FlowType
  object OptionalMemberExpression        extends Expression
  object ParenthesizedExpression         extends BabelNode
  object PipelineBareFunction            extends BabelNode
  object PipelinePrimaryTopicReference   extends BabelNode
  object PipelineTopicExpression         extends Expression
  object Placeholder                     extends BabelNode
  object PrivateName                     extends BabelNode
  object Program                         extends BabelNode
  object QualifiedTypeIdentifier         extends BabelNode
  object RecordExpression                extends Expression
  object RegExpLiteral                   extends BabelNode
  object RegexLiteral                    extends BabelNode
  object RestElement                     extends BabelNode
  object RestProperty                    extends BabelNode
  object ReturnStatement                 extends BabelNode
  object SequenceExpression              extends BabelNode
  object SpreadElement                   extends BabelNode
  object SpreadProperty                  extends BabelNode
  object StaticBlock                     extends BabelNode
  object StringLiteral                   extends BabelNode
  object StringLiteralTypeAnnotation     extends FlowType
  object StringTypeAnnotation            extends FlowType
  object Super                           extends BabelNode
  object SwitchCase                      extends BabelNode
  object SwitchStatement                 extends BabelNode
  object SymbolTypeAnnotation            extends FlowType
  object TSAnyKeyword                    extends TSType
  object TSArrayType                     extends TSType
  object TSAsExpression                  extends Expression
  object TSBigIntKeyword                 extends TSType
  object TSBooleanKeyword                extends TSType
  object TSCallSignatureDeclaration      extends BabelNode
  object TSConditionalType               extends TSType
  object TSConstructSignatureDeclaration extends BabelNode
  object TSConstructorType               extends TSType
  object TSDeclareFunction               extends BabelNode
  object TSDeclareMethod                 extends BabelNode
  object TSEnumDeclaration               extends BabelNode
  object TSEnumMember                    extends BabelNode
  object TSExportAssignment              extends BabelNode
  object TSExpressionWithTypeArguments   extends TSType
  object TSExternalModuleReference       extends BabelNode
  object TSFunctionType                  extends TSType
  object TSImportEqualsDeclaration       extends BabelNode
  object TSImportType                    extends TSType
  object TSIndexSignature                extends BabelNode
  object TSIndexedAccessType             extends TSType
  object TSInferType                     extends TSType
  object TSInstantiationExpression       extends Expression
  object TSInterfaceBody                 extends BabelNode
  object TSInterfaceDeclaration          extends BabelNode
  object TSIntersectionType              extends TSType
  object TSIntrinsicKeyword              extends TSType
  object TSLiteralType                   extends TSType
  object TSMappedType                    extends TSType
  object TSMethodSignature               extends BabelNode
  object TSModuleBlock                   extends BabelNode
  object TSModuleDeclaration             extends BabelNode
  object TSNamedTupleMember              extends BabelNode
  object TSNamespaceExportDeclaration    extends BabelNode
  object TSNeverKeyword                  extends TSType
  object TSNonNullExpression             extends Expression
  object TSNullKeyword                   extends TSType
  object TSNumberKeyword                 extends TSType
  object TSObjectKeyword                 extends TSType
  object TSOptionalType                  extends TSType
  object TSParameterProperty             extends Expression
  object TSParenthesizedType             extends TSType
  object TSPropertySignature             extends BabelNode
  object TSQualifiedName                 extends BabelNode
  object TSRestType                      extends TSType
  object TSSatisfiesExpression           extends Expression
  object TSStringKeyword                 extends TSType
  object TSSymbolKeyword                 extends TSType
  object TSThisType                      extends TSType
  object TSTupleType                     extends TSType
  object TSTypeAliasDeclaration          extends BabelNode
  object TSTypeAnnotation                extends FlowType
  object TSTypeAssertion                 extends BabelNode
  object TSTypeExpression                extends TSType
  object TSTypeLiteral                   extends TSType
  object TSTypeOperator                  extends TSType
  object TSTypeParameter                 extends TSType
  object TSTypeParameterDeclaration      extends BabelNode
  object TSTypeParameterInstantiation    extends BabelNode
  object TSTypePredicate                 extends TSType
  object TSTypeQuery                     extends TSType
  object TSTypeReference                 extends TSType
  object TSUndefinedKeyword              extends TSType
  object TSUnionType                     extends TSType
  object TSUnknownKeyword                extends TSType
  object TSVoidKeyword                   extends TSType
  object TaggedTemplateExpression        extends BabelNode
  object TemplateElement                 extends BabelNode
  object TemplateLiteral                 extends BabelNode
  object ThisExpression                  extends Expression
  object ThisTypeAnnotation              extends FlowType
  object ThrowStatement                  extends BabelNode
  object TopicReference                  extends BabelNode
  object TryStatement                    extends BabelNode
  object TupleExpression                 extends BabelNode
  object TupleTypeAnnotation             extends FlowType
  object TypeAlias                       extends BabelNode
  object TypeAnnotation                  extends FlowType
  object TypeCastExpression              extends BabelNode
  object TSTypeCastExpression            extends BabelNode
  object TypeParameter                   extends BabelNode
  object TypeParameterDeclaration        extends BabelNode
  object TypeParameterInstantiation      extends BabelNode
  object TypeofTypeAnnotation            extends FlowType
  object UnaryExpression                 extends BabelNode
  object UnionTypeAnnotation             extends FlowType
  object UpdateExpression                extends Expression
  object V8IntrinsicIdentifier           extends BabelNode
  object VariableDeclaration             extends BabelNode
  object VariableDeclarator              extends BabelNode
  object Variance                        extends BabelNode
  object VoidTypeAnnotation              extends FlowType
  object WhileStatement                  extends BabelNode
  object WithStatement                   extends BabelNode
  object YieldExpression                 extends BabelNode

}
