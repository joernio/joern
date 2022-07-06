package io.joern.jssrc2cpg.parser

object BabelAst {
  // extracted from:
  // https://github.com/babel/babel/blob/main/packages/babel-types/src/ast-types/generated/index.ts

  def fromString(nodeName: String): BabelNode = nodeName match {
    case "AnyTypeAnnotation"               => AnyTypeAnnotation
    case "ArgumentPlaceholder"             => ArgumentPlaceholder
    case "ArrayExpression"                 => ArrayExpression
    case "ArrayPattern"                    => ArrayPattern
    case "ArrayTypeAnnotation"             => ArrayTypeAnnotation
    case "ArrowFunctionExpression"         => ArrowFunctionExpression
    case "AssignmentExpression"            => AssignmentExpression
    case "AssignmentPattern"               => AssignmentPattern
    case "AwaitExpression"                 => AwaitExpression
    case "BigIntLiteral"                   => BigIntLiteral
    case "BinaryExpression"                => BinaryExpression
    case "BindExpression"                  => BindExpression
    case "BlockStatement"                  => BlockStatement
    case "BooleanLiteral"                  => BooleanLiteral
    case "BooleanLiteralTypeAnnotation"    => BooleanLiteralTypeAnnotation
    case "BooleanTypeAnnotation"           => BooleanTypeAnnotation
    case "BreakStatement"                  => BreakStatement
    case "CallExpression"                  => CallExpression
    case "CatchClause"                     => CatchClause
    case "ClassAccessorProperty"           => ClassAccessorProperty
    case "ClassBody"                       => ClassBody
    case "ClassDeclaration"                => ClassDeclaration
    case "ClassExpression"                 => ClassExpression
    case "ClassImplements"                 => ClassImplements
    case "ClassMethod"                     => ClassMethod
    case "ClassPrivateMethod"              => ClassPrivateMethod
    case "ClassPrivateProperty"            => ClassPrivateProperty
    case "ClassProperty"                   => ClassProperty
    case "ConditionalExpression"           => ConditionalExpression
    case "ContinueStatement"               => ContinueStatement
    case "DebuggerStatement"               => DebuggerStatement
    case "DecimalLiteral"                  => DecimalLiteral
    case "DeclareClass"                    => DeclareClass
    case "DeclareExportAllDeclaration"     => DeclareExportAllDeclaration
    case "DeclareExportDeclaration"        => DeclareExportDeclaration
    case "DeclareFunction"                 => DeclareFunction
    case "DeclareInterface"                => DeclareInterface
    case "DeclareModule"                   => DeclareModule
    case "DeclareModuleExports"            => DeclareModuleExports
    case "DeclareOpaqueType"               => DeclareOpaqueType
    case "DeclareTypeAlias"                => DeclareTypeAlias
    case "DeclareVariable"                 => DeclareVariable
    case "DeclaredPredicate"               => DeclaredPredicate
    case "Decorator"                       => Decorator
    case "Directive"                       => Directive
    case "DirectiveLiteral"                => DirectiveLiteral
    case "DoExpression"                    => DoExpression
    case "DoWhileStatement"                => DoWhileStatement
    case "EmptyStatement"                  => EmptyStatement
    case "EmptyTypeAnnotation"             => EmptyTypeAnnotation
    case "EnumBooleanBody"                 => EnumBooleanBody
    case "EnumBooleanMember"               => EnumBooleanMember
    case "EnumDeclaration"                 => EnumDeclaration
    case "EnumDefaultedMember"             => EnumDefaultedMember
    case "EnumNumberBody"                  => EnumNumberBody
    case "EnumNumberMember"                => EnumNumberMember
    case "EnumStringBody"                  => EnumStringBody
    case "EnumStringMember"                => EnumStringMember
    case "EnumSymbolBody"                  => EnumSymbolBody
    case "ExistsTypeAnnotation"            => ExistsTypeAnnotation
    case "ExportAllDeclaration"            => ExportAllDeclaration
    case "ExportDefaultDeclaration"        => ExportDefaultDeclaration
    case "ExportDefaultSpecifier"          => ExportDefaultSpecifier
    case "ExportNamedDeclaration"          => ExportNamedDeclaration
    case "ExportNamespaceSpecifier"        => ExportNamespaceSpecifier
    case "ExportSpecifier"                 => ExportSpecifier
    case "ExpressionStatement"             => ExpressionStatement
    case "File"                            => File
    case "ForInStatement"                  => ForInStatement
    case "ForOfStatement"                  => ForOfStatement
    case "ForStatement"                    => ForStatement
    case "FunctionDeclaration"             => FunctionDeclaration
    case "FunctionExpression"              => FunctionExpression
    case "FunctionTypeAnnotation"          => FunctionTypeAnnotation
    case "FunctionTypeParam"               => FunctionTypeParam
    case "GenericTypeAnnotation"           => GenericTypeAnnotation
    case "Identifier"                      => Identifier
    case "IfStatement"                     => IfStatement
    case "Import"                          => Import
    case "ImportAttribute"                 => ImportAttribute
    case "ImportDeclaration"               => ImportDeclaration
    case "ImportDefaultSpecifier"          => ImportDefaultSpecifier
    case "ImportNamespaceSpecifier"        => ImportNamespaceSpecifier
    case "ImportSpecifier"                 => ImportSpecifier
    case "IndexedAccessType"               => IndexedAccessType
    case "InferredPredicate"               => InferredPredicate
    case "InterfaceDeclaration"            => InterfaceDeclaration
    case "InterfaceExtends"                => InterfaceExtends
    case "InterfaceTypeAnnotation"         => InterfaceTypeAnnotation
    case "InterpreterDirective"            => InterpreterDirective
    case "IntersectionTypeAnnotation"      => IntersectionTypeAnnotation
    case "JSXAttribute"                    => JSXAttribute
    case "JSXClosingElement"               => JSXClosingElement
    case "JSXClosingFragment"              => JSXClosingFragment
    case "JSXElement"                      => JSXElement
    case "JSXEmptyExpression"              => JSXEmptyExpression
    case "JSXExpressionContainer"          => JSXExpressionContainer
    case "JSXFragment"                     => JSXFragment
    case "JSXIdentifier"                   => JSXIdentifier
    case "JSXMemberExpression"             => JSXMemberExpression
    case "JSXNamespacedName"               => JSXNamespacedName
    case "JSXOpeningElement"               => JSXOpeningElement
    case "JSXOpeningFragment"              => JSXOpeningFragment
    case "JSXSpreadAttribute"              => JSXSpreadAttribute
    case "JSXSpreadChild"                  => JSXSpreadChild
    case "JSXText"                         => JSXText
    case "LabeledStatement"                => LabeledStatement
    case "LogicalExpression"               => LogicalExpression
    case "MemberExpression"                => MemberExpression
    case "MetaProperty"                    => MetaProperty
    case "MixedTypeAnnotation"             => MixedTypeAnnotation
    case "ModuleExpression"                => ModuleExpression
    case "NewExpression"                   => NewExpression
    case "Noop"                            => Noop
    case "NullLiteral"                     => NullLiteral
    case "NullLiteralTypeAnnotation"       => NullLiteralTypeAnnotation
    case "NullableTypeAnnotation"          => NullableTypeAnnotation
    case "NumberLiteral"                   => NumberLiteral
    case "NumberLiteralTypeAnnotation"     => NumberLiteralTypeAnnotation
    case "NumberTypeAnnotation"            => NumberTypeAnnotation
    case "NumericLiteral"                  => NumericLiteral
    case "ObjectExpression"                => ObjectExpression
    case "ObjectMethod"                    => ObjectMethod
    case "ObjectPattern"                   => ObjectPattern
    case "ObjectProperty"                  => ObjectProperty
    case "ObjectTypeAnnotation"            => ObjectTypeAnnotation
    case "ObjectTypeCallProperty"          => ObjectTypeCallProperty
    case "ObjectTypeIndexer"               => ObjectTypeIndexer
    case "ObjectTypeInternalSlot"          => ObjectTypeInternalSlot
    case "ObjectTypeProperty"              => ObjectTypeProperty
    case "ObjectTypeSpreadProperty"        => ObjectTypeSpreadProperty
    case "OpaqueType"                      => OpaqueType
    case "OptionalCallExpression"          => OptionalCallExpression
    case "OptionalIndexedAccessType"       => OptionalIndexedAccessType
    case "OptionalMemberExpression"        => OptionalMemberExpression
    case "ParenthesizedExpression"         => ParenthesizedExpression
    case "PipelineBareFunction"            => PipelineBareFunction
    case "PipelinePrimaryTopicReference"   => PipelinePrimaryTopicReference
    case "PipelineTopicExpression"         => PipelineTopicExpression
    case "Placeholder"                     => Placeholder
    case "PrivateName"                     => PrivateName
    case "Program"                         => Program
    case "QualifiedTypeIdentifier"         => QualifiedTypeIdentifier
    case "RecordExpression"                => RecordExpression
    case "RegExpLiteral"                   => RegExpLiteral
    case "RegexLiteral"                    => RegexLiteral
    case "RestElement"                     => RestElement
    case "RestProperty"                    => RestProperty
    case "ReturnStatement"                 => ReturnStatement
    case "SequenceExpression"              => SequenceExpression
    case "SpreadElement"                   => SpreadElement
    case "SpreadProperty"                  => SpreadProperty
    case "StaticBlock"                     => StaticBlock
    case "StringLiteral"                   => StringLiteral
    case "StringLiteralTypeAnnotation"     => StringLiteralTypeAnnotation
    case "StringTypeAnnotation"            => StringTypeAnnotation
    case "Super"                           => Super
    case "SwitchCase"                      => SwitchCase
    case "SwitchStatement"                 => SwitchStatement
    case "SymbolTypeAnnotation"            => SymbolTypeAnnotation
    case "TSAnyKeyword"                    => TSAnyKeyword
    case "TSArrayType"                     => TSArrayType
    case "TSAsExpression"                  => TSAsExpression
    case "TSBigIntKeyword"                 => TSBigIntKeyword
    case "TSBooleanKeyword"                => TSBooleanKeyword
    case "TSCallSignatureDeclaration"      => TSCallSignatureDeclaration
    case "TSConditionalType"               => TSConditionalType
    case "TSConstructSignatureDeclaration" => TSConstructSignatureDeclaration
    case "TSConstructorType"               => TSConstructorType
    case "TSDeclareFunction"               => TSDeclareFunction
    case "TSDeclareMethod"                 => TSDeclareMethod
    case "TSEnumDeclaration"               => TSEnumDeclaration
    case "TSEnumMember"                    => TSEnumMember
    case "TSExportAssignment"              => TSExportAssignment
    case "TSExpressionWithTypeArguments"   => TSExpressionWithTypeArguments
    case "TSExternalModuleReference"       => TSExternalModuleReference
    case "TSFunctionType"                  => TSFunctionType
    case "TSImportEqualsDeclaration"       => TSImportEqualsDeclaration
    case "TSImportType"                    => TSImportType
    case "TSIndexSignature"                => TSIndexSignature
    case "TSIndexedAccessType"             => TSIndexedAccessType
    case "TSInferType"                     => TSInferType
    case "TSInterfaceBody"                 => TSInterfaceBody
    case "TSInterfaceDeclaration"          => TSInterfaceDeclaration
    case "TSIntersectionType"              => TSIntersectionType
    case "TSIntrinsicKeyword"              => TSIntrinsicKeyword
    case "TSLiteralType"                   => TSLiteralType
    case "TSMappedType"                    => TSMappedType
    case "TSMethodSignature"               => TSMethodSignature
    case "TSModuleBlock"                   => TSModuleBlock
    case "TSModuleDeclaration"             => TSModuleDeclaration
    case "TSNamedTupleMember"              => TSNamedTupleMember
    case "TSNamespaceExportDeclaration"    => TSNamespaceExportDeclaration
    case "TSNeverKeyword"                  => TSNeverKeyword
    case "TSNonNullExpression"             => TSNonNullExpression
    case "TSNullKeyword"                   => TSNullKeyword
    case "TSNumberKeyword"                 => TSNumberKeyword
    case "TSObjectKeyword"                 => TSObjectKeyword
    case "TSOptionalType"                  => TSOptionalType
    case "TSParameterProperty"             => TSParameterProperty
    case "TSParenthesizedType"             => TSParenthesizedType
    case "TSPropertySignature"             => TSPropertySignature
    case "TSQualifiedName"                 => TSQualifiedName
    case "TSRestType"                      => TSRestType
    case "TSStringKeyword"                 => TSStringKeyword
    case "TSSymbolKeyword"                 => TSSymbolKeyword
    case "TSThisType"                      => TSThisType
    case "TSTupleType"                     => TSTupleType
    case "TSTypeAliasDeclaration"          => TSTypeAliasDeclaration
    case "TSTypeAnnotation"                => TSTypeAnnotation
    case "TSTypeAssertion"                 => TSTypeAssertion
    case "TSTypeLiteral"                   => TSTypeLiteral
    case "TSTypeOperator"                  => TSTypeOperator
    case "TSTypeParameter"                 => TSTypeParameter
    case "TSTypeParameterDeclaration"      => TSTypeParameterDeclaration
    case "TSTypeParameterInstantiation"    => TSTypeParameterInstantiation
    case "TSTypePredicate"                 => TSTypePredicate
    case "TSTypeQuery"                     => TSTypeQuery
    case "TSTypeReference"                 => TSTypeReference
    case "TSUndefinedKeyword"              => TSUndefinedKeyword
    case "TSUnionType"                     => TSUnionType
    case "TSUnknownKeyword"                => TSUnknownKeyword
    case "TSVoidKeyword"                   => TSVoidKeyword
    case "TaggedTemplateExpression"        => TaggedTemplateExpression
    case "TemplateElement"                 => TemplateElement
    case "TemplateLiteral"                 => TemplateLiteral
    case "ThisExpression"                  => ThisExpression
    case "ThisTypeAnnotation"              => ThisTypeAnnotation
    case "ThrowStatement"                  => ThrowStatement
    case "TopicReference"                  => TopicReference
    case "TryStatement"                    => TryStatement
    case "TupleExpression"                 => TupleExpression
    case "TupleTypeAnnotation"             => TupleTypeAnnotation
    case "TypeAlias"                       => TypeAlias
    case "TypeAnnotation"                  => TypeAnnotation
    case "TypeCastExpression"              => TypeCastExpression
    case "TypeParameter"                   => TypeParameter
    case "TypeParameterDeclaration"        => TypeParameterDeclaration
    case "TypeParameterInstantiation"      => TypeParameterInstantiation
    case "TypeofTypeAnnotation"            => TypeofTypeAnnotation
    case "UnaryExpression"                 => UnaryExpression
    case "UnionTypeAnnotation"             => UnionTypeAnnotation
    case "UpdateExpression"                => UpdateExpression
    case "V8IntrinsicIdentifier"           => V8IntrinsicIdentifier
    case "VariableDeclaration"             => VariableDeclaration
    case "VariableDeclarator"              => VariableDeclarator
    case "Variance"                        => Variance
    case "VoidTypeAnnotation"              => VoidTypeAnnotation
    case "WhileStatement"                  => WhileStatement
    case "WithStatement"                   => WithStatement
    case "YieldExpression"                 => YieldExpression
  }

  sealed trait BabelNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  sealed trait FlowType extends BabelNode

  sealed trait TSType extends BabelNode

  case object AnyTypeAnnotation               extends FlowType
  case object ArgumentPlaceholder             extends BabelNode
  case object ArrayExpression                 extends BabelNode
  case object ArrayPattern                    extends BabelNode
  case object ArrayTypeAnnotation             extends FlowType
  case object ArrowFunctionExpression         extends BabelNode
  case object AssignmentExpression            extends BabelNode
  case object AssignmentPattern               extends BabelNode
  case object AwaitExpression                 extends BabelNode
  case object BigIntLiteral                   extends BabelNode
  case object BinaryExpression                extends BabelNode
  case object BindExpression                  extends BabelNode
  case object BlockStatement                  extends BabelNode
  case object BooleanLiteral                  extends BabelNode
  case object BooleanLiteralTypeAnnotation    extends FlowType
  case object BooleanTypeAnnotation           extends FlowType
  case object BreakStatement                  extends BabelNode
  case object CallExpression                  extends BabelNode
  case object CatchClause                     extends BabelNode
  case object ClassAccessorProperty           extends BabelNode
  case object ClassBody                       extends BabelNode
  case object ClassDeclaration                extends BabelNode
  case object ClassExpression                 extends BabelNode
  case object ClassImplements                 extends BabelNode
  case object ClassMethod                     extends BabelNode
  case object ClassPrivateMethod              extends BabelNode
  case object ClassPrivateProperty            extends BabelNode
  case object ClassProperty                   extends BabelNode
  case object ConditionalExpression           extends BabelNode
  case object ContinueStatement               extends BabelNode
  case object DebuggerStatement               extends BabelNode
  case object DecimalLiteral                  extends BabelNode
  case object DeclareClass                    extends BabelNode
  case object DeclareExportAllDeclaration     extends BabelNode
  case object DeclareExportDeclaration        extends BabelNode
  case object DeclareFunction                 extends BabelNode
  case object DeclareInterface                extends BabelNode
  case object DeclareModule                   extends BabelNode
  case object DeclareModuleExports            extends BabelNode
  case object DeclareOpaqueType               extends BabelNode
  case object DeclareTypeAlias                extends BabelNode
  case object DeclareVariable                 extends BabelNode
  case object DeclaredPredicate               extends BabelNode
  case object Decorator                       extends BabelNode
  case object Directive                       extends BabelNode
  case object DirectiveLiteral                extends BabelNode
  case object DoExpression                    extends BabelNode
  case object DoWhileStatement                extends BabelNode
  case object EmptyStatement                  extends BabelNode
  case object EmptyTypeAnnotation             extends FlowType
  case object EnumBooleanBody                 extends BabelNode
  case object EnumBooleanMember               extends BabelNode
  case object EnumDeclaration                 extends BabelNode
  case object EnumDefaultedMember             extends BabelNode
  case object EnumNumberBody                  extends BabelNode
  case object EnumNumberMember                extends BabelNode
  case object EnumStringBody                  extends BabelNode
  case object EnumStringMember                extends BabelNode
  case object EnumSymbolBody                  extends BabelNode
  case object ExistsTypeAnnotation            extends FlowType
  case object ExportAllDeclaration            extends BabelNode
  case object ExportDefaultDeclaration        extends BabelNode
  case object ExportDefaultSpecifier          extends BabelNode
  case object ExportNamedDeclaration          extends BabelNode
  case object ExportNamespaceSpecifier        extends BabelNode
  case object ExportSpecifier                 extends BabelNode
  case object ExpressionStatement             extends BabelNode
  case object File                            extends BabelNode
  case object ForInStatement                  extends BabelNode
  case object ForOfStatement                  extends BabelNode
  case object ForStatement                    extends BabelNode
  case object FunctionDeclaration             extends BabelNode
  case object FunctionExpression              extends BabelNode
  case object FunctionTypeAnnotation          extends FlowType
  case object FunctionTypeParam               extends BabelNode
  case object GenericTypeAnnotation           extends FlowType
  case object Identifier                      extends BabelNode
  case object IfStatement                     extends BabelNode
  case object Import                          extends BabelNode
  case object ImportAttribute                 extends BabelNode
  case object ImportDeclaration               extends BabelNode
  case object ImportDefaultSpecifier          extends BabelNode
  case object ImportNamespaceSpecifier        extends BabelNode
  case object ImportSpecifier                 extends BabelNode
  case object IndexedAccessType               extends FlowType
  case object InferredPredicate               extends BabelNode
  case object InterfaceDeclaration            extends BabelNode
  case object InterfaceExtends                extends BabelNode
  case object InterfaceTypeAnnotation         extends FlowType
  case object InterpreterDirective            extends BabelNode
  case object IntersectionTypeAnnotation      extends FlowType
  case object JSXAttribute                    extends BabelNode
  case object JSXClosingElement               extends BabelNode
  case object JSXClosingFragment              extends BabelNode
  case object JSXElement                      extends BabelNode
  case object JSXEmptyExpression              extends BabelNode
  case object JSXExpressionContainer          extends BabelNode
  case object JSXFragment                     extends BabelNode
  case object JSXIdentifier                   extends BabelNode
  case object JSXMemberExpression             extends BabelNode
  case object JSXNamespacedName               extends BabelNode
  case object JSXOpeningElement               extends BabelNode
  case object JSXOpeningFragment              extends BabelNode
  case object JSXSpreadAttribute              extends BabelNode
  case object JSXSpreadChild                  extends BabelNode
  case object JSXText                         extends BabelNode
  case object LabeledStatement                extends BabelNode
  case object LogicalExpression               extends BabelNode
  case object MemberExpression                extends BabelNode
  case object MetaProperty                    extends BabelNode
  case object MixedTypeAnnotation             extends FlowType
  case object ModuleExpression                extends BabelNode
  case object NewExpression                   extends BabelNode
  case object Noop                            extends BabelNode
  case object NullLiteral                     extends BabelNode
  case object NullLiteralTypeAnnotation       extends FlowType
  case object NullableTypeAnnotation          extends FlowType
  case object NumberLiteral                   extends BabelNode
  case object NumberLiteralTypeAnnotation     extends FlowType
  case object NumberTypeAnnotation            extends FlowType
  case object NumericLiteral                  extends BabelNode
  case object ObjectExpression                extends BabelNode
  case object ObjectMethod                    extends BabelNode
  case object ObjectPattern                   extends BabelNode
  case object ObjectProperty                  extends BabelNode
  case object ObjectTypeAnnotation            extends FlowType
  case object ObjectTypeCallProperty          extends BabelNode
  case object ObjectTypeIndexer               extends BabelNode
  case object ObjectTypeInternalSlot          extends BabelNode
  case object ObjectTypeProperty              extends BabelNode
  case object ObjectTypeSpreadProperty        extends BabelNode
  case object OpaqueType                      extends BabelNode
  case object OptionalCallExpression          extends BabelNode
  case object OptionalIndexedAccessType       extends FlowType
  case object OptionalMemberExpression        extends BabelNode
  case object ParenthesizedExpression         extends BabelNode
  case object PipelineBareFunction            extends BabelNode
  case object PipelinePrimaryTopicReference   extends BabelNode
  case object PipelineTopicExpression         extends BabelNode
  case object Placeholder                     extends BabelNode
  case object PrivateName                     extends BabelNode
  case object Program                         extends BabelNode
  case object QualifiedTypeIdentifier         extends BabelNode
  case object RecordExpression                extends BabelNode
  case object RegExpLiteral                   extends BabelNode
  case object RegexLiteral                    extends BabelNode
  case object RestElement                     extends BabelNode
  case object RestProperty                    extends BabelNode
  case object ReturnStatement                 extends BabelNode
  case object SequenceExpression              extends BabelNode
  case object SpreadElement                   extends BabelNode
  case object SpreadProperty                  extends BabelNode
  case object StaticBlock                     extends BabelNode
  case object StringLiteral                   extends BabelNode
  case object StringLiteralTypeAnnotation     extends FlowType
  case object StringTypeAnnotation            extends FlowType
  case object Super                           extends BabelNode
  case object SwitchCase                      extends BabelNode
  case object SwitchStatement                 extends BabelNode
  case object SymbolTypeAnnotation            extends FlowType
  case object TSAnyKeyword                    extends TSType
  case object TSArrayType                     extends TSType
  case object TSAsExpression                  extends BabelNode
  case object TSBigIntKeyword                 extends TSType
  case object TSBooleanKeyword                extends TSType
  case object TSCallSignatureDeclaration      extends BabelNode
  case object TSConditionalType               extends TSType
  case object TSConstructSignatureDeclaration extends BabelNode
  case object TSConstructorType               extends TSType
  case object TSDeclareFunction               extends BabelNode
  case object TSDeclareMethod                 extends BabelNode
  case object TSEnumDeclaration               extends BabelNode
  case object TSEnumMember                    extends BabelNode
  case object TSExportAssignment              extends BabelNode
  case object TSExpressionWithTypeArguments   extends TSType
  case object TSExternalModuleReference       extends BabelNode
  case object TSFunctionType                  extends TSType
  case object TSImportEqualsDeclaration       extends BabelNode
  case object TSImportType                    extends TSType
  case object TSIndexSignature                extends BabelNode
  case object TSIndexedAccessType             extends TSType
  case object TSInferType                     extends TSType
  case object TSInterfaceBody                 extends BabelNode
  case object TSInterfaceDeclaration          extends BabelNode
  case object TSIntersectionType              extends TSType
  case object TSIntrinsicKeyword              extends TSType
  case object TSLiteralType                   extends TSType
  case object TSMappedType                    extends TSType
  case object TSMethodSignature               extends BabelNode
  case object TSModuleBlock                   extends BabelNode
  case object TSModuleDeclaration             extends BabelNode
  case object TSNamedTupleMember              extends BabelNode
  case object TSNamespaceExportDeclaration    extends BabelNode
  case object TSNeverKeyword                  extends TSType
  case object TSNonNullExpression             extends BabelNode
  case object TSNullKeyword                   extends TSType
  case object TSNumberKeyword                 extends TSType
  case object TSObjectKeyword                 extends TSType
  case object TSOptionalType                  extends TSType
  case object TSParameterProperty             extends BabelNode
  case object TSParenthesizedType             extends TSType
  case object TSPropertySignature             extends BabelNode
  case object TSQualifiedName                 extends BabelNode
  case object TSRestType                      extends TSType
  case object TSStringKeyword                 extends TSType
  case object TSSymbolKeyword                 extends TSType
  case object TSThisType                      extends TSType
  case object TSTupleType                     extends TSType
  case object TSTypeAliasDeclaration          extends BabelNode
  case object TSTypeAnnotation                extends FlowType
  case object TSTypeAssertion                 extends BabelNode
  case object TSTypeLiteral                   extends TSType
  case object TSTypeOperator                  extends TSType
  case object TSTypeParameter                 extends TSType
  case object TSTypeParameterDeclaration      extends BabelNode
  case object TSTypeParameterInstantiation    extends BabelNode
  case object TSTypePredicate                 extends TSType
  case object TSTypeQuery                     extends TSType
  case object TSTypeReference                 extends TSType
  case object TSUndefinedKeyword              extends TSType
  case object TSUnionType                     extends TSType
  case object TSUnknownKeyword                extends TSType
  case object TSVoidKeyword                   extends TSType
  case object TaggedTemplateExpression        extends BabelNode
  case object TemplateElement                 extends BabelNode
  case object TemplateLiteral                 extends BabelNode
  case object ThisExpression                  extends BabelNode
  case object ThisTypeAnnotation              extends FlowType
  case object ThrowStatement                  extends BabelNode
  case object TopicReference                  extends BabelNode
  case object TryStatement                    extends BabelNode
  case object TupleExpression                 extends BabelNode
  case object TupleTypeAnnotation             extends FlowType
  case object TypeAlias                       extends BabelNode
  case object TypeAnnotation                  extends FlowType
  case object TypeCastExpression              extends BabelNode
  case object TypeParameter                   extends BabelNode
  case object TypeParameterDeclaration        extends BabelNode
  case object TypeParameterInstantiation      extends BabelNode
  case object TypeofTypeAnnotation            extends FlowType
  case object UnaryExpression                 extends BabelNode
  case object UnionTypeAnnotation             extends FlowType
  case object UpdateExpression                extends BabelNode
  case object V8IntrinsicIdentifier           extends BabelNode
  case object VariableDeclaration             extends BabelNode
  case object VariableDeclarator              extends BabelNode
  case object Variance                        extends BabelNode
  case object VoidTypeAnnotation              extends FlowType
  case object WhileStatement                  extends BabelNode
  case object WithStatement                   extends BabelNode
  case object YieldExpression                 extends BabelNode

}
