package io.joern.jssrc2cpg.parser

object BabelAst {
  // extracted from:
  // https://github.com/babel/babel/blob/main/packages/babel-types/src/ast-types/generated/index.ts

  def fromString(nodeName: String): Node = nodeName match {
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

  abstract sealed class Node {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  case object AnyTypeAnnotation               extends Node
  case object ArgumentPlaceholder             extends Node
  case object ArrayExpression                 extends Node
  case object ArrayPattern                    extends Node
  case object ArrayTypeAnnotation             extends Node
  case object ArrowFunctionExpression         extends Node
  case object AssignmentExpression            extends Node
  case object AssignmentPattern               extends Node
  case object AwaitExpression                 extends Node
  case object BigIntLiteral                   extends Node
  case object BinaryExpression                extends Node
  case object BindExpression                  extends Node
  case object BlockStatement                  extends Node
  case object BooleanLiteral                  extends Node
  case object BooleanLiteralTypeAnnotation    extends Node
  case object BooleanTypeAnnotation           extends Node
  case object BreakStatement                  extends Node
  case object CallExpression                  extends Node
  case object CatchClause                     extends Node
  case object ClassAccessorProperty           extends Node
  case object ClassBody                       extends Node
  case object ClassDeclaration                extends Node
  case object ClassExpression                 extends Node
  case object ClassImplements                 extends Node
  case object ClassMethod                     extends Node
  case object ClassPrivateMethod              extends Node
  case object ClassPrivateProperty            extends Node
  case object ClassProperty                   extends Node
  case object ConditionalExpression           extends Node
  case object ContinueStatement               extends Node
  case object DebuggerStatement               extends Node
  case object DecimalLiteral                  extends Node
  case object DeclareClass                    extends Node
  case object DeclareExportAllDeclaration     extends Node
  case object DeclareExportDeclaration        extends Node
  case object DeclareFunction                 extends Node
  case object DeclareInterface                extends Node
  case object DeclareModule                   extends Node
  case object DeclareModuleExports            extends Node
  case object DeclareOpaqueType               extends Node
  case object DeclareTypeAlias                extends Node
  case object DeclareVariable                 extends Node
  case object DeclaredPredicate               extends Node
  case object Decorator                       extends Node
  case object Directive                       extends Node
  case object DirectiveLiteral                extends Node
  case object DoExpression                    extends Node
  case object DoWhileStatement                extends Node
  case object EmptyStatement                  extends Node
  case object EmptyTypeAnnotation             extends Node
  case object EnumBooleanBody                 extends Node
  case object EnumBooleanMember               extends Node
  case object EnumDeclaration                 extends Node
  case object EnumDefaultedMember             extends Node
  case object EnumNumberBody                  extends Node
  case object EnumNumberMember                extends Node
  case object EnumStringBody                  extends Node
  case object EnumStringMember                extends Node
  case object EnumSymbolBody                  extends Node
  case object ExistsTypeAnnotation            extends Node
  case object ExportAllDeclaration            extends Node
  case object ExportDefaultDeclaration        extends Node
  case object ExportDefaultSpecifier          extends Node
  case object ExportNamedDeclaration          extends Node
  case object ExportNamespaceSpecifier        extends Node
  case object ExportSpecifier                 extends Node
  case object ExpressionStatement             extends Node
  case object File                            extends Node
  case object ForInStatement                  extends Node
  case object ForOfStatement                  extends Node
  case object ForStatement                    extends Node
  case object FunctionDeclaration             extends Node
  case object FunctionExpression              extends Node
  case object FunctionTypeAnnotation          extends Node
  case object FunctionTypeParam               extends Node
  case object GenericTypeAnnotation           extends Node
  case object Identifier                      extends Node
  case object IfStatement                     extends Node
  case object Import                          extends Node
  case object ImportAttribute                 extends Node
  case object ImportDeclaration               extends Node
  case object ImportDefaultSpecifier          extends Node
  case object ImportNamespaceSpecifier        extends Node
  case object ImportSpecifier                 extends Node
  case object IndexedAccessType               extends Node
  case object InferredPredicate               extends Node
  case object InterfaceDeclaration            extends Node
  case object InterfaceExtends                extends Node
  case object InterfaceTypeAnnotation         extends Node
  case object InterpreterDirective            extends Node
  case object IntersectionTypeAnnotation      extends Node
  case object JSXAttribute                    extends Node
  case object JSXClosingElement               extends Node
  case object JSXClosingFragment              extends Node
  case object JSXElement                      extends Node
  case object JSXEmptyExpression              extends Node
  case object JSXExpressionContainer          extends Node
  case object JSXFragment                     extends Node
  case object JSXIdentifier                   extends Node
  case object JSXMemberExpression             extends Node
  case object JSXNamespacedName               extends Node
  case object JSXOpeningElement               extends Node
  case object JSXOpeningFragment              extends Node
  case object JSXSpreadAttribute              extends Node
  case object JSXSpreadChild                  extends Node
  case object JSXText                         extends Node
  case object LabeledStatement                extends Node
  case object LogicalExpression               extends Node
  case object MemberExpression                extends Node
  case object MetaProperty                    extends Node
  case object MixedTypeAnnotation             extends Node
  case object ModuleExpression                extends Node
  case object NewExpression                   extends Node
  case object Noop                            extends Node
  case object NullLiteral                     extends Node
  case object NullLiteralTypeAnnotation       extends Node
  case object NullableTypeAnnotation          extends Node
  case object NumberLiteral                   extends Node
  case object NumberLiteralTypeAnnotation     extends Node
  case object NumberTypeAnnotation            extends Node
  case object NumericLiteral                  extends Node
  case object ObjectExpression                extends Node
  case object ObjectMethod                    extends Node
  case object ObjectPattern                   extends Node
  case object ObjectProperty                  extends Node
  case object ObjectTypeAnnotation            extends Node
  case object ObjectTypeCallProperty          extends Node
  case object ObjectTypeIndexer               extends Node
  case object ObjectTypeInternalSlot          extends Node
  case object ObjectTypeProperty              extends Node
  case object ObjectTypeSpreadProperty        extends Node
  case object OpaqueType                      extends Node
  case object OptionalCallExpression          extends Node
  case object OptionalIndexedAccessType       extends Node
  case object OptionalMemberExpression        extends Node
  case object ParenthesizedExpression         extends Node
  case object PipelineBareFunction            extends Node
  case object PipelinePrimaryTopicReference   extends Node
  case object PipelineTopicExpression         extends Node
  case object Placeholder                     extends Node
  case object PrivateName                     extends Node
  case object Program                         extends Node
  case object QualifiedTypeIdentifier         extends Node
  case object RecordExpression                extends Node
  case object RegExpLiteral                   extends Node
  case object RegexLiteral                    extends Node
  case object RestElement                     extends Node
  case object RestProperty                    extends Node
  case object ReturnStatement                 extends Node
  case object SequenceExpression              extends Node
  case object SpreadElement                   extends Node
  case object SpreadProperty                  extends Node
  case object StaticBlock                     extends Node
  case object StringLiteral                   extends Node
  case object StringLiteralTypeAnnotation     extends Node
  case object StringTypeAnnotation            extends Node
  case object Super                           extends Node
  case object SwitchCase                      extends Node
  case object SwitchStatement                 extends Node
  case object SymbolTypeAnnotation            extends Node
  case object TSAnyKeyword                    extends Node
  case object TSArrayType                     extends Node
  case object TSAsExpression                  extends Node
  case object TSBigIntKeyword                 extends Node
  case object TSBooleanKeyword                extends Node
  case object TSCallSignatureDeclaration      extends Node
  case object TSConditionalType               extends Node
  case object TSConstructSignatureDeclaration extends Node
  case object TSConstructorType               extends Node
  case object TSDeclareFunction               extends Node
  case object TSDeclareMethod                 extends Node
  case object TSEnumDeclaration               extends Node
  case object TSEnumMember                    extends Node
  case object TSExportAssignment              extends Node
  case object TSExpressionWithTypeArguments   extends Node
  case object TSExternalModuleReference       extends Node
  case object TSFunctionType                  extends Node
  case object TSImportEqualsDeclaration       extends Node
  case object TSImportType                    extends Node
  case object TSIndexSignature                extends Node
  case object TSIndexedAccessType             extends Node
  case object TSInferType                     extends Node
  case object TSInterfaceBody                 extends Node
  case object TSInterfaceDeclaration          extends Node
  case object TSIntersectionType              extends Node
  case object TSIntrinsicKeyword              extends Node
  case object TSLiteralType                   extends Node
  case object TSMappedType                    extends Node
  case object TSMethodSignature               extends Node
  case object TSModuleBlock                   extends Node
  case object TSModuleDeclaration             extends Node
  case object TSNamedTupleMember              extends Node
  case object TSNamespaceExportDeclaration    extends Node
  case object TSNeverKeyword                  extends Node
  case object TSNonNullExpression             extends Node
  case object TSNullKeyword                   extends Node
  case object TSNumberKeyword                 extends Node
  case object TSObjectKeyword                 extends Node
  case object TSOptionalType                  extends Node
  case object TSParameterProperty             extends Node
  case object TSParenthesizedType             extends Node
  case object TSPropertySignature             extends Node
  case object TSQualifiedName                 extends Node
  case object TSRestType                      extends Node
  case object TSStringKeyword                 extends Node
  case object TSSymbolKeyword                 extends Node
  case object TSThisType                      extends Node
  case object TSTupleType                     extends Node
  case object TSTypeAliasDeclaration          extends Node
  case object TSTypeAnnotation                extends Node
  case object TSTypeAssertion                 extends Node
  case object TSTypeLiteral                   extends Node
  case object TSTypeOperator                  extends Node
  case object TSTypeParameter                 extends Node
  case object TSTypeParameterDeclaration      extends Node
  case object TSTypeParameterInstantiation    extends Node
  case object TSTypePredicate                 extends Node
  case object TSTypeQuery                     extends Node
  case object TSTypeReference                 extends Node
  case object TSUndefinedKeyword              extends Node
  case object TSUnionType                     extends Node
  case object TSUnknownKeyword                extends Node
  case object TSVoidKeyword                   extends Node
  case object TaggedTemplateExpression        extends Node
  case object TemplateElement                 extends Node
  case object TemplateLiteral                 extends Node
  case object ThisExpression                  extends Node
  case object ThisTypeAnnotation              extends Node
  case object ThrowStatement                  extends Node
  case object TopicReference                  extends Node
  case object TryStatement                    extends Node
  case object TupleExpression                 extends Node
  case object TupleTypeAnnotation             extends Node
  case object TypeAlias                       extends Node
  case object TypeAnnotation                  extends Node
  case object TypeCastExpression              extends Node
  case object TypeParameter                   extends Node
  case object TypeParameterDeclaration        extends Node
  case object TypeParameterInstantiation      extends Node
  case object TypeofTypeAnnotation            extends Node
  case object UnaryExpression                 extends Node
  case object UnionTypeAnnotation             extends Node
  case object UpdateExpression                extends Node
  case object V8IntrinsicIdentifier           extends Node
  case object VariableDeclaration             extends Node
  case object VariableDeclarator              extends Node
  case object Variance                        extends Node
  case object VoidTypeAnnotation              extends Node
  case object WhileStatement                  extends Node
  case object WithStatement                   extends Node
  case object YieldExpression                 extends Node

}
