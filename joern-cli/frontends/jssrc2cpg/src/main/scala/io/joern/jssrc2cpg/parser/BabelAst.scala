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

  sealed trait Expression extends BabelNode

  sealed trait Binary extends BabelNode

  sealed trait Scopable extends BabelNode

  sealed trait BlockParent extends BabelNode

  sealed trait Block extends BabelNode

  sealed trait Statement extends BabelNode

  sealed trait Conditional extends BabelNode

  sealed trait Loop extends BabelNode

  sealed trait While extends BabelNode

  sealed trait For extends BabelNode

  sealed trait Function extends BabelNode

  sealed trait Declaration extends BabelNode

  sealed trait PatternLike extends BabelNode

  sealed trait LVal extends BabelNode

  sealed trait TSEntityName extends BabelNode

  sealed trait Method extends BabelNode

  sealed trait ObjectMember extends BabelNode

  sealed trait Pattern extends BabelNode

  sealed trait Class extends BabelNode

  sealed trait ModuleDeclaration extends BabelNode

  sealed trait ExportDeclaration extends BabelNode

  sealed trait Flow extends BabelNode

  sealed trait FlowType extends BabelNode

  sealed trait TSTypeElement extends BabelNode

  sealed trait TSType extends BabelNode

  case object AnyTypeAnnotation            extends BabelNode with Flow with FlowType
  case object ArgumentPlaceholder          extends BabelNode
  case object ArrayExpression              extends Expression
  case object ArrayPattern                 extends BabelNode with PatternLike with LVal with Pattern
  case object ArrayTypeAnnotation          extends BabelNode with Flow with FlowType
  case object ArrowFunctionExpression      extends Expression with Scopable with BlockParent with Function
  case object AssignmentExpression         extends Expression
  case object AssignmentPattern            extends BabelNode with PatternLike with LVal with Pattern
  case object AwaitExpression              extends Expression
  case object BigIntLiteral                extends Expression
  case object BinaryExpression             extends Expression with Binary
  case object BindExpression               extends Expression
  case object BlockStatement               extends BabelNode with Scopable with BlockParent with Block with Statement
  case object BooleanLiteral               extends Expression
  case object BooleanLiteralTypeAnnotation extends BabelNode with Flow with FlowType
  case object BooleanTypeAnnotation        extends BabelNode with Flow with FlowType
  case object BreakStatement               extends BabelNode with Statement
  case object CallExpression               extends Expression
  case object CatchClause                  extends BabelNode with Scopable with BlockParent
  case object ClassAccessorProperty        extends BabelNode
  case object ClassBody                    extends BabelNode
  case object ClassDeclaration             extends BabelNode with Scopable with Statement with Declaration with Class
  case object ClassExpression              extends Expression with Scopable with Class
  case object ClassImplements              extends BabelNode with Flow
  case object ClassMethod                  extends BabelNode with Scopable with BlockParent with Function with Method
  case object ClassPrivateMethod           extends BabelNode with Scopable with BlockParent with Function with Method
  case object ClassPrivateProperty         extends BabelNode
  case object ClassProperty                extends BabelNode
  case object ConditionalExpression        extends Expression with Conditional
  case object ContinueStatement            extends BabelNode with Statement
  case object DebuggerStatement            extends BabelNode with Statement
  case object DecimalLiteral               extends Expression
  case object DeclareClass                 extends BabelNode with Statement with Declaration with Flow
  case object DeclareExportAllDeclaration  extends BabelNode with Statement with Declaration with Flow
  case object DeclareExportDeclaration     extends BabelNode with Statement with Declaration with Flow
  case object DeclareFunction              extends BabelNode with Statement with Declaration with Flow
  case object DeclareInterface             extends BabelNode with Statement with Declaration with Flow
  case object DeclareModule                extends BabelNode with Statement with Declaration with Flow
  case object DeclareModuleExports         extends BabelNode with Statement with Declaration with Flow
  case object DeclareOpaqueType            extends BabelNode with Statement with Declaration with Flow
  case object DeclareTypeAlias             extends BabelNode with Statement with Declaration with Flow
  case object DeclareVariable              extends BabelNode with Statement with Declaration with Flow
  case object DeclaredPredicate            extends BabelNode with Flow
  case object Decorator                    extends BabelNode
  case object Directive                    extends BabelNode
  case object DirectiveLiteral             extends BabelNode
  case object DoExpression                 extends Expression
  case object DoWhileStatement     extends BabelNode with Scopable with BlockParent with Statement with Loop with While
  case object EmptyStatement       extends BabelNode with Statement
  case object EmptyTypeAnnotation  extends BabelNode with Flow with FlowType
  case object EnumBooleanBody      extends BabelNode with Flow
  case object EnumBooleanMember    extends BabelNode with Flow
  case object EnumDeclaration      extends BabelNode with Statement with Declaration with Flow
  case object EnumDefaultedMember  extends BabelNode with Flow
  case object EnumNumberBody       extends BabelNode with Flow
  case object EnumNumberMember     extends BabelNode with Flow
  case object EnumStringBody       extends BabelNode with Flow
  case object EnumStringMember     extends BabelNode with Flow
  case object EnumSymbolBody       extends BabelNode with Flow
  case object ExistsTypeAnnotation extends BabelNode with FlowType
  case object ExportAllDeclaration
      extends BabelNode
      with Statement
      with Declaration
      with ModuleDeclaration
      with ExportDeclaration
  case object ExportDefaultDeclaration
      extends BabelNode
      with Statement
      with Declaration
      with ModuleDeclaration
      with ExportDeclaration
  case object ExportDefaultSpecifier extends BabelNode
  case object ExportNamedDeclaration
      extends BabelNode
      with Statement
      with Declaration
      with ModuleDeclaration
      with ExportDeclaration
  case object ExportNamespaceSpecifier extends BabelNode
  case object ExportSpecifier          extends BabelNode
  case object ExpressionStatement      extends BabelNode with Statement
  case object File                     extends BabelNode
  case object ForInStatement extends BabelNode with Scopable with BlockParent with Statement with Loop with For
  case object ForOfStatement extends BabelNode with Scopable with BlockParent with Statement with Loop with For
  case object ForStatement   extends BabelNode with Scopable with BlockParent with Statement with Loop with For
  case object FunctionDeclaration
      extends BabelNode
      with Scopable
      with BlockParent
      with Statement
      with Function
      with Declaration
  case object FunctionExpression          extends Expression with Scopable with BlockParent with Function
  case object FunctionTypeAnnotation      extends BabelNode with FlowType
  case object FunctionTypeParam           extends BabelNode
  case object GenericTypeAnnotation       extends BabelNode with FlowType
  case object Identifier                  extends Expression with PatternLike with LVal with TSEntityName
  case object IfStatement                 extends BabelNode with Statement with Conditional
  case object Import                      extends Expression
  case object ImportAttribute             extends BabelNode
  case object ImportDeclaration           extends BabelNode with Statement with Declaration with ModuleDeclaration
  case object ImportDefaultSpecifier      extends BabelNode
  case object ImportNamespaceSpecifier    extends BabelNode
  case object ImportSpecifier             extends BabelNode
  case object IndexedAccessType           extends BabelNode with Flow with FlowType
  case object InferredPredicate           extends BabelNode
  case object InterfaceDeclaration        extends BabelNode with Statement with Declaration with Flow
  case object InterfaceExtends            extends BabelNode with Flow
  case object InterfaceTypeAnnotation     extends BabelNode with Flow with FlowType
  case object InterpreterDirective        extends BabelNode
  case object IntersectionTypeAnnotation  extends BabelNode with Flow with FlowType
  case object JSXAttribute                extends BabelNode
  case object JSXClosingElement           extends BabelNode
  case object JSXClosingFragment          extends BabelNode
  case object JSXElement                  extends Expression
  case object JSXEmptyExpression          extends Expression
  case object JSXExpressionContainer      extends BabelNode
  case object JSXFragment                 extends Expression
  case object JSXIdentifier               extends BabelNode
  case object JSXMemberExpression         extends Expression
  case object JSXNamespacedName           extends BabelNode
  case object JSXOpeningElement           extends BabelNode
  case object JSXOpeningFragment          extends BabelNode
  case object JSXSpreadAttribute          extends BabelNode
  case object JSXSpreadChild              extends BabelNode
  case object JSXText                     extends BabelNode
  case object LabeledStatement            extends BabelNode with Statement
  case object LogicalExpression           extends Expression with Binary
  case object MemberExpression            extends Expression with LVal
  case object MetaProperty                extends Expression
  case object MixedTypeAnnotation         extends BabelNode with Flow with FlowType
  case object ModuleExpression            extends Expression
  case object NewExpression               extends Expression
  case object Noop                        extends BabelNode
  case object NullLiteral                 extends Expression
  case object NullLiteralTypeAnnotation   extends BabelNode with Flow with FlowType
  case object NullableTypeAnnotation      extends BabelNode with Flow with FlowType
  case object NumberLiteral               extends BabelNode
  case object NumberLiteralTypeAnnotation extends BabelNode with Flow with FlowType
  case object NumberTypeAnnotation        extends BabelNode with Flow with FlowType
  case object NumericLiteral              extends Expression
  case object ObjectExpression            extends Expression
  case object ObjectMethod  extends BabelNode with Scopable with BlockParent with Function with Method with ObjectMember
  case object ObjectPattern extends BabelNode with PatternLike with LVal with Pattern
  case object ObjectProperty                  extends BabelNode with ObjectMember
  case object ObjectTypeAnnotation            extends BabelNode with Flow with FlowType
  case object ObjectTypeCallProperty          extends BabelNode with Flow
  case object ObjectTypeIndexer               extends BabelNode with Flow
  case object ObjectTypeInternalSlot          extends BabelNode with Flow
  case object ObjectTypeProperty              extends BabelNode with Flow
  case object ObjectTypeSpreadProperty        extends BabelNode with Flow
  case object OpaqueType                      extends BabelNode with Statement with Declaration with Flow
  case object OptionalCallExpression          extends Expression
  case object OptionalIndexedAccessType       extends BabelNode with Flow with FlowType
  case object OptionalMemberExpression        extends Expression
  case object ParenthesizedExpression         extends Expression
  case object PipelineBareFunction            extends Expression
  case object PipelinePrimaryTopicReference   extends Expression
  case object PipelineTopicExpression         extends Expression
  case object Placeholder                     extends BabelNode
  case object PrivateName                     extends BabelNode
  case object Program                         extends BabelNode with Scopable with BlockParent with Block
  case object QualifiedTypeIdentifier         extends BabelNode with Flow
  case object RecordExpression                extends Expression
  case object RegExpLiteral                   extends Expression
  case object RegexLiteral                    extends BabelNode
  case object RestElement                     extends BabelNode with PatternLike with LVal
  case object RestProperty                    extends BabelNode
  case object ReturnStatement                 extends BabelNode with Statement
  case object SequenceExpression              extends Expression
  case object SpreadElement                   extends BabelNode
  case object SpreadProperty                  extends BabelNode
  case object StaticBlock                     extends BabelNode with Scopable with BlockParent
  case object StringLiteral                   extends Expression
  case object StringLiteralTypeAnnotation     extends BabelNode with Flow with FlowType
  case object StringTypeAnnotation            extends BabelNode with Flow with FlowType
  case object Super                           extends Expression
  case object SwitchCase                      extends BabelNode
  case object SwitchStatement                 extends BabelNode with Scopable with BlockParent with Statement
  case object SymbolTypeAnnotation            extends BabelNode with Flow with FlowType
  case object TSAnyKeyword                    extends BabelNode with TSType
  case object TSArrayType                     extends BabelNode with TSType
  case object TSAsExpression                  extends Expression
  case object TSBigIntKeyword                 extends BabelNode with TSType
  case object TSBooleanKeyword                extends BabelNode with TSType
  case object TSCallSignatureDeclaration      extends BabelNode with Declaration with TSTypeElement
  case object TSConditionalType               extends BabelNode with TSType
  case object TSConstructSignatureDeclaration extends BabelNode with Declaration
  case object TSConstructorType               extends BabelNode with TSType
  case object TSDeclareFunction               extends BabelNode with Statement
  case object TSDeclareMethod                 extends BabelNode
  case object TSEnumDeclaration               extends BabelNode with Statement with Declaration
  case object TSEnumMember                    extends BabelNode
  case object TSExportAssignment              extends BabelNode with Statement
  case object TSExpressionWithTypeArguments   extends BabelNode with TSType
  case object TSExternalModuleReference       extends BabelNode
  case object TSFunctionType                  extends BabelNode with TSType
  case object TSImportEqualsDeclaration       extends BabelNode with Statement with Declaration
  case object TSImportType                    extends BabelNode with TSType
  case object TSIndexSignature                extends BabelNode with TSTypeElement
  case object TSIndexedAccessType             extends BabelNode with TSType
  case object TSInferType                     extends BabelNode with TSType
  case object TSInterfaceBody                 extends BabelNode
  case object TSInterfaceDeclaration          extends BabelNode with Statement with Declaration
  case object TSIntersectionType              extends BabelNode with TSType
  case object TSIntrinsicKeyword              extends BabelNode with TSType
  case object TSLiteralType                   extends BabelNode with TSType
  case object TSMappedType                    extends BabelNode with TSType
  case object TSMethodSignature               extends BabelNode with TSTypeElement
  case object TSModuleBlock                   extends BabelNode with Scopable with BlockParent with Block
  case object TSModuleDeclaration             extends BabelNode with Statement with Declaration
  case object TSNamedTupleMember              extends BabelNode
  case object TSNamespaceExportDeclaration    extends BabelNode with Statement with Declaration
  case object TSNeverKeyword                  extends BabelNode with TSType
  case object TSNonNullExpression             extends Expression
  case object TSNullKeyword                   extends BabelNode with TSType
  case object TSNumberKeyword                 extends BabelNode with TSType
  case object TSObjectKeyword                 extends BabelNode with TSType
  case object TSOptionalType                  extends BabelNode with TSType
  case object TSParameterProperty             extends BabelNode with LVal
  case object TSParenthesizedType             extends BabelNode with TSType
  case object TSPropertySignature             extends BabelNode with TSTypeElement
  case object TSQualifiedName                 extends BabelNode with TSEntityName
  case object TSRestType                      extends BabelNode with TSType
  case object TSStringKeyword                 extends BabelNode with TSType
  case object TSSymbolKeyword                 extends BabelNode with TSType
  case object TSThisType                      extends BabelNode with TSType
  case object TSTupleType                     extends BabelNode with TSType
  case object TSTypeAliasDeclaration          extends BabelNode with Statement with Declaration
  case object TSTypeAnnotation                extends BabelNode with FlowType
  case object TSTypeAssertion                 extends Expression
  case object TSTypeLiteral                   extends BabelNode with TSType
  case object TSTypeOperator                  extends BabelNode with TSType
  case object TSTypeParameter                 extends BabelNode with TSType
  case object TSTypeParameterDeclaration      extends BabelNode with Declaration
  case object TSTypeParameterInstantiation    extends BabelNode
  case object TSTypePredicate                 extends BabelNode with TSType
  case object TSTypeQuery                     extends BabelNode with TSType
  case object TSTypeReference                 extends BabelNode with TSType
  case object TSUndefinedKeyword              extends BabelNode with TSType
  case object TSUnionType                     extends BabelNode with TSType
  case object TSUnknownKeyword                extends BabelNode with TSType
  case object TSVoidKeyword                   extends BabelNode with TSType
  case object TaggedTemplateExpression        extends Expression
  case object TemplateElement                 extends BabelNode
  case object TemplateLiteral                 extends Expression
  case object ThisExpression                  extends Expression
  case object ThisTypeAnnotation              extends BabelNode with Flow with FlowType
  case object ThrowStatement                  extends BabelNode with Statement
  case object TopicReference                  extends Expression
  case object TryStatement                    extends BabelNode with Statement
  case object TupleExpression                 extends Expression
  case object TupleTypeAnnotation             extends BabelNode with Flow with FlowType
  case object TypeAlias                       extends BabelNode with Statement with Declaration with Flow
  case object TypeAnnotation                  extends BabelNode with Flow with FlowType
  case object TypeCastExpression              extends Expression with Flow
  case object TypeParameter                   extends BabelNode with Flow
  case object TypeParameterDeclaration        extends BabelNode with Declaration with Flow
  case object TypeParameterInstantiation      extends BabelNode with Flow
  case object TypeofTypeAnnotation            extends BabelNode with Flow with FlowType
  case object UnaryExpression                 extends Expression
  case object UnionTypeAnnotation             extends BabelNode with Flow with FlowType
  case object UpdateExpression                extends Expression
  case object V8IntrinsicIdentifier           extends BabelNode
  case object VariableDeclaration             extends BabelNode with Statement with Declaration
  case object VariableDeclarator              extends BabelNode
  case object Variance                        extends BabelNode with Flow
  case object VoidTypeAnnotation              extends BabelNode with Flow with FlowType
  case object WhileStatement  extends BabelNode with Scopable with BlockParent with Statement with Loop with While
  case object WithStatement   extends BabelNode with Statement
  case object YieldExpression extends Expression

}
