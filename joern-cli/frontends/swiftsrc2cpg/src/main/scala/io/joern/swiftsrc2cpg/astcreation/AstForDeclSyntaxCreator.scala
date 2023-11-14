package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.shiftleft.codepropertygraph.generated.nodes.NewBinding
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import org.apache.commons.lang.StringUtils

trait AstForDeclSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForAccessorDeclSyntax(node: AccessorDeclSyntax): Ast                   = notHandledYet(node)
  private def astForActorDeclSyntax(node: ActorDeclSyntax): Ast                         = notHandledYet(node)
  private def astForAssociatedTypeDeclSyntax(node: AssociatedTypeDeclSyntax): Ast       = notHandledYet(node)
  private def astForClassDeclSyntax(node: ClassDeclSyntax): Ast                         = notHandledYet(node)
  private def astForDeinitializerDeclSyntax(node: DeinitializerDeclSyntax): Ast         = notHandledYet(node)
  private def astForEditorPlaceholderDeclSyntax(node: EditorPlaceholderDeclSyntax): Ast = notHandledYet(node)
  private def astForEnumCaseDeclSyntax(node: EnumCaseDeclSyntax): Ast                   = notHandledYet(node)
  private def astForEnumDeclSyntax(node: EnumDeclSyntax): Ast                           = notHandledYet(node)
  private def astForExtensionDeclSyntax(node: ExtensionDeclSyntax): Ast                 = notHandledYet(node)

  private def createFunctionTypeAndTypeDecl(
    node: FunctionDeclSyntax,
    method: NewMethod,
    methodName: String,
    methodFullName: String,
    signature: String
  ): Ast = {
    val normalizedName     = StringUtils.normalizeSpace(methodName)
    val normalizedFullName = StringUtils.normalizeSpace(methodFullName)

    val parentNode: NewTypeDecl = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }.getOrElse {
      val astParentType     = methodAstParentStack.head.label
      val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString
      val typeDeclNode_ = typeDeclNode(
        node,
        normalizedName,
        normalizedFullName,
        method.filename,
        normalizedName,
        astParentType,
        astParentFullName
      )
      Ast.storeInDiffGraph(Ast(typeDeclNode_), diffGraph)
      typeDeclNode_
    }

    method.astParentFullName = parentNode.fullName
    method.astParentType = parentNode.label
    val functionBinding = NewBinding().name(normalizedName).methodFullName(normalizedFullName).signature(signature)
    Ast(functionBinding).withBindsEdge(parentNode, functionBinding).withRefEdge(functionBinding, method)
  }

  private def astForFunctionDeclSyntax(node: FunctionDeclSyntax): Ast = {
    // TODO: handle genericParameterClause
    // TODO: handle genericWhereClause
    val attributes = node.attributes.children.map(astForNode)
    val modifiers =
      node.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier])) :+ NewModifier()
        .modifierType(ModifierTypes.VIRTUAL)
    val (methodName, methodFullName) = calcMethodNameAndFullName(node)
    val filename                     = parserResult.filename
    val returnType                   = node.signature.returnClause.map(c => code(c.`type`)).getOrElse(Defines.Any)
    registerType(returnType, returnType)

    val signature = s"$returnType $methodFullName ${code(node.signature.parameterClause)}"

    val codeString  = code(node)
    val methodNode_ = methodNode(node, methodName, codeString, methodFullName, Some(signature), filename)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullName, methodName, methodNode_, None)

    val parameterAsts = node.signature.parameterClause.parameters.children.map(astForNode)

    val methodReturnNode =
      newMethodReturnNode(returnType, dynamicTypeHintFullName = None, line = line(node), column = column(node))
    val astForMethodBody = node.body.map(astForNode).getOrElse(Ast())
    val astForMethod =
      methodAstWithAnnotations(
        methodNode_,
        parameterAsts,
        astForMethodBody,
        methodReturnNode,
        modifiers = modifiers,
        annotations = attributes
      )

    scope.popScope()
    methodAstParentStack.pop()

    val typeDeclAst = createFunctionTypeAndTypeDecl(node, methodNode_, methodName, methodFullName, signature)
    astForMethod.merge(typeDeclAst)
  }

  private def astForIfConfigDeclSyntax(node: IfConfigDeclSyntax): Ast               = notHandledYet(node)
  private def astForImportDeclSyntax(node: ImportDeclSyntax): Ast                   = notHandledYet(node)
  private def astForInitializerDeclSyntax(node: InitializerDeclSyntax): Ast         = notHandledYet(node)
  private def astForMacroDeclSyntax(node: MacroDeclSyntax): Ast                     = notHandledYet(node)
  private def astForMacroExpansionDeclSyntax(node: MacroExpansionDeclSyntax): Ast   = notHandledYet(node)
  private def astForMissingDeclSyntax(node: MissingDeclSyntax): Ast                 = notHandledYet(node)
  private def astForOperatorDeclSyntax(node: OperatorDeclSyntax): Ast               = notHandledYet(node)
  private def astForPoundSourceLocationSyntax(node: PoundSourceLocationSyntax): Ast = notHandledYet(node)
  private def astForPrecedenceGroupDeclSyntax(node: PrecedenceGroupDeclSyntax): Ast = notHandledYet(node)
  private def astForProtocolDeclSyntax(node: ProtocolDeclSyntax): Ast               = notHandledYet(node)
  private def astForStructDeclSyntax(node: StructDeclSyntax): Ast                   = notHandledYet(node)
  private def astForSubscriptDeclSyntax(node: SubscriptDeclSyntax): Ast             = notHandledYet(node)
  private def astForTypeAliasDeclSyntax(node: TypeAliasDeclSyntax): Ast             = notHandledYet(node)
  private def astForVariableDeclSyntax(node: VariableDeclSyntax): Ast               = notHandledYet(node)

  protected def astForDeclSyntax(declSyntax: DeclSyntax): Ast = declSyntax match {
    case node: AccessorDeclSyntax          => astForAccessorDeclSyntax(node)
    case node: ActorDeclSyntax             => astForActorDeclSyntax(node)
    case node: AssociatedTypeDeclSyntax    => astForAssociatedTypeDeclSyntax(node)
    case node: ClassDeclSyntax             => astForClassDeclSyntax(node)
    case node: DeinitializerDeclSyntax     => astForDeinitializerDeclSyntax(node)
    case node: EditorPlaceholderDeclSyntax => astForEditorPlaceholderDeclSyntax(node)
    case node: EnumCaseDeclSyntax          => astForEnumCaseDeclSyntax(node)
    case node: EnumDeclSyntax              => astForEnumDeclSyntax(node)
    case node: ExtensionDeclSyntax         => astForExtensionDeclSyntax(node)
    case node: FunctionDeclSyntax          => astForFunctionDeclSyntax(node)
    case node: IfConfigDeclSyntax          => astForIfConfigDeclSyntax(node)
    case node: ImportDeclSyntax            => astForImportDeclSyntax(node)
    case node: InitializerDeclSyntax       => astForInitializerDeclSyntax(node)
    case node: MacroDeclSyntax             => astForMacroDeclSyntax(node)
    case node: MacroExpansionDeclSyntax    => astForMacroExpansionDeclSyntax(node)
    case node: MissingDeclSyntax           => astForMissingDeclSyntax(node)
    case node: OperatorDeclSyntax          => astForOperatorDeclSyntax(node)
    case node: PoundSourceLocationSyntax   => astForPoundSourceLocationSyntax(node)
    case node: PrecedenceGroupDeclSyntax   => astForPrecedenceGroupDeclSyntax(node)
    case node: ProtocolDeclSyntax          => astForProtocolDeclSyntax(node)
    case node: StructDeclSyntax            => astForStructDeclSyntax(node)
    case node: SubscriptDeclSyntax         => astForSubscriptDeclSyntax(node)
    case node: TypeAliasDeclSyntax         => astForTypeAliasDeclSyntax(node)
    case node: VariableDeclSyntax          => astForVariableDeclSyntax(node)
  }

}
