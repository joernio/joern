package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode

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
  private def astForFunctionDeclSyntax(node: FunctionDeclSyntax): Ast                   = notHandledYet(node)
  private def astForIfConfigDeclSyntax(node: IfConfigDeclSyntax): Ast                   = notHandledYet(node)
  private def astForImportDeclSyntax(node: ImportDeclSyntax): Ast                       = notHandledYet(node)
  private def astForInitializerDeclSyntax(node: InitializerDeclSyntax): Ast             = notHandledYet(node)
  private def astForMacroDeclSyntax(node: MacroDeclSyntax): Ast                         = notHandledYet(node)
  private def astForMacroExpansionDeclSyntax(node: MacroExpansionDeclSyntax): Ast       = notHandledYet(node)
  private def astForMissingDeclSyntax(node: MissingDeclSyntax): Ast                     = notHandledYet(node)
  private def astForOperatorDeclSyntax(node: OperatorDeclSyntax): Ast                   = notHandledYet(node)
  private def astForPoundSourceLocationSyntax(node: PoundSourceLocationSyntax): Ast     = notHandledYet(node)
  private def astForPrecedenceGroupDeclSyntax(node: PrecedenceGroupDeclSyntax): Ast     = notHandledYet(node)
  private def astForProtocolDeclSyntax(node: ProtocolDeclSyntax): Ast                   = notHandledYet(node)
  private def astForStructDeclSyntax(node: StructDeclSyntax): Ast                       = notHandledYet(node)
  private def astForSubscriptDeclSyntax(node: SubscriptDeclSyntax): Ast                 = notHandledYet(node)
  private def astForTypeAliasDeclSyntax(node: TypeAliasDeclSyntax): Ast                 = notHandledYet(node)
  private def astForVariableDeclSyntax(node: VariableDeclSyntax): Ast                   = notHandledYet(node)

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
