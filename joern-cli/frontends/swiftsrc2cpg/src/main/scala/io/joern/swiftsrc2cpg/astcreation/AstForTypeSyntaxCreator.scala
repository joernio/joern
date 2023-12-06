package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.TypeSyntax
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode

trait AstForTypeSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForArrayTypeSyntax(node: ArrayTypeSyntax): Ast =
    notHandledYet(node)

  private def astForAttributedTypeSyntax(node: AttributedTypeSyntax): Ast =
    notHandledYet(node)

  private def astForClassRestrictionTypeSyntax(node: ClassRestrictionTypeSyntax): Ast =
    notHandledYet(node)

  private def astForCompositionTypeSyntax(node: CompositionTypeSyntax): Ast =
    notHandledYet(node)

  private def astForDictionaryTypeSyntax(node: DictionaryTypeSyntax): Ast =
    notHandledYet(node)

  private def astForFunctionTypeSyntax(node: FunctionTypeSyntax): Ast =
    notHandledYet(node)

  private def astForIdentifierTypeSyntax(node: IdentifierTypeSyntax): Ast =
    notHandledYet(node)

  private def astForImplicitlyUnwrappedOptionalTypeSyntax(node: ImplicitlyUnwrappedOptionalTypeSyntax): Ast =
    notHandledYet(node)

  private def astForMemberTypeSyntax(node: MemberTypeSyntax): Ast =
    notHandledYet(node)

  private def astForMetatypeTypeSyntax(node: MetatypeTypeSyntax): Ast =
    notHandledYet(node)

  private def astForMissingTypeSyntax(node: MissingTypeSyntax): Ast =
    notHandledYet(node)

  private def astForNamedOpaqueReturnTypeSyntax(node: NamedOpaqueReturnTypeSyntax): Ast =
    notHandledYet(node)

  private def astForOptionalTypeSyntax(node: OptionalTypeSyntax): Ast =
    notHandledYet(node)

  private def astForPackElementTypeSyntax(node: PackElementTypeSyntax): Ast =
    notHandledYet(node)

  private def astForPackExpansionTypeSyntax(node: PackExpansionTypeSyntax): Ast =
    notHandledYet(node)

  private def astForSomeOrAnyTypeSyntax(node: SomeOrAnyTypeSyntax): Ast =
    notHandledYet(node)

  private def astForSuppressedTypeSyntax(node: SuppressedTypeSyntax): Ast =
    notHandledYet(node)

  private def astForTupleTypeSyntax(node: TupleTypeSyntax): Ast =
    notHandledYet(node)

  protected def astForTypeSyntax(typeSyntax: TypeSyntax): Ast = typeSyntax match {
    case node: ArrayTypeSyntax                       => astForArrayTypeSyntax(node)
    case node: AttributedTypeSyntax                  => astForAttributedTypeSyntax(node)
    case node: ClassRestrictionTypeSyntax            => astForClassRestrictionTypeSyntax(node)
    case node: CompositionTypeSyntax                 => astForCompositionTypeSyntax(node)
    case node: DictionaryTypeSyntax                  => astForDictionaryTypeSyntax(node)
    case node: FunctionTypeSyntax                    => astForFunctionTypeSyntax(node)
    case node: IdentifierTypeSyntax                  => astForIdentifierTypeSyntax(node)
    case node: ImplicitlyUnwrappedOptionalTypeSyntax => astForImplicitlyUnwrappedOptionalTypeSyntax(node)
    case node: MemberTypeSyntax                      => astForMemberTypeSyntax(node)
    case node: MetatypeTypeSyntax                    => astForMetatypeTypeSyntax(node)
    case node: MissingTypeSyntax                     => astForMissingTypeSyntax(node)
    case node: NamedOpaqueReturnTypeSyntax           => astForNamedOpaqueReturnTypeSyntax(node)
    case node: OptionalTypeSyntax                    => astForOptionalTypeSyntax(node)
    case node: PackElementTypeSyntax                 => astForPackElementTypeSyntax(node)
    case node: PackExpansionTypeSyntax               => astForPackExpansionTypeSyntax(node)
    case node: SomeOrAnyTypeSyntax                   => astForSomeOrAnyTypeSyntax(node)
    case node: SuppressedTypeSyntax                  => astForSuppressedTypeSyntax(node)
    case node: TupleTypeSyntax                       => astForTupleTypeSyntax(node)
  }

}
