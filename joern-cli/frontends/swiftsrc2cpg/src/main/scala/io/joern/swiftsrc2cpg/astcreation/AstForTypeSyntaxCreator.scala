package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl

import scala.annotation.unused

trait AstForTypeSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def typeDeclForTypeSyntax(node: TypeSyntax): NewTypeDecl = {
    val TypeInfo(typeName, typeFullName) = typeNameInfoForTypeSyntax(node)
    val typeDeclNode_                    = typeDeclNode(node, typeName, typeFullName, parserResult.filename, code(node))
    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)
    typeDeclNode_
  }

  private def astForArrayTypeSyntax(node: ArrayTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForAttributedTypeSyntax(node: AttributedTypeSyntax): Ast = {
    astForTypeSyntax(node.baseType)
  }

  private def astForClassRestrictionTypeSyntax(node: ClassRestrictionTypeSyntax): Ast = {
    Ast(identifierNode(node, code(node)))
  }

  private def astForCompositionTypeSyntax(node: CompositionTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForDictionaryTypeSyntax(node: DictionaryTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForFunctionTypeSyntax(node: FunctionTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForIdentifierTypeSyntax(node: IdentifierTypeSyntax): Ast = {
    val nodeCode = code(node)
    val (typeFullName, possibleTypes) = fullnameProvider.typeFullname(node) match {
      case Some(tpe) => (tpe, Seq.empty)
      case None =>
        val tpe = AstCreatorHelper.cleanType(nodeCode) match {
          case value if Defines.SwiftTypes.contains(value) => value
          case _                                           => Defines.Any
        }
        (tpe, Seq(nodeCode))
    }
    registerType(typeFullName)
    Ast(identifierNode(node, nodeCode, nodeCode, typeFullName, possibleTypes))
  }

  private def astForImplicitlyUnwrappedOptionalTypeSyntax(node: ImplicitlyUnwrappedOptionalTypeSyntax): Ast = {
    astForTypeSyntax(node.wrappedType)
  }

  private def astForMemberTypeSyntax(node: MemberTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForMetatypeTypeSyntax(node: MetatypeTypeSyntax): Ast = {
    astForTypeSyntax(node.baseType)
  }

  private def astForMissingTypeSyntax(@unused node: MissingTypeSyntax): Ast = Ast()

  private def astForNamedOpaqueReturnTypeSyntax(node: NamedOpaqueReturnTypeSyntax): Ast = {
    astForTypeSyntax(node.`type`)
  }

  private def astForOptionalTypeSyntax(node: OptionalTypeSyntax): Ast = {
    astForTypeSyntax(node.wrappedType)
  }

  private def astForPackElementTypeSyntax(node: PackElementTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForPackExpansionTypeSyntax(node: PackExpansionTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForSomeOrAnyTypeSyntax(node: SomeOrAnyTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

  private def astForSuppressedTypeSyntax(node: SuppressedTypeSyntax): Ast = {
    astForTypeSyntax(node.`type`)
  }

  private def astForTupleTypeSyntax(node: TupleTypeSyntax): Ast = {
    Ast(typeDeclForTypeSyntax(node))
  }

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
