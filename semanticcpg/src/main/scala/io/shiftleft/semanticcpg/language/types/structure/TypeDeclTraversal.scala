package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, Properties, nodes}
import io.shiftleft.semanticcpg.language._
import overflowdb._
import overflowdb.traversal.{Traversal, toElementTraversal, toNodeTraversal}

/** Type declaration - possibly a template that requires instantiation
  */
class TypeDeclTraversal(val traversal: Traversal[TypeDecl]) extends AnyVal {
  import TypeDeclTraversal._

  /** Annotations of the type declaration
    */
  def annotation: Traversal[nodes.Annotation] =
    traversal.flatMap(_._annotationViaAstOut)

  /** Types referencing to this type declaration.
    */
  def referencingType: Traversal[Type] =
    traversal.in(EdgeTypes.REF).cast[Type]

  /** Namespace in which this type declaration is defined
    */
  def namespace: Traversal[Namespace] =
    traversal
      .in(EdgeTypes.AST)
      .hasLabel(NodeTypes.NAMESPACE_BLOCK)
      .out(EdgeTypes.REF)
      .cast[Namespace]

  /** Methods defined as part of this type
    */
  def method: Traversal[Method] =
    canonicalType.out(EdgeTypes.AST).hasLabel(NodeTypes.METHOD).cast[Method]

  /** Filter for type declarations contained in the analyzed code.
    */
  def internal: Traversal[TypeDecl] =
    canonicalType.has(Properties.IS_EXTERNAL -> false)

  /** Filter for type declarations not contained in the analyzed code.
    */
  def external: Traversal[TypeDecl] =
    canonicalType.has(Properties.IS_EXTERNAL -> true)

  /** Member variables
    */
  def member: Traversal[Member] =
    canonicalType.out.hasLabel(NodeTypes.MEMBER).cast[Member]

  /** Direct base types in the inheritance graph.
    */
  def baseType: Traversal[Type] =
    canonicalType.out(EdgeTypes.INHERITS_FROM).cast[Type]

  /** Direct base type declaration.
    */
  def derivedTypeDecl: Traversal[TypeDecl] =
    referencingType.derivedTypeDecl

  /** Direct and transitive base type declaration.
    */
  def derivedTypeDeclTransitive: Traversal[TypeDecl] =
    traversal.repeat(_.derivedTypeDecl)(_.emitAllButFirst)

  /** Direct base type declaration.
    */
  def baseTypeDecl: Traversal[TypeDecl] =
    traversal.baseType.referencedTypeDecl

  /** Direct and transitive base type declaration.
    */
  def baseTypeDeclTransitive: Traversal[TypeDecl] =
    traversal.repeat(_.baseTypeDecl)(_.emitAllButFirst)

  /** Traverse to alias type declarations.
    */
  def isAlias: Traversal[TypeDecl] =
    traversal.filter(_.aliasTypeFullName.isDefined)

  /** Traverse to canonical type declarations.
    */
  def isCanonical: Traversal[TypeDecl] =
    traversal.filter(_.aliasTypeFullName.isEmpty)

  /** If this is an alias type declaration, go to its underlying type declaration else unchanged.
    */
  def unravelAlias: Traversal[TypeDecl] = {
    traversal.map { typeDecl =>
      val alias = for {
        tpe      <- typeDecl.aliasedType.nextOption()
        typeDecl <- tpe.referencedTypeDecl.nextOption()
      } yield typeDecl

      alias.getOrElse(typeDecl)
    }
  }

  /** Traverse to canonical type which means unravel aliases until we find a non alias type declaration.
    */
  def canonicalType: Traversal[TypeDecl] =
    traversal.repeat(_.unravelAlias)(_.until(_.isCanonical).times(maxAliasExpansions))

  /** Direct alias type declarations.
    */
  def aliasTypeDecl: Traversal[TypeDecl] =
    referencingType.aliasTypeDecl

  /** Direct and transitive alias type declarations.
    */
  def aliasTypeDeclTransitive: Traversal[TypeDecl] =
    traversal.repeat(_.aliasTypeDecl)(_.emitAllButFirst)

}

object TypeDeclTraversal {
  private val maxAliasExpansions = 100
}
