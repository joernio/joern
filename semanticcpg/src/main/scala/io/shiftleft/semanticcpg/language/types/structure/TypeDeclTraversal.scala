package io.shiftleft.semanticcpg.language.types.structure

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

/** Type declaration - possibly a template that requires instantiation
  */
class TypeDeclTraversal(val traversal: Iterator[TypeDecl]) extends AnyVal {
  import TypeDeclTraversal.*

  /** Annotations of the type declaration
    */
  def annotation: Iterator[Annotation] =
    traversal.flatMap(_._annotationViaAstOut)

  /** Types referencing to this type declaration.
    */
  def referencingType: Iterator[Type] =
    traversal.refIn

  /** Namespace in which this type declaration is defined
    */
  def namespace: Iterator[Namespace] =
    traversal.flatMap(_.namespaceBlock).namespace

  /** Methods defined as part of this type
    */
  def method: Iterator[Method] =
    canonicalType.flatMap(_._methodViaAstOut)

  /** Filter for type declarations contained in the analyzed code.
    */
  def internal: Iterator[TypeDecl] =
    canonicalType.isExternal(false)

  /** Filter for type declarations not contained in the analyzed code.
    */
  def external: Iterator[TypeDecl] =
    canonicalType.isExternal(true)

  /** Member variables
    */
  def member: Iterator[Member] =
    canonicalType.flatMap(_._memberViaAstOut)

  /** Direct base types in the inheritance graph.
    */
  def baseType: Iterator[Type] =
    canonicalType.flatMap(_._typeViaInheritsFromOut)

  /** Direct base type declaration.
    */
  def derivedTypeDecl: Iterator[TypeDecl] =
    referencingType.derivedTypeDecl

  /** Direct and transitive base type declaration.
    */
  def derivedTypeDeclTransitive: Iterator[TypeDecl] =
    traversal.repeat(_.derivedTypeDecl)(_.emitAllButFirst.dedup)

  /** Direct base type declaration.
    */
  def baseTypeDecl: Iterator[TypeDecl] =
    traversal.baseType.referencedTypeDecl

  /** Direct and transitive base type declaration.
    */
  def baseTypeDeclTransitive: Iterator[TypeDecl] =
    traversal.repeat(_.baseTypeDecl)(_.emitAllButFirst.dedup)

  /** Traverse to alias type declarations.
    */
  def isAlias: Iterator[TypeDecl] =
    traversal.filter(_.aliasTypeFullName.isDefined)

  /** Traverse to canonical type declarations.
    */
  def isCanonical: Iterator[TypeDecl] =
    traversal.filter(_.aliasTypeFullName.isEmpty)

  /** If this is an alias type declaration, go to its underlying type declaration else unchanged.
    */
  def unravelAlias: Iterator[TypeDecl] = {
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
  def canonicalType: Iterator[TypeDecl] =
    traversal.repeat(_.unravelAlias)(_.until(_.isCanonical).maxDepth(maxAliasExpansions))

  /** Direct alias type declarations.
    */
  def aliasTypeDecl: Iterator[TypeDecl] =
    referencingType.aliasTypeDecl

  /** Direct and transitive alias type declarations.
    */
  def aliasTypeDeclTransitive: Iterator[TypeDecl] =
    traversal.repeat(_.aliasTypeDecl)(_.emitAllButFirst.dedup)

  def content: Iterator[String] = {
    traversal.flatMap(contentOnSingle)
  }
}

object TypeDeclTraversal {
  private val maxAliasExpansions = 100

  private def contentOnSingle(typeDecl: TypeDecl): Option[String] = {
    for {
      content <- typeDecl.file.content.headOption
      if content != File.PropertyDefaults.Content
      offset    <- typeDecl.offset
      offsetEnd <- typeDecl.offsetEnd
    } yield content.slice(offset, offsetEnd)
  }
}
