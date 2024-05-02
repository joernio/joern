package io.shiftleft.semanticcpg.language.types.propertyaccessors

import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Modifier}
import io.shiftleft.codepropertygraph.generated.traversal.toModifierTraversalExtGen
import io.shiftleft.semanticcpg.language.*
import overflowdb.*

class ModifierAccessors[A <: AstNode](val traversal: Iterator[A]) extends AnyVal {

  /** Filter: only `public` nodes */
  def isPublic: Iterator[A] =
    hasModifier(ModifierTypes.PUBLIC)

  /** Filter: only `private` nodes */
  def isPrivate: Iterator[A] =
    hasModifier(ModifierTypes.PRIVATE)

  /** Filter: only `protected` nodes */
  def isProtected: Iterator[A] =
    hasModifier(ModifierTypes.PROTECTED)

  /** Filter: only `abstract` nodes */
  def isAbstract: Iterator[A] =
    hasModifier(ModifierTypes.ABSTRACT)

  /** Filter: only `static` nodes */
  def isStatic: Iterator[A] =
    hasModifier(ModifierTypes.STATIC)

  /** Filter: only `native` nodes */
  def isNative: Iterator[A] =
    hasModifier(ModifierTypes.NATIVE)

  /** Filter: only `constructor` nodes */
  def isConstructor: Iterator[A] =
    hasModifier(ModifierTypes.CONSTRUCTOR)

  /** Filter: only `virtual` nodes */
  def isVirtual: Iterator[A] =
    hasModifier(ModifierTypes.VIRTUAL)

  /** Filter: only `module` nodes */
  def isModule: Iterator[A] =
    hasModifier(ModifierTypes.MODULE)

  /** Filter: only `lambda` methods */
  def isLambda: Iterator[A] =
    hasModifier(ModifierTypes.LAMBDA)

  def hasModifier(modifier: String): Iterator[A] =
    traversal.where(_._astOut.collectAll[Modifier].modifierTypeExact(modifier))

  /** Traverse to modifiers, e.g., "static", "public".
    */
  def modifier: Iterator[Modifier] =
    traversal._astOut.collectAll[Modifier]
}
