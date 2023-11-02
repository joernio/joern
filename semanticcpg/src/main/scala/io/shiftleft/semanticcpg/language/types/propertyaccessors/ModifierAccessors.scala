package io.shiftleft.semanticcpg.language.types.propertyaccessors

import io.shiftleft.codepropertygraph.generated.v2.ModifierTypes
import io.shiftleft.codepropertygraph.generated.v2.nodes.{Modifier, StoredNode}
import io.shiftleft.codepropertygraph.generated.v2.Language.*
import io.shiftleft.semanticcpg.language.*

class ModifierAccessors[A <: StoredNode](val traversal: Iterator[A]) extends AnyVal {

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

  def hasModifier(modifier: String): Iterator[A] =
    traversal.where(_.out.collectAll[Modifier].modifierType(modifier))

  /** Traverse to modifiers, e.g., "static", "public".
    */
  def modifier: Iterator[Modifier] =
    traversal.out.collectAll[Modifier]
}
