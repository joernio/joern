package io.shiftleft.semanticcpg.language.types.propertyaccessors

import io.shiftleft.codepropertygraph.generated.nodes.Modifier
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes, Properties}
import overflowdb._
import overflowdb.traversal._

class ModifierAccessors[A <: Node](val traversal: Traversal[A]) extends AnyVal {

  /** Filter: only `public` nodes */
  def isPublic: Traversal[A] =
    hasModifier(ModifierTypes.PUBLIC)

  /** Filter: only `private` nodes */
  def isPrivate: Traversal[A] =
    hasModifier(ModifierTypes.PRIVATE)

  /** Filter: only `protected` nodes */
  def isProtected: Traversal[A] =
    hasModifier(ModifierTypes.PROTECTED)

  /** Filter: only `abstract` nodes */
  def isAbstract: Traversal[A] =
    hasModifier(ModifierTypes.ABSTRACT)

  /** Filter: only `static` nodes */
  def isStatic: Traversal[A] =
    hasModifier(ModifierTypes.STATIC)

  /** Filter: only `native` nodes */
  def isNative: Traversal[A] =
    hasModifier(ModifierTypes.NATIVE)

  /** Filter: only `constructor` nodes */
  def isConstructor: Traversal[A] =
    hasModifier(ModifierTypes.CONSTRUCTOR)

  /** Filter: only `virtual` nodes */
  def isVirtual: Traversal[A] =
    hasModifier(ModifierTypes.VIRTUAL)

  def hasModifier(modifier: String): Traversal[A] =
    traversal.where(_.out.hasLabel(NodeTypes.MODIFIER).has(Properties.MODIFIER_TYPE -> modifier))

  /** Traverse to modifiers, e.g., "static", "public".
    */
  def modifier: Traversal[Modifier] =
    traversal.out.hasLabel(NodeTypes.MODIFIER).cast[Modifier]
}
