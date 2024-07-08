package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.nodemethods.*

trait NodeExtensionFinder {
  def apply(n: StoredNode): Option[NodeExtension]
}

object DefaultNodeExtensionFinder extends NodeExtensionFinder {
  override def apply(node: StoredNode): Option[NodeExtension] = {
    node match {
      case n: Method             => Some(new MethodMethods(n))
      case n: MethodParameterIn  => Some(new MethodParameterInMethods(n))
      case n: MethodParameterOut => Some(new MethodParameterOutMethods(n))
      case n: MethodReturn       => Some(new MethodReturnMethods(n))
      case n: Call               => Some(new CallMethods(n))
      case n: Identifier         => Some(new IdentifierMethods(n))
      case n: Literal            => Some(new LiteralMethods(n))
      case n: Local              => Some(new LocalMethods(n))
      case n: MethodRef          => Some(new MethodRefMethods(n))
      case _                     => None
    }
  }
}
