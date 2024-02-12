package io.joern.rubysrc2cpg.datastructures

import io.joern.x2cpg.datastructures.*
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

class RubyScope(summary: RubyProgramSummary)
    extends Scope[String, NewNode, TypedScopeElement]
    with TypedScope[RubyMethod, RubyField, RubyType](summary) {

  /** @return
    *   the full name of the surrounding scope.
    */
  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(x: NamespaceLikeScope, _) => x.fullName
    case ScopeElement(x: TypeLikeScope, _)      => x.fullName
    case ScopeElement(MethodScope(fullName), _) => fullName
  }

  /** @return
    *   the corresponding node label according to the scope element.
    */
  def surroundingAstLabel: Option[String] = stack.collectFirst {
    case ScopeElement(_: NamespaceLikeScope, _) => NodeTypes.NAMESPACE_BLOCK
    case ScopeElement(_: TypeLikeScope, _)      => NodeTypes.TYPE_DECL
    case ScopeElement(MethodScope(_), _)        => NodeTypes.METHOD
    case ScopeElement(BlockScope, _)            => NodeTypes.BLOCK
  }

}
