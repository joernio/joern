package io.joern.c2cpg.astcreation

import io.joern.x2cpg.datastructures.Scope
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

object C2CpgScope {
  private type NewVariableNode = NewLocal | NewMethodParameterIn

  sealed trait ScopeVariable {
    def node: NewVariableNode
    def typeFullName: String
    def name: String
  }

  private final case class ScopeLocal(override val node: NewLocal) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name: String         = node.name
  }

  private final case class ScopeParameter(override val node: NewMethodParameterIn) extends ScopeVariable {
    val typeFullName: String = node.typeFullName
    val name: String         = node.name
  }

}

class C2CpgScope extends Scope[String, (NewNode, String), NewNode] {

  import C2CpgScope.*

  def variablesInScope: List[ScopeVariable] = {
    stack.reverse.flatMap(_.variables.values.map(_._1)).collect {
      case local: NewLocal                 => ScopeLocal(local)
      case parameter: NewMethodParameterIn => ScopeParameter(parameter)
    }
  }

}
