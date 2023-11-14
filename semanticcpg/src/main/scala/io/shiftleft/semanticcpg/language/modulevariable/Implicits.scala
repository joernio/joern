package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.semanticcpg.language.modulevariable.NodeTypeStarters
import io.shiftleft.semanticcpg.language.modulevariable.nodemethods.{
  ModuleVariableAsMemberMethods,
  ModuleVariableReferenceMethods
}

import scala.language.implicitConversions

trait Implicits {

  implicit def toNodeTypeStartersModuleVariableExtension(cpg: Cpg): NodeTypeStarters = new NodeTypeStarters(cpg)

  implicit def toModuleVariableAsMemberExt(node: Member): ModuleVariableAsMemberMethods =
    new ModuleVariableAsMemberMethods(node)

  implicit def toModuleVariableAsMemberTrav(steps: Iterator[Member]): ModuleVariableAsMemberTraversal =
    new ModuleVariableAsMemberTraversal(steps)

  implicit def toModuleVariablesExt(node: OpNodes.ModuleVariableReference): ModuleVariableReferenceMethods =
    new ModuleVariableReferenceMethods(node)

  implicit def toModuleVariablesTrav(steps: IterableOnce[OpNodes.ModuleVariable]): ModuleVariableTraversal =
    new ModuleVariableTraversal(steps.iterator)

  implicit def toModuleReferenceTrav(steps: IterableOnce[OpNodes.ModuleVariableReference]): ModuleReferenceTraversal =
    new ModuleReferenceTraversal(steps.iterator)

}
