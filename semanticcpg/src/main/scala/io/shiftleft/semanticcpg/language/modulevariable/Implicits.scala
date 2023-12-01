package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.modulevariable.nodemethods.*

import scala.language.implicitConversions

trait Implicits {

  implicit def toNodeTypeStartersModuleVariableExtension(cpg: Cpg): NodeTypeStarters = new NodeTypeStarters(cpg)

  /* Extensions on existing CPG nodes */

  implicit def toModuleVariableAsLocalExt(node: Local): ModuleVariableAsLocalMethods =
    new ModuleVariableAsLocalMethods(node)

  implicit def toModuleVariableAsLocalTrav(steps: IterableOnce[Local]): ModuleVariableAsLocalTraversal =
    new ModuleVariableAsLocalTraversal(steps.iterator)

  implicit def toModuleVariableAsIdentifierTrav(steps: IterableOnce[Identifier]): ModuleVariableAsIdentifierTraversal =
    new ModuleVariableAsIdentifierTraversal(steps.iterator)

  implicit def toModuleVariableAsMemberTrav(steps: IterableOnce[Member]): ModuleVariableAsMemberTraversal =
    new ModuleVariableAsMemberTraversal(steps.iterator)

  implicit def toModuleVariableAsExpressionTrav(steps: IterableOnce[Expression]): ModuleVariableAsExpressionTraversal =
    new ModuleVariableAsExpressionTraversal(steps.iterator)

  implicit def toModuleVariableAsFieldIdentifierTrav(
    steps: IterableOnce[FieldIdentifier]
  ): ModuleVariableAsFieldIdentifierTraversal = new ModuleVariableAsFieldIdentifierTraversal(steps.iterator)

  /* Extensions on module variable nodes */

  implicit def toModuleVariableExt(node: OpNodes.ModuleVariable): ModuleVariableMethods =
    new ModuleVariableMethods(node)

  implicit def toModuleVariablesTrav(steps: IterableOnce[OpNodes.ModuleVariable]): ModuleVariableTraversal =
    new ModuleVariableTraversal(steps.iterator)

}
