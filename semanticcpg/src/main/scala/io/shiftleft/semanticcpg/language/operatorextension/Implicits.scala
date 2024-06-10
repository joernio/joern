package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Expression}
import io.shiftleft.semanticcpg.language.operatorextension.nodemethods._

trait Implicits {
  implicit def toNodeTypeStartersOperatorExtension(cpg: Cpg): NodeTypeStarters = new NodeTypeStarters(cpg)

  implicit def toArrayAccessExt(arrayAccess: OpNodes.ArrayAccess): ArrayAccessMethods =
    new ArrayAccessMethods(arrayAccess)
  implicit def toArrayAccessTrav(steps: Iterator[OpNodes.ArrayAccess]): ArrayAccessTraversal =
    new ArrayAccessTraversal(steps)

  implicit def toFieldAccessExt(fieldAccess: OpNodes.FieldAccess): FieldAccessMethods =
    new FieldAccessMethods(fieldAccess)
  implicit def toFieldAccessTrav(steps: Iterator[OpNodes.FieldAccess]): FieldAccessTraversal =
    new FieldAccessTraversal(steps)

  implicit def toAssignmentExt(assignment: OpNodes.Assignment): AssignmentMethods = new AssignmentMethods(assignment)
  implicit def toAssignmentTrav(steps: Iterator[OpNodes.Assignment]): AssignmentTraversal =
    new AssignmentTraversal(steps)

  implicit def toTargetExt(call: Expression): TargetMethods               = new TargetMethods(call)
  implicit def toTargetTrav(steps: Iterator[Expression]): TargetTraversal = new TargetTraversal(steps)

  implicit def toOpAstNodeExt[A <: AstNode](node: A): OpAstNodeMethods[A]               = new OpAstNodeMethods(node)
  implicit def toOpAstNodeTrav[A <: AstNode](steps: Iterator[A]): OpAstNodeTraversal[A] = new OpAstNodeTraversal(steps)

}
