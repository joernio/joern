import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension._
import io.shiftleft.dataflowengineoss.language._
import overflowdb.traversal._

private def callOutsAreConst(method: Method): Boolean = {
  method.start.callOut.calledMethod.internal.l.forall(_.signature.contains("const"))
}

private def parameterOpsAreConst(method: Method): Boolean = {
  method.start.parameter.inAssignment.source.l.forall {
    case c: Call => c.signature.contains("const")
    case _ => false
  } && method.start.assignments.target.reachableBy(method.start.parameter).isEmpty
}

@main def main(): Set[Method] = {
  cpg.method.internal.filter { method =>
    !method.signature.contains("const") &&
      callOutsAreConst(method) &&
      parameterOpsAreConst(method)
  }.toSet
}
