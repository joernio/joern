import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.dataflowengine.language._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.opnodes.Assignment

private def mallocCalls(cpg: Cpg): Steps[Assignment] = {
  cpg.assignment.where { assignment =>
    assignment.argument(2) match {
      case call: Call => call.name == "malloc"
      case _ => false
    }
  }
}

private def freeCalls(cpg: Cpg): Steps[Call] = cpg.call("free")

@main def main(): Set[Assignment] = {
  val freedCalls = freeCalls(cpg).reachableBy(mallocCalls(cpg)).toSet
  mallocCalls(cpg).toSet.diff(freedCalls)
}
