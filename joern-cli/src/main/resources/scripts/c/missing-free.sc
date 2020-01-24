import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, Identifier}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.dataflowengine.language._

@main def main(): List[Identifier] = {
  (cpg: Cpg).assignment.where { assignment =>
    assignment.argument(2) match {
      case call: Call => call.name == "malloc"
      case _ => false
    }
  }.map { assignment =>
    (assignment.method, assignment.argument(1).asInstanceOf[Identifier])
  }.where { case (containingMethod, allocatedIdentifier) =>
    containingMethod.start.callOut.name("free").where { freeCall =>
      val freeArg = freeCall.argument(1).asInstanceOf[Identifier]
      freeArg.name == allocatedIdentifier.code && freeArg.code == allocatedIdentifier.code
    }.isEmpty
  }.map { case (_, identifier) => identifier }.l
}
