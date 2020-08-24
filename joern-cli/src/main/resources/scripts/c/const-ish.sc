import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Member, Method}
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

@main def main(): Set[Method] = {
  cpg.method
    .internal
    .whereNot { method =>
      method
        .assignments
        .target
        .reachableBy(method.parameter.filter(_.typeFullName.contains("const")))
  }.toSet
}
