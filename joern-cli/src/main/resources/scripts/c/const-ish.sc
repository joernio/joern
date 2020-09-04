import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Member, Method}
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

@main def main(): Set[Method] = {
  (cpg: Cpg).method
    .internal
    .whereNonEmpty { method =>

    method.start
      .assignments
      .target
      .reachableBy(method.parameter.where(_.typeFullName.contains("const")))
  }.toSet
}
