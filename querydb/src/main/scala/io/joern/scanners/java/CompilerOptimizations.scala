package io.joern.scanners.java

import io.joern.scanners._
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Type}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.console._
import io.shiftleft.macros.QueryMacros._
import io.shiftleft.semanticcpg.language.operatorextension.opnodes.Assignment
import io.shiftleft.dataflowengineoss.queryengine.EngineContext
import overflowdb.traversal.Traversal

object CompilerOptimizations extends QueryBundle {

  implicit val resolver: ICallResolver = NoResolve

  @q
  def simpleConstant()(implicit context: EngineContext): Query =
    Query.make(
      name = "simple-constant-detection",
      author = Crew.dave,
      title =
        "Simple Constant Detection: Finds identifiers with primitives only assigned once",
      description =
        """
          |Detect variables holding simple constants. A term is a simple constant
          |if it assigns a primitive constant, or if all its operands are simple constants.
          |
          |This should be optimized by the compiler during compile time.
          |""".stripMargin,
      score = 1,
      withStrRep({ cpg =>
        cpg.assignment
          .groupBy(_.argument.order(1).code.l)
          .flatMap {
            case (_: List[String], as: Traversal[Assignment]) => Option(as.l)
            case _                                            => Option.empty
          }
          .filter(_.size == 1)
          .flatMap {
            case as: List[Assignment] =>
              Option(as.head.argument.head, as.head.argument.l.head.typ.l)
            case _ => Option.empty
          }
          .filter {
            case (_: Identifier, ts: List[Type]) =>
              ts.nonEmpty &&
                ts.head.namespace.l.exists { x =>
                  x.name.contains("<global>")
                } &&
                !ts.head.fullName.contains("[]")
            case _ => false
          }
          .flatMap {
            case (i: Identifier, _: List[Type]) => Option(i)
            case _                              => Option.empty
          }
      }),
      tags = List(QueryTags.compilerOptimization)
    )

}
