package io.joern.x2cpg.passes.callgraph

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

implicit val resolver: ICallResolver = NoResolve
class StaticCallLinker(cpg: Cpg) extends ForkJoinParallelCpgPass[Seq[Method]](cpg) with LinkingUtil {

  private val logger: Logger = LoggerFactory.getLogger(classOf[StaticCallLinker])

  override def generateParts(): Array[Seq[Method]] = {
    val allMethods = cpg.method.l
    allMethods.grouped(getBatchSize(allMethods.size)).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, methods: Seq[Method]): Unit = {
    methods.foreach { method =>
      try {
        cpg.call
          .methodFullNameExact(method.fullName)
          .foreach(call => {
            call.dispatchType match {
              case DispatchTypes.STATIC_DISPATCH | DispatchTypes.INLINED =>
                builder.addEdge(call, method, EdgeTypes.CALL)
              case DispatchTypes.DYNAMIC_DISPATCH =>
              // Do nothing
              case _ => logger.warn(s"Unknown dispatch type on dynamic CALL ${call.code}")
            }
          })
      } catch {
        case exception: Exception =>
          logger.error(s"Exception in StaticCallLinker: ", exception)
      }
    }
  }

  override def finish(): Unit = {
    super.finish()
    val invalidCallRelations = cpg.call.count(x =>
      (x.dispatchType == DispatchTypes.STATIC_DISPATCH || x.dispatchType == DispatchTypes.INLINED) && x.callee.size > 1
    )
    if (invalidCallRelations > 0) {
      logger.warn(s"Invalid CPG structure detected, as $invalidCallRelations static calls have multiple callees")
    }
  }
}
