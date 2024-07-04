package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.accesspath.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.MemberAccess
import org.slf4j.LoggerFactory

object AccessPathUsage {

  private val logger = LoggerFactory.getLogger(getClass)

  def toTrackedBaseAndAccessPathSimple(node: StoredNode): (TrackedBase, AccessPath) = {
    val (base, revPath) = toTrackedBaseAndAccessPathInternal(node)
    (base, AccessPath.apply(Elements.normalized(revPath.reverse), Nil))
  }

  private def toTrackedBaseAndAccessPathInternal(node: StoredNode): (TrackedBase, List[AccessElement]) = {
    val result = AccessPathHandling.leafToTrackedBaseAndAccessPathInternal(node)
    if (result.isDefined) {
      result.get
    } else {
      node match {

        case block: Block =>
          AccessPathHandling
            .lastExpressionInBlock(block)
            .map { toTrackedBaseAndAccessPathInternal }
            .getOrElse((TrackedUnknown, Nil))
        case call: Call if !MemberAccess.isGenericMemberAccessName(call.name) => (TrackedReturnValue(call), Nil)

        case memberAccess: Call =>
          // assume: MemberAccess.isGenericMemberAccessName(call.name)
          val argOne = memberAccess.argumentOption(1)
          if (argOne.isEmpty) {
            logger.warn(s"Missing first argument on call ${memberAccess.code}.")
            return (TrackedUnknown, Nil)
          }
          val (base, tail) = toTrackedBaseAndAccessPathInternal(argOne.get)
          val path         = AccessPathHandling.memberAccessToPath(memberAccess, tail)
          (base, path)
        case _ =>
          logger.warn(s"Missing handling for node type ${node.getClass}.")
          (TrackedUnknown, Nil)
      }
    }
  }
}
