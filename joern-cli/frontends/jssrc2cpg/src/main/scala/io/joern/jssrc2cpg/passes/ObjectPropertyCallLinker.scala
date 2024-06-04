package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.nodes.{Call, MethodRef}
import io.shiftleft.codepropertygraph.generated.{Cpg, PropertyNames}
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language.*

/** Perform a simple analysis to find a common pattern in JavaScript where objects are dynamically assigned function
  * pointers. To keep this precise, this will only match objects defined within the scope of the same file.
  *
  * This relies on JavaScriptTypeHintCallLinker.
  */
class ObjectPropertyCallLinker(cpg: Cpg) extends CpgPass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    def propertyCallRegexPattern(withMatchingGroup: Boolean): String =
      "^(?:\\{.*\\}|.*<returnValue>):<member>\\(" + (if withMatchingGroup then "(.*)" else ".*") + "\\):.*$"

    val propertyCallRegex = propertyCallRegexPattern(true).r
    val objectCalls       = cpg.call.methodFullName(propertyCallRegexPattern(false)).l
    val propertyAccessToCalls = objectCalls
      .flatMap { call =>
        call.methodFullName match {
          case propertyCallRegex(baseProperty) => Option(s"$baseProperty.${call.name}" -> call)
          case _                               => None
        }
      }
      .groupBy(_._1)
      .map { case (k, vs) => k -> vs.map(_._2) }
    cpg.assignment
      .and(_.source.isMethodRef, _.target.isCall.fieldAccess)
      .map { a => a.target.asInstanceOf[Call] -> a.source.asInstanceOf[MethodRef].referencedMethod.fullName }
      .foreach { (functionTarget, calleeFn) =>
        propertyAccessToCalls
          .filter { case (propertyAccess, _) => functionTarget.code.endsWith(propertyAccess) }
          .foreach { case (_, calls) =>
            calls.where(_.file.nameExact(functionTarget.file.name.toSeq*)).foreach { c =>
              builder.setNodeProperty(c, PropertyNames.METHOD_FULL_NAME, calleeFn)
            }
          }
      }
  }

}
