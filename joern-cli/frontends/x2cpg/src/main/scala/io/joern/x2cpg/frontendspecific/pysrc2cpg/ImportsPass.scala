package io.joern.x2cpg.frontendspecific.pysrc2cpg

import io.joern.x2cpg.passes.frontend.XImportsPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment

class ImportsPass(cpg: Cpg) extends XImportsPass(cpg) {

  override protected val importCallName: String = "import"

  override protected def importCallToPart(x: Call): Iterator[(Call, Assignment)] = x.inAssignment.map(y => (x, y))

  override def importedEntityFromCall(call: Call): String = {
    call.argument.code.l match {
      case List("", what)    => what.split('.')(0)
      case List(where, what) => s"${resolve(where, what, call)}"

      case List("", what, _)    => what
      case List(where, what, _) => s"${resolve(where, what, call)}"
      case _                    => ""
    }
  }

  private def resolve(where: String, what: String, call: Call): String = {
    where match {
      case "." =>
        call.file.name.headOption
          .map { path =>
            path.split(".").dropRight(1) match {
              case Array() =>
                what
              case packagePathElems =>
                packagePathElems.mkString(".") + s".$what"
            }
          }
          .getOrElse("")
      case x => x + s".$what"
    }
  }

}
