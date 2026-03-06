package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

object SwiftTypeNodePass {

  def withRegisteredTypes(registeredTypes: List[String], cpg: Cpg): TypeNodePass = {
    new TypeNodePass(registeredTypes, cpg, getTypesFromCpg = false) {

      override def fullToShortName(typeName: String): String = {
        typeName match {
          case name if name.endsWith(NamespaceTraversal.globalNamespaceName) => NamespaceTraversal.globalNamespaceName
          case s"${Defines.Function}<$_>"                                    => "Function"
          case name if name.contains(s"${io.joern.x2cpg.Defines.ClosurePrefix}") =>
            val index  = name.lastIndexOf(io.joern.x2cpg.Defines.ClosurePrefix)
            val offset = index + io.joern.x2cpg.Defines.ClosurePrefix.length
            s"${io.joern.x2cpg.Defines.ClosurePrefix}${name.substring(offset).takeWhile(_.isDigit)}"
          case _ =>
            typeName.split('.').lastOption.map(_.takeWhile(_ != ':')).getOrElse(typeName)
        }
      }

    }
  }

}
