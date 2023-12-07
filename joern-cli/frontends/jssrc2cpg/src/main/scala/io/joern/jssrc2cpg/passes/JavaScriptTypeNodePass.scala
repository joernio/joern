package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.passes.KeyPool

import scala.collection.mutable

object JavaScriptTypeNodePass {

  def withRegisteredTypes(registeredTypes: List[String], cpg: Cpg, keyPool: Option[KeyPool] = None): TypeNodePass = {
    new TypeNodePass(registeredTypes, cpg, keyPool, getTypesFromCpg = false) {

      override def fullToShortName(typeName: String): String = {
        typeName match {
          case name if name.endsWith(":program") => ":program"
          case name if name.contains("=>")       => name
          case name if name.contains(":")        => name.split(':').lastOption.getOrElse(typeName)
          case _                                 => typeName.split('.').lastOption.getOrElse(typeName)
        }
      }

      override protected def typeDeclTypes: mutable.Set[String] = {
        // The only difference to the default implementation in TypeNodePass.typeDeclTypes is the following:
        // We do not want to add types for types being inherited as this is already handled by the JavaScriptInheritanceNamePass.
        cpg.typeDecl.fullName.toSetMutable
      }

    }
  }

}
