package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*

object Dereference {

  def apply(cpg: Cpg): Dereference = cpg.metaData.language.headOption match {
    case Some(Languages.NEWC) => CDereference()
    case _                    => DefaultDereference()
  }

}

sealed trait Dereference {

  def dereferenceTypeFullName(fullName: String): String

}

case class CDereference() extends Dereference {

  /** Types from C/C++ can be annotated with * to indicate being a reference. As our CPG schema currently lacks a
    * separate field for that information the * is part of the type full name and needs to be removed when linking.
    */
  override def dereferenceTypeFullName(fullName: String): String = fullName.replace("*", "")

}

case class DefaultDereference() extends Dereference {

  override def dereferenceTypeFullName(fullName: String): String = fullName

}
