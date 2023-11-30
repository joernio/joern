package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.v2.nodes.*
import io.shiftleft.codepropertygraph.generated.v2.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.modulevariable.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
// TODO bring back help/doc
//import overflowdb.traversal.help.Doc

class ModuleVariableAsLocalTraversal(traversal: Iterator[Local]) extends AnyVal {

  // TODO bring back help/doc
  // @Doc(info = "Locals representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    traversal.filter(_.isModuleVariable).cast[OpNodes.ModuleVariable]
  }

}

class ModuleVariableAsIdentifierTraversal(traversal: Iterator[Identifier]) extends AnyVal {

  // TODO bring back help/doc
  // @Doc(info = "Identifiers representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    traversal.flatMap(_.localViaRefOut).moduleVariables
  }

}

class ModuleVariableAsFieldIdentifierTraversal(traversal: Iterator[FieldIdentifier]) extends AnyVal {

  // TODO bring back help/doc
  // @Doc(info = "Field identifiers representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    traversal.flatMap { fieldIdentifier =>
      Cpg(fieldIdentifier.graph).method
        .fullNameExact(fieldIdentifier.inFieldAccess.argument(1).isIdentifier.typeFullName.toSeq*)
        .isModule
        .local
        .nameExact(fieldIdentifier.canonicalName)
    }.moduleVariables
  }

}

class ModuleVariableAsMemberTraversal(traversal: Iterator[Member]) extends AnyVal {

  // TODO bring back help/doc
  // @Doc(info = "Members representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    val members          = traversal.toList
    lazy val memberNames = members.name.toSeq
    members.headOption.map(m => Cpg(m.graph)) match
      case Some(cpg) =>
        cpg.method
          .fullNameExact(members.typeDecl.fullName.toSeq*)
          .isModule
          .flatMap(_.local.nameExact(memberNames*).moduleVariables)
      case None => Iterator.empty
  }

}

class ModuleVariableAsExpressionTraversal(traversal: Iterator[Expression]) extends AnyVal {

  // TODO bring back help/doc
//  @Doc(info = "Expression nodes representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    traversal.flatMap {
      case x: Identifier                              => x.start.moduleVariables
      case x: FieldIdentifier                         => x.start.moduleVariables
      case x: Call if x.name == Operators.fieldAccess => x.asInstanceOf[FieldAccess].fieldIdentifier.moduleVariables
      case _                                          => Iterator.empty
    }
  }

}
