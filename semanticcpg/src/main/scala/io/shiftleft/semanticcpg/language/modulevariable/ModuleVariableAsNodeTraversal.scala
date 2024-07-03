package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.modulevariable.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess
import io.shiftleft.codepropertygraph.generated.help.Doc

@Traversal(elementType = classOf[Local])
class ModuleVariableAsLocalTraversal(traversal: Iterator[Local]) extends AnyVal {

  @Doc(info = "Locals representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    traversal.filter(_.isModuleVariable).cast[OpNodes.ModuleVariable]
  }

}

@Traversal(elementType = classOf[Identifier])
class ModuleVariableAsIdentifierTraversal(traversal: Iterator[Identifier]) extends AnyVal {

  @Doc(info = "Identifiers representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    traversal.flatMap(_._localViaRefOut).moduleVariables
  }

}

@Traversal(elementType = classOf[FieldIdentifier])
class ModuleVariableAsFieldIdentifierTraversal(traversal: Iterator[FieldIdentifier]) extends AnyVal {

  @Doc(info = "Field identifiers representing module variables")
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

@Traversal(elementType = classOf[Member])
class ModuleVariableAsMemberTraversal(traversal: Iterator[Member]) extends AnyVal {

  @Doc(info = "Members representing module variables")
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

@Traversal(elementType = classOf[Expression])
class ModuleVariableAsExpressionTraversal(traversal: Iterator[Expression]) extends AnyVal {

  @Doc(info = "Expression nodes representing module variables")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] = {
    traversal.flatMap {
      case x: Identifier                              => x.start.moduleVariables
      case x: FieldIdentifier                         => x.start.moduleVariables
      case x: Call if x.name == Operators.fieldAccess => x.asInstanceOf[FieldAccess].fieldIdentifier.moduleVariables
      case _                                          => Iterator.empty
    }
  }

}
