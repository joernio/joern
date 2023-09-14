package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import io.shiftleft.codepropertygraph.generated.help.Doc

@Traversal(elementType = classOf[Local])
class ModuleVariableTraversal(traversal: Iterator[OpNodes.ModuleVariable]) extends AnyVal {

  @Doc(info = "All assignments where the module variables in this traversal are the target across the program")
  def definitions: Iterator[Assignment] = traversal.references.flatMap {
    case x: Identifier      => x.start.inAssignment.filter(_.target == x)
    case x: FieldIdentifier => x.inAssignment.filter(_.target.contains(x.inFieldAccess))
  }

  @Doc(info = "Calls this module variable invokes across the program")
  def invokingCalls: Iterator[Call] =
    traversal.references
      .flatMap {
        case x: Identifier      => x :: Nil
        case x: FieldIdentifier => x.fieldAccess
      }
      .argumentIndexLte(1)
      .inCall
      .dedup
      .iterator

  @Doc(info =
    "References of this module variable across the codebase, as either identifiers or field identifiers, depending on" +
      " how the variable was imported"
  )
  def references: Iterator[Identifier | FieldIdentifier] = {
    val variables = traversal.toList
    variables.headOption.map(node => Cpg(node.graph)) match
      case Some(cpg) =>
        val modules       = cpg.method.isModule.l
        val variableNames = variables.name.toSet
        val immediateRef  = variables.referencingIdentifiers
        val externalRefs = modules.call
          .where(_.referencedImports)
          .flatMap { call =>
            val module = call.method
            call.referencedImports
              .flatMap(extractAliasNodePair)
              .filter {
                case (_, node: Member)   => variableNames.contains(node.name)
                case (_, node: TypeDecl) => node.member.name.exists(variableNames.contains)
                case _                   => false
              }
              .flatMap { case (alias, _) =>
                module.local.nameExact(alias).referencingIdentifiers ++
                  module.fieldAccess.fieldIdentifier.canonicalNameExact(alias)
              }
          }
        (immediateRef ++ externalRefs).collectAll[Identifier | FieldIdentifier]
      case None => Iterator.empty
  }

  private def extractAliasNodePair(i: Import): Seq[(String, AstNode)] = {
    i.importedAs
      .map { alias =>
        i.call.tag.resolvedEntity
          .flatMap {
            case m: Member   => (alias -> m) :: Nil
            case t: TypeDecl => (alias -> t) :: Nil
            case m: Method   => (alias -> m) :: Nil
            case _           => Nil
          }
          .distinct
          .toSeq
      }
      .getOrElse(Seq.empty)
  }

  @Doc(info = "The referencing member nodes of these module variables.")
  def referencingMembers: Iterator[Member] = {
    val variables          = traversal.toList
    lazy val moduleNames   = variables.method.isModule.fullName.dedup.toSeq
    lazy val variableNames = variables.name.toSeq
    variables.headOption.map(node => Cpg(node.graph)) match
      case Some(cpg) => cpg.typeDecl.fullNameExact(moduleNames*).member.nameExact(variableNames*)
      case None      => Iterator.empty
  }

}
