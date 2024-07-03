package io.shiftleft.semanticcpg.language.importresolver

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Declaration, Member, Tag}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

class ResolvedImportAsTagExt(node: Tag) extends AnyVal {

  @Doc(info = "Parses this tag as an EvaluatedImport class")
  def _toEvaluatedImport: Option[EvaluatedImport] = EvaluatedImport.tagToEvaluatedImport(node)

  @Doc(info = "If this tag represents a resolved import, will attempt to find the CPG entities this refers to")
  def resolvedEntity: Iterator[AstNode] = {
    val cpg = Cpg(node.graph())
    node._toEvaluatedImport.iterator
      .collectAll[ResolvedImport]
      .flatMap {
        case ResolvedMethod(fullName, _, _, _) =>
          cpg.method.fullNameExact(fullName)
        case ResolvedTypeDecl(fullName, _) =>
          cpg.typeDecl.fullNameExact(fullName)
        case ResolvedMember(basePath, memberName, _) =>
          cpg.typeDecl.fullNameExact(basePath).member.nameExact(memberName)
      }
      .iterator
  }

}

class ResolvedImportAsTagTraversal(steps: Iterator[Tag]) extends AnyVal {

  @Doc(info = "Parses these tags as EvaluatedImport classes")
  def _toEvaluatedImport: Iterator[EvaluatedImport] = {
    steps.flatMap(_._toEvaluatedImport)
  }

  @Doc(info = "If these tags represent resolved imports, will attempt to find the CPG entities referred to")
  def resolvedEntity: Iterator[AstNode] = {
    steps.flatMap(_.resolvedEntity)
  }

}
