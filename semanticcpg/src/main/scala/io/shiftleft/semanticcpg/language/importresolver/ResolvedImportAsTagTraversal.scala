package io.shiftleft.semanticcpg.language.importresolver

import io.shiftleft.codepropertygraph.generated.v2.Cpg
import io.shiftleft.codepropertygraph.generated.v2.nodes.{AstNode, Declaration, Member, Tag}
import io.shiftleft.semanticcpg.language.*
// TODO bring back help/doc
//import overflowdb.traversal.help.Doc

class ResolvedImportAsTagExt(node: Tag) extends AnyVal {

  // TODO bring back help/doc
//  @Doc(info = "Parses this tag as an EvaluatedImport class")
  def _toEvaluatedImport: Option[EvaluatedImport] = EvaluatedImport.tagToEvaluatedImport(node)

  // TODO bring back help/doc
//  @Doc(info = "If this tag represents a resolved import, will attempt to find the CPG entities this refers to")
  def resolvedEntity: Iterator[AstNode] = {
    val cpg = Cpg(node.graph)
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

  // TODO bring back help/doc
//  @Doc(info = "Parses these tags as EvaluatedImport classes")
  def _toEvaluatedImport: Iterator[EvaluatedImport] = {
    steps.flatMap(_._toEvaluatedImport)
  }

  // TODO bring back help/doc
//  @Doc(info = "If these tags represent resolved imports, will attempt to find the CPG entities referred to")
  def resolvedEntity: Iterator[AstNode] = {
    steps.flatMap(_.resolvedEntity)
  }

}
