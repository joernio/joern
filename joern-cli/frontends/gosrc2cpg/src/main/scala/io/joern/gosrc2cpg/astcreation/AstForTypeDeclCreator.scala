package io.joern.gosrc2cpg.astcreation
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.utils.NodeBuilders.newOperatorCallNode
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewFieldIdentifier
import ujson.Value

import scala.util.{Success, Try}

trait AstForTypeDeclCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForTypeSpec(typeSpecNode: ParserNodeInfo): Seq[Ast] = {
    val (name, fullName, memberAsts) = processTypeSepc(typeSpecNode.json)
    val typeDeclNode_ =
      typeDeclNode(typeSpecNode, name, fullName, relPathFileName, typeSpecNode.code)
    val modifier = addModifier(typeDeclNode_, name)
    Seq(Ast(typeDeclNode_).withChild(Ast(modifier)).withChildren(memberAsts))
  }

  protected def astForStructType(expr: ParserNodeInfo, typeDeclFullName: String): Seq[Ast] = {
    Try(expr.json(ParserKeys.Fields)) match
      case Success(fields) if fields != null =>
        fields(ParserKeys.List).arrOpt
          .getOrElse(List())
          .flatMap(x => {
            val typeInfo                = createParserNodeInfo(x(ParserKeys.Type))
            val (typeFullName, _, _, _) = processTypeInfo(typeInfo)
            x(ParserKeys.Names).arrOpt
              .getOrElse(List())
              .map(fieldInfo => {
                val fieldNodeInfo = createParserNodeInfo(fieldInfo)
                val fieldName     = fieldNodeInfo.json(ParserKeys.Name).str
                GoGlobal.recordStructTypeMemberType(typeDeclFullName + Defines.dot + fieldName, typeFullName)
                Ast(memberNode(typeInfo, fieldName, fieldNodeInfo.code, typeFullName, Seq()))
              })
          })
          .toSeq
      case _ => Seq.empty
  }

  protected def astForFieldAccess(info: ParserNodeInfo): Seq[Ast] = {
    val identifierAsts       = astForNode(info.json(ParserKeys.X))
    val receiverTypeFullName = getTypeFullNameFromAstNode(identifierAsts)
    val fieldIdentifier      = info.json(ParserKeys.Sel)(ParserKeys.Name).str
    val fieldTypeFullName = GoGlobal.structTypeMemberTypeMapping.getOrDefault(
      receiverTypeFullName + Defines.dot + fieldIdentifier,
      XDefines.Unknown
    )
    val fieldIdentifierNode = NewFieldIdentifier()
      .canonicalName(fieldIdentifier)
      .lineNumber(line(info))
      .columnNumber(column(info))
      .code(fieldIdentifier)
    val fieldIdAst = Ast(fieldIdentifierNode)
    val callNode =
      newOperatorCallNode(Operators.fieldAccess, info.code, Some(fieldTypeFullName), line(info), column(info))
    Seq(callAst(callNode, identifierAsts ++ Seq(fieldIdAst)))
  }
}
