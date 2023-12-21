package io.joern.gosrc2cpg.astcreation
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.utils.NodeBuilders.{newFieldIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.Operators
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

  protected def processFuncType(typeNode: ParserNodeInfo, typeDeclFullName: String): Seq[Ast] = {
    val (signature, returnTypeFullName, _, _, _) = generateLambdaSignature(typeNode)
    goGlobal.recordLambdaSigntureToLambdaType(signature, typeDeclFullName, returnTypeFullName)
    Seq.empty
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
                goGlobal.recordStructTypeMemberType(typeDeclFullName + Defines.dot + fieldName, typeFullName)
                Ast(memberNode(typeInfo, fieldName, fieldNodeInfo.code, typeFullName))
              })
          })
          .toSeq
      case _ => Seq.empty
  }

  private def processReceiver(info: ParserNodeInfo): (Seq[Ast], String) = {
    val xnode           = createParserNodeInfo(info.json(ParserKeys.X))
    val fieldIdentifier = info.json(ParserKeys.Sel)(ParserKeys.Name).str
    xnode.node match
      case Ident =>
        Try(xnode.json(ParserKeys.Obj)) match
          case Success(_) =>
            // The presence of "Obj" field indicates its variable identifier and not an alias
            receiverAstAndFullName(xnode, fieldIdentifier)
          case _ =>
            // Otherwise its an alias to imported namespace using which global variable is getting accessed
            val alias            = xnode.json(ParserKeys.Name).str
            val receiverFullName = resolveAliasToFullName(alias, fieldIdentifier)
            (
              astForNode(xnode),
              goGlobal.structTypeMemberTypeMapping.getOrDefault(
                receiverFullName,
                s"$receiverFullName${Defines.dot}${Defines.FieldAccess}${Defines.dot}${XDefines.Unknown}"
              )
            )
      case _ =>
        // This will take care of chained calls
        receiverAstAndFullName(xnode, fieldIdentifier)
  }

  private def receiverAstAndFullName(xnode: ParserNodeInfo, fieldIdentifier: String): (Seq[Ast], String) = {
    val identifierAsts       = astForNode(xnode)
    val receiverTypeFullName = getTypeFullNameFromAstNode(identifierAsts)
    val fieldTypeFullName = goGlobal.structTypeMemberTypeMapping.getOrDefault(
      s"$receiverTypeFullName${Defines.dot}$fieldIdentifier",
      s"$receiverTypeFullName${Defines.dot}$fieldIdentifier${Defines.dot}${Defines.FieldAccess}${Defines.dot}${XDefines.Unknown}"
    )
    (identifierAsts, fieldTypeFullName)
  }

  protected def astForFieldAccess(info: ParserNodeInfo): Seq[Ast] = {
    val (identifierAsts, fieldTypeFullName) = processReceiver(info)
    val fieldIdentifier                     = info.json(ParserKeys.Sel)(ParserKeys.Name).str
    val callNode =
      newOperatorCallNode(Operators.fieldAccess, info.code, Some(fieldTypeFullName), line(info), column(info))
    Seq(
      callAst(callNode, identifierAsts ++ Seq(Ast(newFieldIdentifierNode(fieldIdentifier, line(info), column(info)))))
    )
  }
}
