package io.joern.gosrc2cpg.astcreation
import io.joern.gosrc2cpg.datastructures.LambdaTypeInfo
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.Operators
import ujson.Value

import scala.util.{Success, Try}

trait AstForTypeDeclCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForTypeSpec(typeSpecNode: ParserNodeInfo): Seq[Ast] = {
    val (name, fullName, memberAsts) = processTypeSepc(createParserNodeInfo(typeSpecNode.json))
    val typeDeclNode_ =
      typeDeclNode(typeSpecNode, name, fullName, relPathFileName, typeSpecNode.code)
    val modifier = addModifier(typeDeclNode_, name)
    Seq(Ast(typeDeclNode_).withChild(Ast(modifier)).withChildren(memberAsts))
  }

  protected def processFuncType(typeNode: ParserNodeInfo, typeDeclFullName: String): Seq[Ast] = {
    val LambdaFunctionMetaData(signature, returnTypeFullName, _, _, _) = generateLambdaSignature(typeNode)
    goGlobal.recordLambdaSigntureToLambdaType(signature, LambdaTypeInfo(typeDeclFullName, returnTypeFullName))
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
                if (goGlobal.checkForDependencyFlags(fieldName)) {
                  goGlobal.recordStructTypeMemberTypeInfo(typeDeclFullName, fieldName, typeFullName)
                }
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
            val alias     = xnode.json(ParserKeys.Name).str
            val nameSpace = resolveAliasToFullName(alias)
            (
              astForNode(xnode),
              goGlobal
                .getStructTypeMemberType(nameSpace, fieldIdentifier)
                .getOrElse(
                  s"$nameSpace.$fieldIdentifier${Defines.dot}${Defines.FieldAccess}${Defines.dot}${XDefines.Unknown}"
                )
            )
      case _ =>
        // This will take care of chained calls
        receiverAstAndFullName(xnode, fieldIdentifier)
  }

  private def receiverAstAndFullName(xnode: ParserNodeInfo, fieldIdentifier: String): (Seq[Ast], String) = {
    val identifierAsts       = astForNode(xnode)
    val receiverTypeFullName = getTypeFullNameFromAstNode(identifierAsts)
    val fieldTypeFullName = goGlobal
      .getStructTypeMemberType(receiverTypeFullName, fieldIdentifier)
      .getOrElse(
        s"$receiverTypeFullName${Defines.dot}$fieldIdentifier${Defines.dot}${Defines.FieldAccess}${Defines.dot}${XDefines.Unknown}"
      )
    (identifierAsts, fieldTypeFullName)
  }

  protected def astForFieldAccess(info: ParserNodeInfo): Seq[Ast] = {
    val (identifierAsts, fieldTypeFullName) = processReceiver(info)
    val fieldIdentifier                     = info.json(ParserKeys.Sel)(ParserKeys.Name).str
    val callNode =
      operatorCallNode(info, info.code, Operators.fieldAccess, Some(fieldTypeFullName))
    Seq(callAst(callNode, identifierAsts ++ Seq(Ast(fieldIdentifierNode(info, fieldIdentifier, fieldIdentifier)))))
  }
}
