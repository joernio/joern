package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import ujson.Value

import scala.util.{Failure, Success, Try}

trait AstForTypeDeclCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForTypeSpec(typeSpecNode: ParserNodeInfo): Seq[Ast] = {
    // TODO: Add support for member variables and methods
    methodAstParentStack.collectFirst { case t: NewTypeDecl => t } match {
      case Some(parentMethodAstNode) =>
        val nameNode = typeSpecNode.json(ParserKeys.Name)
        val typeNode = createParserNodeInfo(typeSpecNode.json(ParserKeys.Type))

        val astParentType     = parentMethodAstNode.label
        val astParentFullName = parentMethodAstNode.fullName
        val fullName          = fullyQualifiedPackage + Defines.dot + nameNode(ParserKeys.Name).str
        val typeDeclNode_ =
          typeDeclNode(
            typeSpecNode,
            nameNode(ParserKeys.Name).str,
            fullName,
            parserResult.filename,
            typeSpecNode.code,
            astParentType,
            astParentFullName
          )

        methodAstParentStack.push(typeDeclNode_)
        scope.pushNewScope(typeDeclNode_)
        val memberAsts = astForStructType(typeNode)

        methodAstParentStack.pop()
        scope.popScope()

        val modifier = addModifier(typeDeclNode_, nameNode(ParserKeys.Name).str)
        Seq(Ast(typeDeclNode_).withChild(Ast(modifier)).withChildren(memberAsts))
      case None => Seq.empty
    }

  }

  protected def astForStructType(expr: ParserNodeInfo): Seq[Ast] = {
    Try(expr.json(ParserKeys.Fields)) match
      case Success(fields) if fields != null =>
        astForFieldList(createParserNodeInfo(fields))
      case _ => Seq.empty
  }

  private def astForFieldList(fieldList: ParserNodeInfo): Seq[Ast] = {
    fieldList
      .json(ParserKeys.List)
      .arrOpt
      .getOrElse(List())
      .flatMap(x => {
        val typeInfo = createParserNodeInfo(x(ParserKeys.Type))
        val (typeFullName, typeFullNameForCode, isVariadic, evaluationStrategy) = processTypeInfo(typeInfo)
        x(ParserKeys.Names).arrOpt
          .getOrElse(List())
          .map(fieldInfo => {
            val fieldNodeInfo = createParserNodeInfo(fieldInfo)
            val fieldName     = fieldNodeInfo.json(ParserKeys.Name).str
            Ast(memberNode(typeInfo, fieldName, fieldNodeInfo.code, typeFullName, Seq()))
          })
      })
      .toSeq
  }
}
