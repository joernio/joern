package io.joern.gosrc2cpg.astcreation

import io.joern.x2cpg.ValidationMode
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import ujson.Value
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl

import scala.util.Try

trait AstForStructuresCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForTypeSpec(typeSpecNode: ParserNodeInfo): Seq[Ast] = {
    // TODO: Add support for member variables and methods
    Option(typeSpecNode) match {
      case Some(typeSpec) =>
        val nameNode          = typeSpec.json(ParserKeys.Name)
        val typeNode          = createParserNodeInfo(typeSpec.json(ParserKeys.Type))
        val a: NewTypeDecl    = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }.get
        val astParentType     = a.label
        val astParentFullName = a.fullName
        val fullName          = nameNode(ParserKeys.Name).str // TODO: Discuss fullName structure
        val typeDeclNode_ =
          typeDeclNode(
            typeSpec,
            nameNode(ParserKeys.Name).str,
            fullName,
            parserResult.filename,
            typeSpec.code,
            astParentType,
            astParentFullName
          )

        methodAstParentStack.push(typeDeclNode_)
        scope.pushNewScope(typeDeclNode_)
        val memberAsts = astForStructType(typeNode)

        methodAstParentStack.pop()
        scope.popScope()

        val modifier = addModifier(typeDeclNode_, nameNode(ParserKeys.Name).str)
        Seq(Ast(typeDeclNode_).withChild(Ast(modifier)).withChildren(memberAsts.toSeq))
      case None =>
        Seq.empty
    }
  }

  protected def astForStructType(expr: ParserNodeInfo): Seq[Ast] = {
    astForFieldList(createParserNodeInfo(expr.json(ParserKeys.Fields)))
  }

  private def astForFieldList(fieldList: ParserNodeInfo): Seq[Ast] = {
    fieldList
      .json(ParserKeys.List)
      .arr
      .map(createParserNodeInfo)
      .map(astForField)
      .toSeq
  }

}
