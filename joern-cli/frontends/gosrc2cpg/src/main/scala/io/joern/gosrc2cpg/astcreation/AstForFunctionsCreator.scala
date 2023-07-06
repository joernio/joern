package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.{BlockStmt, Ellipsis, Ident, SelectorExpr}
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.joern.x2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import ujson.Value

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait AstForFunctionsCreator { this: AstCreator =>
  def astForFuncDecl(funcDecl: ParserNodeInfo): Seq[Ast] = {

    val filename = relPathFileName
    val name     = funcDecl.json(ParserKeys.Name).obj(ParserKeys.Name).str
    val fullname = s"${fullyQualifiedPackage.get()}.${name}"
    // TODO: handle multiple return type or tuple (int, int)
    val returnType     = getReturnType(funcDecl.json(ParserKeys.Type)).headOption.getOrElse("")
    val templateParams = ""
    val methodType     = createParserNodeInfo(funcDecl.json(ParserKeys.Type))
    val signature =
      s"$fullname$templateParams (${parameterSignature(methodType)})$returnType "

    val methodNode_ = methodNode(funcDecl, name, funcDecl.code, fullname, Some(signature), filename)
    methodAstParentStack.push(methodNode_)
    scope.pushNewScope(methodNode_)
    val astForMethod = methodAst(
      methodNode_,
      astForMethodParameter(methodType),
      astForMethodBody(funcDecl.json(ParserKeys.Body)),
      newMethodReturnNode(returnType, None, line(funcDecl), column(funcDecl))
    )
    scope.popScope()
    methodAstParentStack.pop()
    // TODO register type above
    Seq(astForMethod)
  }

  private def astForMethodParameter(paramType: ParserNodeInfo): Seq[Ast] = {
    Seq(Ast(NewMethodParameterIn()))
  }

  private def parameterSignature(paramType: ParserNodeInfo): String = {
    //    func foo(argc, something int, argv string) int {
    // We get params -> list -> names [argc, something], type (int)
    paramType
      .json(ParserKeys.Params)(ParserKeys.List)
      .arrOpt
      .map(a =>
        a.map(x =>
          x(ParserKeys.Names).arr
            .map(y => {
              // We are returning same type from x object for each name in the names array.
              val typeInfo = createParserNodeInfo(x(ParserKeys.Type))
              typeInfo.node match {
                case Ident    => typeInfo.json(ParserKeys.Name).str
                case Ellipsis => "..." + typeInfo.json(ParserKeys.Elt)(ParserKeys.Name).str
                case SelectorExpr =>
                  typeInfo.json(ParserKeys.X)(ParserKeys.Name).str + "." + typeInfo
                    .json(ParserKeys.Sel)(ParserKeys.Name)
                    .str
              }
            }) mkString (", ")
        )
      )
      .getOrElse(ArrayBuffer())
      .mkString(", ")
  }

  private def getReturnType(methodTypes: Value): Seq[String] = {
    if (methodTypes.obj.contains(ParserKeys.Results))
      methodTypes(ParserKeys.Results)(ParserKeys.List).arr.map(x => x(ParserKeys.Type)(ParserKeys.Name))
    Seq()
  }

  def astForMethodBody(body: Value): Ast = {

    val nodeInfo = createParserNodeInfo(body)
    nodeInfo.node match {
      case BlockStmt => astForBlockStatement(nodeInfo)
      case _         => Ast()
    }
  }
}
