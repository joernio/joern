package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, NodeTypes}
import ujson.Value

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success, Try}

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForFuncDecl(funcDecl: ParserNodeInfo): Seq[Ast] = {
    val methodMetadata = processFuncDecl(funcDecl.json)
    // TODO: handle multiple return type or tuple (int, int)
    val (returnTypeStr, returnTypeInfo) =
      getReturnType(funcDecl.json(ParserKeys.Type), methodMetadata.genericTypeMethodMap).headOption
        .getOrElse((Defines.voidTypeName, funcDecl))
    val methodReturn = methodReturnNode(returnTypeInfo, returnTypeStr)
    val methodNode_ = methodNode(
      funcDecl,
      methodMetadata.name,
      funcDecl.code,
      methodMetadata.methodFullname,
      Some(methodMetadata.signature),
      relPathFileName
    )
    methodAstParentStack.push(methodNode_)
    scope.pushNewScope(methodNode_)
    val receiverNode = astForReceiver(methodMetadata.receiverInfo)
    val astForMethod =
      methodAst(
        methodNode_,
        receiverNode ++ astForMethodParameter(methodMetadata.params, methodMetadata.genericTypeMethodMap),
        astForMethodBody(funcDecl.json(ParserKeys.Body)),
        methodReturn
      )
    scope.popScope()
    methodAstParentStack.pop()
    methodMetadata.receiverInfo match
      case Some(_, typeFullName, _, _) =>
        // if method is related to Struct then fill astParentFullName and astParentType
        methodNode_.astParentType(NodeTypes.TYPE_DECL).astParentFullName(typeFullName)
      case _ =>
        methodNode_.astParentType(NodeTypes.TYPE_DECL).astParentFullName(fullyQualifiedPackage)
    Ast.storeInDiffGraph(astForMethod, diffGraph)
    Seq.empty
  }

  private def astForReceiver(receiverInfo: Option[(String, String, String, ParserNodeInfo)]): Seq[Ast] = {
    receiverInfo match
      case Some(recName, typeFullName, evaluationStrategy, recNode) =>
        val recParamNode = NodeBuilders.newThisParameterNode(
          name = recName,
          code = recNode.code,
          typeFullName = typeFullName,
          line = line(recNode),
          column = column(recNode),
          evaluationStrategy = evaluationStrategy
        )
        scope.addToScope(recName, (recParamNode, typeFullName))
        Seq(Ast(recParamNode))
      case _ => Seq.empty
  }

  protected def getReceiverInfo(receiver: Try[Value]): Option[(String, String, String, ParserNodeInfo)] = {
    receiver match
      case Success(rec) if rec != null =>
        val recnode = createParserNodeInfo(rec)
        rec(ParserKeys.List).arr.headOption.map(recValue => {
          val recName        = Try(recValue(ParserKeys.Names).arr.head(ParserKeys.Name).str).getOrElse(Defines.This)
          val typeParserNode = createParserNodeInfo(recValue(ParserKeys.Type))
          val (typeFullName, evaluationStrategy) = typeParserNode.node match
            case Ident =>
              (
                generateTypeFullName(typeName = typeParserNode.json(ParserKeys.Name).strOpt),
                EvaluationStrategies.BY_VALUE
              )
            case StarExpr =>
              (
                generateTypeFullName(typeName = typeParserNode.json(ParserKeys.X)(ParserKeys.Name).strOpt),
                EvaluationStrategies.BY_SHARING
              )
            case x =>
              logger.warn(s"Unhandled class ${x.getClass} under getReceiverInfo! file -> ${parserResult.fullPath}")
              ("", "")
          (recName, typeFullName, evaluationStrategy, recnode)
        })
      case _ => None
  }

  protected def astForMethodParameter(params: Value, genericTypeMethodMap: Map[String, List[String]]): Seq[Ast] = {
    var index = 1
    params.arrOpt
      .getOrElse(List())
      .flatMap(x =>
        val typeInfo = createParserNodeInfo(x(ParserKeys.Type))
        val (typeFullName, typeFullNameForcode, isVariadic, evaluationStrategy) =
          processTypeInfo(typeInfo, genericTypeMethodMap)
        x(ParserKeys.Names).arrOpt
          .getOrElse(List())
          .map(y => {
            // We are returning same type from x object for each name in the names array.
            val parameterInfo = createParserNodeInfo(y)
            val paramName     = parameterInfo.json(ParserKeys.Name).str
            val parameterNode = parameterInNode(
              parameterInfo,
              paramName,
              s"${paramName} ${typeFullNameForcode}",
              index,
              isVariadic,
              evaluationStrategy,
              typeFullName
            )
            index += 1
            scope.addToScope(paramName, (parameterNode, typeFullName))
            Ast(parameterNode)
          })
      )
      .toSeq
  }

  protected def parameterSignature(params: Value, genericTypeMethodMap: Map[String, List[String]]): String = {
    //    func foo(argc, something int, argv string) int {
    // We get params -> list -> names [argc, something], type (int)
    params.arrOpt
      .getOrElse(ArrayBuffer())
      .map(x =>
        val typeInfo                                           = createParserNodeInfo(x(ParserKeys.Type))
        val (typeFullName, typeFullNameForcode, isVariadic, _) = processTypeInfo(typeInfo, genericTypeMethodMap)
        x(ParserKeys.Names).arrOpt
          /*
          While generating the signature for a function structure
                   func test(a, b int, c string) int {
                   }
          it works as there is no situation where parameter name will not be there.

          As we re reuse the same function for generating the signature for lambda types as below
                   type Operation func(int, int) int
          Now in this case there is no parameter name exist, in order to handle this situation add this empty string as default value,
          which only facilitates adding the parameter type to list.
           */
          .getOrElse(List(""))
          .map(_ => {
            // We are returning same type from x object for each name in the names array.
            typeFullName
          })
          .mkString(", ")
      )
      .mkString(", ")
  }

  protected def getReturnType(
    methodTypes: Value,
    genericTypeMethodMap: Map[String, List[String]] = Map.empty
  ): Seq[(String, ParserNodeInfo)] = {
    Try(methodTypes(ParserKeys.Results)) match
      case Success(returnType) =>
        returnType(ParserKeys.List).arrOpt
          .getOrElse(List())
          .map(x =>
            val typeInfo                = createParserNodeInfo(x(ParserKeys.Type))
            val (typeFullName, _, _, _) = processTypeInfo(typeInfo, genericTypeMethodMap)
            (typeFullName, typeInfo)
          )
          .toSeq
      case Failure(exception) => Seq.empty
  }

  def astForMethodBody(body: Value): Ast = {

    val nodeInfo = createParserNodeInfo(body)
    nodeInfo.node match {
      case BlockStmt => astForBlockStatement(nodeInfo)
      case _         => Ast()
    }
  }

  protected def processTypeParams(funDecl: Value): Map[String, List[String]] = {
    val genericTypeMethodMap = new mutable.HashMap[String, List[String]]()
    Try(funDecl(ParserKeys.TypeParams)) match {
      case Success(typeParams) =>
        if (typeParams.obj.contains(ParserKeys.List)) {
          typeParams
            .obj(ParserKeys.List)
            .arr
            .foreach(params => {
              if (params.obj.contains(ParserKeys.Type)) {
                val genericTypeName = params
                  .obj(ParserKeys.Names)
                  .arr
                  .headOption
                  .get
                  .obj(ParserKeys.Name)
                  .str
                val genericMethodList = ListBuffer[String]()
                processGenericType(params(ParserKeys.Type), genericMethodList)
                genericTypeMethodMap.put(genericTypeName, genericMethodList.toList)
              }
            })
        }
      case _ =>
        Map()
    }
    genericTypeMethodMap.toMap
  }

  private def processGenericType(typeDecl: Value, genericTypeList: ListBuffer[String]): Unit = {
    if (typeDecl.obj.contains(ParserKeys.Name)) {
      genericTypeList.addOne(typeDecl(ParserKeys.Name).str)
    } else if (typeDecl.obj.contains(ParserKeys.Op)) {
      processGenericType(typeDecl(ParserKeys.X), genericTypeList)
      processGenericType(typeDecl(ParserKeys.Y), genericTypeList)
    }
  }
}
