package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewMethodReturn}
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, NodeTypes}
import ujson.Value

trait AstForLambdaCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForFuncLiteral(funcLiteral: ParserNodeInfo): Seq[Ast] = {
    val lambdaName = nextClosureName()
    // if the top of the stack function is fake file level method node (which is checked with filename) then use the fully qualified package name as base fullname
    val baseFullName = methodAstParentStack
      .collectFirst({ case m: NewMethod if !m.fullName.endsWith(parserResult.filename) => m.fullName })
      .getOrElse(fullyQualifiedPackage)
    val fullName = s"$baseFullName.$lambdaName"
    val (signature, returnTypeStr, methodReturn, params, genericTypeMethodMap) = generateLambdaSignature(
      createParserNodeInfo(funcLiteral.json(ParserKeys.Type))
    )
    val methodNode_ = methodNode(funcLiteral, lambdaName, funcLiteral.code, fullName, Some(signature), relPathFileName)
    methodAstParentStack.push(methodNode_)
    scope.pushNewScope(methodNode_)
    val astForMethod =
      methodAst(
        methodNode_,
        astForMethodParameter(params, genericTypeMethodMap),
        astForMethodBody(funcLiteral.json(ParserKeys.Body)),
        methodReturn,
        newModifierNode(ModifierTypes.LAMBDA) :: Nil
      )
    scope.popScope()
    methodAstParentStack.pop()
    if (!goGlobal.processingDependencies) {
      // Create these lambda AST nodes only for main source code and ignore it while processing dependencies code.
      val typeDeclNode_ = typeDeclNode(funcLiteral, lambdaName, fullName, relPathFileName, lambdaName)
      if baseFullName == fullyQualifiedPackage then
        typeDeclNode_.astParentType(NodeTypes.TYPE_DECL).astParentFullName(fullyQualifiedPackage)
      else typeDeclNode_.astParentType(NodeTypes.METHOD).astParentFullName(baseFullName)
      val structTypes = Option(goGlobal.lambdaSignatureToLambdaTypeMap.get(signature)) match {
        case Some(types) => types.map(_._1)
        case None        => Seq.empty
      }
      typeDeclNode_.inheritsFromTypeFullName(structTypes)
      Ast.storeInDiffGraph(Ast(typeDeclNode_), diffGraph)
      // Setting Lambda TypeDecl as its parent.
      methodNode_.astParentType(NodeTypes.TYPE_DECL)
      methodNode_.astParentFullName(fullName)
      Ast.storeInDiffGraph(astForMethod, diffGraph)
    }
    goGlobal.recordFullNameToReturnType(fullName, returnTypeStr, signature)
    Seq(Ast(methodRefNode(funcLiteral, funcLiteral.code, fullName, fullName)))
  }

  protected def generateLambdaSignature(
    funcType: ParserNodeInfo
  ): (String, String, NewMethodReturn, Value, Map[String, List[String]]) = {
    val genericTypeMethodMap: Map[String, List[String]] = Map()
    // TODO: While handling the tuple return type we need to handle it here as well.
    val (returnTypeStr, returnTypeInfo) =
      getReturnType(funcType.json, genericTypeMethodMap).headOption
        .getOrElse((Defines.voidTypeName, funcType))
    val methodReturn = methodReturnNode(returnTypeInfo, returnTypeStr)

    val params         = funcType.json(ParserKeys.Params)(ParserKeys.List)
    val paramSignature = parameterSignature(params, genericTypeMethodMap)
    val signature =
      s"${XDefines.ClosurePrefix}($paramSignature)$returnTypeStr"
    (signature, returnTypeStr, methodReturn, params, genericTypeMethodMap)
  }
}
