package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewMethodReturn, NewTypeDecl}
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
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
    baseFullName match
      case fullyQualifiedPackage =>
        methodNode_.astParentType(NodeTypes.TYPE_DECL).astParentFullName(fullyQualifiedPackage)
      case _ =>
        methodNode_.astParentType(NodeTypes.METHOD).astParentFullName(baseFullName)
    Ast.storeInDiffGraph(astForMethod, diffGraph)
    // TODO: Create TypeDecl for lambda function for which we didnt find the type.
    // We need to create TypeDecl for every lambda function and set its inheritance with all the matching lambda types.
    //    val typeFullName = GoGlobal.lambdaSignatureToLambdaTypeMap.getOrDefault(signature, fullName)
    GoGlobal.recordFullNameToReturnType(fullName, returnTypeStr, signature)
    Seq(Ast(methodRefNode(funcLiteral, funcLiteral.code, fullName, fullName)))
  }

  private def generateLambdaSignature(
    funcType: ParserNodeInfo
  ): (String, String, NewMethodReturn, Value, Map[String, List[String]]) = {
    val genericTypeMethodMap: Map[String, List[String]] = Map()
    val (returnTypeStr, returnTypeInfo) =
      getReturnType(funcType.json, genericTypeMethodMap).headOption
        .getOrElse((Defines.voidTypeName, funcType))
    val methodReturn = methodReturnNode(returnTypeInfo, returnTypeStr)

    val params = funcType.json(ParserKeys.Params)(ParserKeys.List)
    val signature =
      s"${XDefines.ClosurePrefix}(${parameterSignature(params, genericTypeMethodMap)})$returnTypeStr"
    (signature, returnTypeStr, methodReturn, params, genericTypeMethodMap)
  }
}
