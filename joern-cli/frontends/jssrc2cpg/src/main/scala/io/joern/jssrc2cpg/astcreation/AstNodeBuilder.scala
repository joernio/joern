package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.scope.MethodScope
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import ujson.Value

trait AstNodeBuilder {

  this: AstCreator =>

  protected def newUnknown(node: Value, order: Int): NewUnknown =
    NewUnknown()
      .parserTypeName(nodeType(node).toString)
      .code(code(node))
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(node))
      .columnNumber(column(node))

  protected def newTypeDecl(
    name: String,
    fullname: String,
    filename: String,
    code: String,
    astParentType: String = "",
    astParentFullName: String = "",
    order: Int = -1,
    inherits: Seq[String] = Seq.empty,
    alias: Option[String] = None,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewTypeDecl =
    NewTypeDecl()
      .name(name)
      .fullName(fullname)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .inheritsFromTypeFullName(inherits)
      .aliasTypeFullName(alias)
      .lineNumber(line)
      .columnNumber(column)
      .order(order)

  protected def createParameterInNode(
    name: String,
    code: String,
    order: Int,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewMethodParameterIn = {
    val param = NewMethodParameterIn()
      .name(name)
      .code(shortenCode(code))
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)
      .order(order)
      .typeFullName(Defines.ANY.label)
    scope.addVariable(name, param, MethodScope)
    param
  }

  protected def createTypeNode(name: String, fullName: String): NewType =
    NewType()
      .name(name)
      .fullName(fullName)
      .typeDeclFullName(fullName)

  protected def createBindingNode(): NewBinding =
    NewBinding()
      .name("")
      .signature("")

  protected def createFunctionTypeAndTypeDeclAst(
    methodNode: NewMethod,
    parentNode: NewNode,
    methodName: String,
    methodFullName: String,
    filename: String
  ): Ast = {
    val typeNode = createTypeNode(methodName, methodFullName)
    Ast.storeInDiffGraph(Ast(typeNode), diffGraph)

    val astParentType     = parentNode.label
    val astParentFullName = parentNode.properties("FULL_NAME").toString
    val functionTypeDeclNode =
      newTypeDecl(
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        order = 0
      ).inheritsFromTypeFullName(List(Defines.ANY.label))
    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = createBindingNode()
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

}
