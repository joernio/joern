package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.MethodScope
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.methodReturnNode
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.Operators

trait AstNodeBuilder { this: AstCreator =>

  protected def newUnknown(node: BabelNodeInfo): NewUnknown =
    NewUnknown()
      .parserTypeName(node.node.toString)
      .code(node.code)
      .lineNumber(node.lineNumber)
      .columnNumber(node.columnNumber)

  protected def createAnnotationNode(annotation: BabelNodeInfo, name: String, fullName: String): NewAnnotation = {
    val code         = annotation.code
    val lineNumber   = annotation.lineNumber
    val columnNumber = annotation.columnNumber
    NewAnnotation()
      .code(code)
      .name(name)
      .fullName(fullName)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  protected def createTypeRefNode(code: String, typeFullName: String, classNode: BabelNodeInfo): NewTypeRef =
    NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(classNode.lineNumber)
      .columnNumber(classNode.columnNumber)

  protected def createTypeDeclNode(
    name: String,
    fullName: String,
    filename: String,
    code: String,
    astParentType: String = "",
    astParentFullName: String = "",
    inherits: Seq[String] = Seq.empty,
    alias: Option[String] = None,
    line: Option[Integer] = None,
    column: Option[Integer] = None
  ): NewTypeDecl =
    NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .inheritsFromTypeFullName(inherits)
      .aliasTypeFullName(alias)
      .lineNumber(line)
      .columnNumber(column)

  protected def createReturnNode(ret: BabelNodeInfo): NewReturn =
    NewReturn()
      .code(ret.code.stripSuffix(";"))
      .lineNumber(ret.lineNumber)
      .columnNumber(ret.columnNumber)

  protected def createMethodReturnNode(func: BabelNodeInfo): NewMethodReturn = {
    methodReturnNode(typeFor(func), line = func.lineNumber, column = func.columnNumber)
  }

  protected def setOrderExplicitly(ast: Ast, order: Int): Unit = {
    ast.root.foreach { case expr: ExpressionNew => expr.order = order }
  }

  protected def createReturnAst(returnNode: NewReturn, arguments: List[Ast] = List()): Ast = {
    setArgumentIndices(arguments)
    Ast(returnNode)
      .withChildren(arguments)
      .withArgEdges(returnNode, arguments.flatMap(_.root))
  }

  protected def createJumpTarget(switchCase: BabelNodeInfo): NewJumpTarget = {
    val (switchName, switchCode) = if (switchCase.json("test").isNull) {
      ("default", "default:")
    } else {
      ("case", s"case ${code(switchCase.json("test"))}:")
    }
    NewJumpTarget()
      .parserTypeName(switchCase.node.toString)
      .name(switchName)
      .code(switchCode)
      .lineNumber(switchCase.lineNumber)
      .columnNumber(switchCase.columnNumber)
  }

  protected def createControlStructureNode(node: BabelNodeInfo, controlStructureType: String): NewControlStructure = {
    val line   = node.lineNumber
    val column = node.columnNumber
    val code   = node.code
    NewControlStructure()
      .controlStructureType(controlStructureType)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def createParameterInNode(
    name: String,
    code: String,
    index: Int,
    isVariadic: Boolean,
    line: Option[Integer],
    column: Option[Integer],
    tpe: Option[String] = None
  ): NewMethodParameterIn = {
    val param = NewMethodParameterIn()
      .name(name)
      .code(code)
      .index(index)
      .order(index)
      .isVariadic(isVariadic)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)
      .typeFullName(tpe.getOrElse(Defines.Any))
    scope.addVariable(name, param, MethodScope)
    param
  }

  protected def createMethodRefNode(code: String, methodFullName: String, func: BabelNodeInfo): NewMethodRef = {
    val line   = func.lineNumber
    val column = func.columnNumber
    NewMethodRef()
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName(methodFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def createMemberNode(name: String, node: BabelNodeInfo, dynamicTypeOption: Option[String]): NewMember = {
    val tpe  = typeFor(node)
    val code = node.code
    NewMember()
      .code(code)
      .name(name)
      .typeFullName(tpe)
      .dynamicTypeHintFullName(dynamicTypeOption.toList)
  }

  protected def createMethodNode(methodName: String, methodFullName: String, func: BabelNodeInfo): NewMethod = {
    val line      = func.lineNumber
    val column    = func.columnNumber
    val lineEnd   = func.lineNumberEnd
    val columnEnd = func.columnNumberEnd
    val code      = func.code
    NewMethod()
      .name(methodName)
      .filename(parserResult.filename)
      .code(code)
      .fullName(methodFullName)
      .isExternal(false)
      .lineNumber(line)
      .columnNumber(column)
      .lineNumberEnd(lineEnd)
      .columnNumberEnd(columnEnd)
  }

  protected def codeOf(node: NewNode): String = node match {
    case node: AstNodeNew => node.code
    case _                => ""
  }

  protected def createIndexAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseNode)}[${codeOf(partNode)}]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(Ast(baseNode), Ast(partNode))
    callAst(callNode, arguments)
  }

  protected def createIndexAccessCallAst(
    baseAst: Ast,
    partAst: Ast,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseAst.nodes.head)}[${codeOf(partAst.nodes.head)}]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(baseAst, partAst)
    callAst(callNode, arguments)
  }

  protected def createFieldAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseNode)}.${codeOf(partNode)}",
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(Ast(baseNode), Ast(partNode))
    callAst(callNode, arguments)
  }

  protected def createFieldAccessCallAst(
    baseAst: Ast,
    partNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      s"${codeOf(baseAst.nodes.head)}.${codeOf(partNode)}",
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(baseAst, Ast(partNode))
    callAst(callNode, arguments)
  }

  protected def createTernaryCallAst(
    testAst: Ast,
    trueAst: Ast,
    falseAst: Ast,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val code      = s"${codeOf(testAst.nodes.head)} ? ${codeOf(trueAst.nodes.head)} : ${codeOf(falseAst.nodes.head)}"
    val callNode  = createCallNode(code, Operators.conditional, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(testAst, trueAst, falseAst)
    callAst(callNode, arguments)
  }

  protected def createCallNode(
    code: String,
    callName: String,
    dispatchType: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewCall = NewCall()
    .code(code)
    .name(callName)
    .methodFullName(
      if (dispatchType == DispatchTypes.STATIC_DISPATCH) callName else x2cpg.Defines.DynamicCallUnknownFallName
    )
    .dispatchType(dispatchType)
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.Any)

  protected def createVoidCallNode(line: Option[Integer], column: Option[Integer]): NewCall =
    createCallNode("void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH, line, column)

  protected def createFieldIdentifierNode(
    name: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewFieldIdentifier = {
    val cleanedName = stripQuotes(name)
    NewFieldIdentifier()
      .code(cleanedName)
      .canonicalName(cleanedName)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def createLiteralNode(
    code: String,
    dynamicTypeOption: Option[String],
    line: Option[Integer],
    column: Option[Integer]
  ): NewLiteral = {
    val typeFullName = dynamicTypeOption match {
      case Some(value) if Defines.JsTypes.contains(value) => value
      case _                                              => Defines.Any
    }
    NewLiteral()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
      .dynamicTypeHintFullName(dynamicTypeOption.toList)
  }

  protected def createEqualsCallAst(dest: Ast, source: Ast, line: Option[Integer], column: Option[Integer]): Ast = {
    val code      = s"${codeOf(dest.nodes.head)} === ${codeOf(source.nodes.head)}"
    val callNode  = createCallNode(code, Operators.equals, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(dest, source)
    callAst(callNode, arguments)
  }

  protected def createAssignmentCallAst(
    destId: NewNode,
    sourceId: NewNode,
    code: String,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode  = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(Ast(destId), Ast(sourceId))
    callAst(callNode, arguments)
  }

  protected def createAssignmentCallAst(
    dest: Ast,
    source: Ast,
    code: String,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode  = createCallNode(code, Operators.assignment, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(dest, source)
    callAst(callNode, arguments)
  }

  protected def createIdentifierNode(name: String, node: BabelNodeInfo): NewIdentifier = {
    val dynamicInstanceTypeOption = name match {
      case "this"    => dynamicInstanceTypeStack.headOption
      case "console" => Option(Defines.Console)
      case "Math"    => Option(Defines.Math)
      case _         => None
    }
    createIdentifierNode(name, dynamicInstanceTypeOption, node.lineNumber, node.columnNumber)
  }

  protected def createIdentifierNode(
    name: String,
    dynamicTypeOption: Option[String],
    line: Option[Integer],
    column: Option[Integer]
  ): NewIdentifier = NewIdentifier()
    .name(name)
    .code(name)
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.Any)
    .dynamicTypeHintFullName(dynamicTypeOption.toList)

  protected def createStaticCallNode(
    code: String,
    callName: String,
    fullName: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewCall = NewCall()
    .code(code)
    .name(callName)
    .methodFullName(fullName)
    .dispatchType(DispatchTypes.STATIC_DISPATCH)
    .signature("")
    .lineNumber(line)
    .columnNumber(column)
    .typeFullName(Defines.Any)

  protected def createLocalNode(name: String, typeFullName: String, closureBindingId: Option[String] = None): NewLocal =
    NewLocal().code(name).name(name).typeFullName(typeFullName).closureBindingId(closureBindingId).order(0)

  protected def createClosureBindingNode(closureBindingId: String, closureOriginalName: String): NewClosureBinding =
    NewClosureBinding()
      .closureBindingId(Option(closureBindingId))
      .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
      .closureOriginalName(Option(closureOriginalName))

  protected def createBindingNode(): NewBinding = NewBinding().name("").signature("")

  protected def createTemplateDomNode(
    name: String,
    code: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewTemplateDom =
    NewTemplateDom()
      .name(name)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)

  protected def createBlockNode(node: BabelNodeInfo, customCode: Option[String] = None): NewBlock =
    NewBlock()
      .typeFullName(Defines.Any)
      .code(customCode.getOrElse(node.code))
      .lineNumber(node.lineNumber)
      .columnNumber(node.columnNumber)

  protected def createFunctionTypeAndTypeDeclAst(
    methodNode: NewMethod,
    parentNode: NewNode,
    methodName: String,
    methodFullName: String,
    filename: String
  ): Ast = {
    registerType(methodName, methodFullName)

    val astParentType     = parentNode.label
    val astParentFullName = parentNode.properties("FULL_NAME").toString
    val functionTypeDeclNode =
      createTypeDeclNode(
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName
      ).inheritsFromTypeFullName(List(Defines.Any))

    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = createBindingNode()
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

}
