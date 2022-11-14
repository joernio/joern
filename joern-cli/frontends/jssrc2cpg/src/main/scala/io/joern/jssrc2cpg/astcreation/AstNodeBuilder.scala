package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.MethodScope
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg
import io.joern.x2cpg.Ast
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

  protected def createDependencyNode(name: String, groupId: String, version: String): NewDependency =
    NewDependency()
      .name(name)
      .dependencyGroupId(groupId)
      .version(version)

  protected def createTypeRefNode(code: String, typeFullName: String, classNode: BabelNodeInfo): NewTypeRef =
    NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(classNode.lineNumber)
      .columnNumber(classNode.columnNumber)

  protected def createTypeDeclNode(
    name: String,
    fullname: String,
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

  protected def createReturnNode(ret: BabelNodeInfo): NewReturn =
    NewReturn()
      .code(ret.code.stripSuffix(";"))
      .lineNumber(ret.lineNumber)
      .columnNumber(ret.columnNumber)

  protected def createMethodReturnNode(func: BabelNodeInfo): NewMethodReturn = {
    val line   = func.lineNumber
    val column = func.columnNumber
    val code   = "RET"
    val tpe    = typeFor(func)
    NewMethodReturn()
      .code(code)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName(tpe)
      .lineNumber(line)
      .columnNumber(column)
  }

  protected def setIndices(
    asts: List[Ast],
    receiver: Option[Ast] = None,
    countEmpty: Boolean = false,
    base: Option[Ast] = None
  ): Unit = {
    var currIndex = 1
    var currOrder = if (base.isDefined) 2 else 1

    asts.foreach { a =>
      a.root match {
        case Some(x: ExpressionNew) =>
          x.argumentIndex = currIndex
          x.order = currOrder
          currIndex = currIndex + 1
          currOrder = currOrder + 1
        case None if !countEmpty => // do nothing
        case _ =>
          currIndex = currIndex + 1
          currOrder = currOrder + 1
      }
    }
    val baseRoot = base.flatMap(_.root).toList
    baseRoot match {
      case List(x: ExpressionNew) =>
        x.argumentIndex = 0
        x.order = 1
      case _ =>
    }
    val receiverRoot = receiver.flatMap(_.root).toList
    receiverRoot match {
      case List(x: ExpressionNew) =>
        x.argumentIndex = 0
        x.order = 0
      case _ =>
    }
  }

  protected def createCallAst(
    callNode: NewCall,
    arguments: List[Ast],
    receiver: Option[Ast] = None,
    base: Option[Ast] = None
  ): Ast = {
    setIndices(arguments, receiver, base = base)

    val receiverRoot = receiver.flatMap(_.root).toList
    val rcvAst       = receiver.getOrElse(Ast())
    val baseRoot     = base.flatMap(_.root).toList
    val baseAst      = base.getOrElse(Ast())
    Ast(callNode)
      .withChild(rcvAst)
      .withChild(baseAst)
      .withChildren(arguments)
      .withArgEdges(callNode, arguments.flatMap(_.root))
      .withArgEdges(callNode, baseRoot)
      .withReceiverEdges(callNode, receiverRoot)
  }

  protected def createReturnAst(returnNode: NewReturn, arguments: List[Ast] = List()): Ast = {
    setIndices(arguments)
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
      .typeFullName(tpe.getOrElse(Defines.ANY))
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

  protected def createImportNode(
    impDecl: BabelNodeInfo,
    importedEntity: Option[String],
    importedAs: String
  ): NewImport =
    NewImport()
      .code(impDecl.code.stripSuffix(";"))
      .importedEntity(importedEntity)
      .importedAs(importedAs)
      .lineNumber(impDecl.lineNumber)
      .columnNumber(impDecl.columnNumber)

  protected def createMemberNode(name: String, code: String, dynamicTypeOption: Option[String]): NewMember =
    NewMember()
      .code(code)
      .name(name)
      .typeFullName(Defines.ANY)
      .dynamicTypeHintFullName(dynamicTypeOption.toList)

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
    createCallAst(callNode, arguments)
  }

  protected def createFieldAccessCallAst(
    baseNode: NewNode,
    partNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val callNode = createCallNode(
      codeOf(baseNode) + "." + codeOf(partNode),
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(Ast(baseNode), Ast(partNode))
    createCallAst(callNode, arguments)
  }

  protected def createTernaryCallAst(
    testNode: NewNode,
    trueNode: NewNode,
    falseNode: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val code      = codeOf(testNode) + " ? " + codeOf(trueNode) + " : " + codeOf(falseNode)
    val callNode  = createCallNode(code, Operators.conditional, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(Ast(testNode), Ast(trueNode), Ast(falseNode))
    createCallAst(callNode, arguments)
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
    .typeFullName(Defines.ANY)

  protected def createVoidCallNode(line: Option[Integer], column: Option[Integer]): NewCall =
    createCallNode("void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH, line, column)

  protected def createFieldIdentifierNode(
    name: String,
    line: Option[Integer],
    column: Option[Integer]
  ): NewFieldIdentifier = NewFieldIdentifier()
    .code(name)
    .canonicalName(name)
    .lineNumber(line)
    .columnNumber(column)

  protected def createLiteralNode(
    code: String,
    dynamicTypeOption: Option[String],
    line: Option[Integer],
    column: Option[Integer]
  ): NewLiteral = {
    val typeFullName = dynamicTypeOption match {
      case Some(value) if Defines.JSTYPES.contains(value) => value
      case _                                              => Defines.ANY
    }
    NewLiteral()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
      .dynamicTypeHintFullName(dynamicTypeOption.toList)
  }

  protected def createEqualsCallAst(
    destId: NewNode,
    sourceId: NewNode,
    line: Option[Integer],
    column: Option[Integer]
  ): Ast = {
    val code      = codeOf(destId) + " === " + codeOf(sourceId)
    val callNode  = createCallNode(code, Operators.equals, DispatchTypes.STATIC_DISPATCH, line, column)
    val arguments = List(Ast(destId), Ast(sourceId))
    createCallAst(callNode, arguments)
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
    createCallAst(callNode, arguments)
  }

  protected def createIdentifierNode(name: String, node: BabelNodeInfo): NewIdentifier = {
    val dynamicInstanceTypeOption = name match {
      case "this"    => dynamicInstanceTypeStack.headOption
      case "console" => Some(Defines.CONSOLE)
      case "Math"    => Some(Defines.MATH)
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
    .typeFullName(Defines.ANY)
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
    .typeFullName(Defines.ANY)

  protected def createLocalNode(name: String, typeFullName: String, closureBindingId: Option[String] = None): NewLocal =
    NewLocal().code(name).name(name).typeFullName(typeFullName).closureBindingId(closureBindingId).order(0)

  protected def createClosureBindingNode(closureBindingId: String, closureOriginalName: String): NewClosureBinding =
    NewClosureBinding()
      .closureBindingId(Some(closureBindingId))
      .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
      .closureOriginalName(Some(closureOriginalName))

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

  protected def createBlockNode(node: BabelNodeInfo): NewBlock =
    NewBlock()
      .typeFullName(Defines.ANY)
      .code(node.code)
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
      ).inheritsFromTypeFullName(List(Defines.ANY))

    // Problem for https://github.com/ShiftLeftSecurity/codescience/issues/3626 here.
    // As the type (thus, the signature) of the function node is unknown (i.e., ANY*)
    // we can't generate the correct binding with signature.
    val bindingNode = createBindingNode()
    Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
  }

}
