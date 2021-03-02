package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, nodes}
import io.shiftleft.passes.DiffGraph

class NodeBuilder(diffGraph: DiffGraph.Builder) {

  private def addNodeToDiff[T <: nodes.NewNode](node: T): T = {
    diffGraph.addNode(node)
    node
  }

  def callNode(
      code: String,
      name: String,
      dispatchType: String,
      lineAndColumn: LineAndColumn
  ): nodes.NewCall = {
    val callNode = nodes
      .NewCall()
      .code(code)
      .name(name)
      .methodFullName(name)
      .dispatchType(dispatchType)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(callNode)
  }

  def typeNode(name: String, fullName: String): nodes.NewType = {
    val typeNode = nodes
      .NewType()
      .name(name)
      .fullName(fullName)
      .typeDeclFullName(fullName)
    addNodeToDiff(typeNode)
  }

  def typeDeclNode(name: String, fullName: String): nodes.NewTypeDecl = {
    val typeDeclNode = nodes
      .NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .isExternal(false)
    addNodeToDiff(typeDeclNode)
  }

  def typeRefNode(code: String, lineAndColumn: LineAndColumn): nodes.NewTypeRef = {
    val typeRefNode = nodes
      .NewTypeRef()
      .code(code)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(typeRefNode)
  }

  def bindingNode(): nodes.NewBinding = {
    val bindingNode = nodes.NewBinding()

    addNodeToDiff(bindingNode)
  }

  def methodNode(name: String, fullName: String, lineAndColumn: LineAndColumn): nodes.NewMethod = {
    val methodNode = nodes
      .NewMethod()
      .name(name)
      .fullName(fullName)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(methodNode)
  }

  def methodRefNode(
      name: String,
      fullName: String,
      lineAndColumn: LineAndColumn
  ): nodes.NewMethodRef = {
    val methodRefNode = nodes
      .NewMethodRef()
      .code(name)
      .methodFullName(fullName)
      .typeFullName(fullName)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(methodRefNode)
  }

  def closureBindingNode(
      closureBindingId: String,
      closureOriginalName: String
  ): nodes.NewClosureBinding = {
    val closureBindingNode = nodes
      .NewClosureBinding()
      .closureBindingId(Some(closureBindingId))
      .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
      .closureOriginalName(Some(closureOriginalName))
    addNodeToDiff(closureBindingNode)
  }

  def methodParameterNode(
      name: String,
      lineAndColumn: LineAndColumn
  ): nodes.NewMethodParameterIn = {
    val methodParameterNode = nodes
      .NewMethodParameterIn()
      .name(name)
      .code(name)
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)
      .typeFullName(Constants.ANY)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(methodParameterNode)
  }

  def methodReturnNode(lineAndColumn: LineAndColumn): nodes.NewMethodReturn = {
    val methodReturnNode = nodes
      .NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)
      .typeFullName(Constants.ANY)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))

    addNodeToDiff(methodReturnNode)
  }

  def returnNode(code: String, lineAndColumn: LineAndColumn): nodes.NewReturn = {
    val returnNode = nodes
      .NewReturn()
      .code(code)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))

    addNodeToDiff(returnNode)
  }

  def identifierNode(name: String, lineAndColumn: LineAndColumn): nodes.NewIdentifier = {
    val identifierNode = nodes
      .NewIdentifier()
      .code(name)
      .name(name)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(identifierNode)
  }

  def fieldIdentifierNode(name: String, lineAndColumn: LineAndColumn): nodes.NewFieldIdentifier = {
    val fieldIdentifierNode = nodes
      .NewFieldIdentifier()
      .code(name)
      .canonicalName(name)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(fieldIdentifierNode)
  }

  def numberLiteralNode(number: Int, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    numberLiteralNode(number.toString, lineAndColumn)
  }

  def numberLiteralNode(number: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    val literalNode = nodes
      .NewLiteral()
      .code(number)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(literalNode)
  }

  def stringLiteralNode(string: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    val literalNode = nodes
      .NewLiteral()
      .code(string)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(literalNode)
  }

  def blockNode(code: String, lineAndColumn: LineAndColumn): nodes.NewBlock = {
    val blockNode = nodes
      .NewBlock()
      .code(code)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(blockNode)
  }

  def controlStructureNode(
      code: String,
      parserTypeName: String,
      lineAndColumn: LineAndColumn
  ): nodes.NewControlStructure = {
    val controlStructureNode = nodes
      .NewControlStructure()
      .code(code)
      .parserTypeName(parserTypeName)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(controlStructureNode)
  }

  def localNode(name: String, closureBindingId: Option[String] = None): nodes.NewLocal = {
    val localNode = nodes
      .NewLocal()
      .code(name)
      .name(name)
      .closureBindingId(closureBindingId)
    addNodeToDiff(localNode)
  }

  def fileNode(fileName: String): nodes.NewFile = {
    val fileNode = nodes
      .NewFile()
      .name(fileName)
    addNodeToDiff(fileNode)
  }

  def namespaceBlockNode(fileName: String): nodes.NewNamespaceBlock = {
    val namespaceBlockNode = nodes
      .NewNamespaceBlock()
      .name("<global>")
      .fullName(fileName + ":" + "<global>")
      .filename(fileName)
    addNodeToDiff(namespaceBlockNode)
  }

  def modifierNode(modifierType: String): nodes.NewModifier = {
    val modifierNode = nodes
      .NewModifier()
      .modifierType(modifierType)
    addNodeToDiff(modifierNode)
  }

  def metaNode(language: String, version: String): nodes.NewMetaData = {
    val metaNode = nodes
      .NewMetaData()
      .language(language)
      .version(version)
    addNodeToDiff(metaNode)
  }

  def unknownNode(
      code: String,
      parserTypeName: String,
      lineAndColumn: LineAndColumn
  ): nodes.NewUnknown = {
    val unknownNode = nodes
      .NewUnknown()
      .code(code)
      .parserTypeName(parserTypeName)
      .lineNumber(Some(lineAndColumn.line))
      .columnNumber(Some(lineAndColumn.column))
    addNodeToDiff(unknownNode)
  }
}
