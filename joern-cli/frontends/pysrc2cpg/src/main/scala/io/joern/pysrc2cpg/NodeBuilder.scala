package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.PythonAstVisitor.{allBuiltinClasses, typingClassesV3, typingPrefix}
import io.joern.x2cpg.frontendspecific.pysrc2cpg.Constants.builtinPrefix
import io.joern.pythonparser.ast
import io.joern.x2cpg.Defines
import io.joern.x2cpg.frontendspecific.pysrc2cpg.Constants
import io.joern.x2cpg.utils.NodeBuilders
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, nodes}
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

class NodeBuilder(diffGraph: DiffGraphBuilder) {

  private def addNodeToDiff[T <: nodes.NewNode](node: T): T = {
    diffGraph.addNode(node)
    node
  }

  def callNode(code: String, name: String, dispatchType: String, lineAndColumn: LineAndColumn): nodes.NewCall = {
    val callNode = nodes
      .NewCall()
      .code(code)
      .name(name)
      .methodFullName(if (dispatchType == DispatchTypes.STATIC_DISPATCH) name else Defines.DynamicCallUnknownFullName)
      .dispatchType(dispatchType)
      .typeFullName(Constants.ANY)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
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

  def typeDeclNode(
    name: String,
    fullName: String,
    fileName: String,
    inheritsFromFullNames: collection.Seq[String],
    lineAndColumn: LineAndColumn
  ): nodes.NewTypeDecl = {
    val typeDeclNode = nodes
      .NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .isExternal(false)
      .filename(fileName)
      .inheritsFromTypeFullName(inheritsFromFullNames)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
      .offset(lineAndColumn.offset)
      .offsetEnd(lineAndColumn.endOffset)

    addNodeToDiff(typeDeclNode)
  }

  def typeRefNode(code: String, typeFullName: String, lineAndColumn: LineAndColumn): nodes.NewTypeRef = {
    val typeRefNode = nodes
      .NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(typeRefNode)
  }

  def memberNode(name: String): nodes.NewMember = {
    val memberNode = nodes
      .NewMember()
      .code(name)
      .name(name)
      .typeFullName(Constants.ANY)
    addNodeToDiff(memberNode)
  }

  def memberNode(name: String, lineAndColumn: LineAndColumn): nodes.NewMember = {
    memberNode(name)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
  }

  def memberNode(name: String, dynamicTypeHintFullName: String): nodes.NewMember =
    memberNode(name).dynamicTypeHintFullName(dynamicTypeHintFullName :: Nil)
  def memberNode(name: String, dynamicTypeHintFullName: String, lineAndColumn: LineAndColumn): nodes.NewMember =
    memberNode(name, lineAndColumn).dynamicTypeHintFullName(dynamicTypeHintFullName :: Nil)

  def bindingNode(): nodes.NewBinding = {
    val bindingNode = nodes
      .NewBinding()
      .name("")
      .signature("")

    addNodeToDiff(bindingNode)
  }

  def methodNode(name: String, fullName: String, fileName: String, lineAndColumn: LineAndColumn): nodes.NewMethod = {
    val methodNode = nodes
      .NewMethod()
      .name(name)
      .fullName(fullName)
      .filename(fileName)
      .isExternal(false)
      .lineNumber(lineAndColumn.line)
      .lineNumberEnd(lineAndColumn.endLine)
      .columnNumber(lineAndColumn.column)
      .columnNumberEnd(lineAndColumn.endColumn)
      .offset(lineAndColumn.offset)
      .offsetEnd(lineAndColumn.endOffset)
    addNodeToDiff(methodNode)
  }

  def methodRefNode(name: String, fullName: String, lineAndColumn: LineAndColumn): nodes.NewMethodRef = {
    val methodRefNode = nodes
      .NewMethodRef()
      .code(name)
      .methodFullName(fullName)
      .typeFullName(fullName)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(methodRefNode)
  }

  def closureBindingNode(closureBindingId: String, closureOriginalName: String): nodes.NewClosureBinding = {
    val closureBindingNode = nodes
      .NewClosureBinding()
      .closureBindingId(Some(closureBindingId))
      .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
      .closureOriginalName(Some(closureOriginalName))
    addNodeToDiff(closureBindingNode)
  }

  def methodParameterNode(
    name: String,
    isVariadic: Boolean,
    lineAndColumn: LineAndColumn,
    index: Option[Int] = None,
    typeHint: Option[ast.iexpr] = None
  ): nodes.NewMethodParameterIn = {
    val methodParameterNode = nodes
      .NewMethodParameterIn()
      .name(name)
      .code(name)
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)
      .typeFullName(extractTypesFromHint(typeHint).getOrElse(Constants.ANY))
      .isVariadic(isVariadic)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    index.foreach(idx => methodParameterNode.index(idx))
    addNodeToDiff(methodParameterNode)
  }

  def extractTypesFromHint(typeHint: Option[ast.iexpr] = None): Option[String] = {
    typeHint match {
      case Some(hint) =>
        val nameSequence = hint match {
          case n: ast.Name => Option(n.id)
          // TODO: Definitely a place for follow up handling of generics - currently only take the polymorphic type
          //  without type args. To see the type arguments, see ast.Subscript.slice
          case attr: ast.Attribute =>
            extractTypesFromHint(Some(attr.value)).map { x => x + "." + attr.attr }
          case n: ast.Subscript if n.value.isInstanceOf[ast.Name] => Option(n.value.asInstanceOf[ast.Name].id)
          case n: ast.Constant if n.value.isInstanceOf[ast.StringConstant] =>
            Option(n.value.asInstanceOf[ast.StringConstant].value)
          case _ => None
        }
        nameSequence.map { typeName =>
          if (allBuiltinClasses.contains(typeName)) s"$builtinPrefix$typeName"
          else if (typingClassesV3.contains(typeName)) s"$typingPrefix$typeName"
          else typeName
        }
      case _ => None
    }
  }

  def methodReturnNode(
    staticTypeHint: Option[String],
    dynamicTypeHintFullName: Option[String],
    lineAndColumn: LineAndColumn
  ): nodes.NewMethodReturn = {
    val methodReturnNode = NodeBuilders
      .newMethodReturnNode(
        staticTypeHint.getOrElse(Constants.ANY),
        dynamicTypeHintFullName,
        Some(lineAndColumn.line),
        Some(lineAndColumn.column)
      )
      .evaluationStrategy(EvaluationStrategies.BY_SHARING)

    addNodeToDiff(methodReturnNode)
  }

  def returnNode(code: String, lineAndColumn: LineAndColumn): nodes.NewReturn = {
    val returnNode = nodes
      .NewReturn()
      .code(code)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)

    addNodeToDiff(returnNode)
  }

  def identifierNode(name: String, lineAndColumn: LineAndColumn): nodes.NewIdentifier = {
    val identifierNode = nodes
      .NewIdentifier()
      .code(name)
      .name(name)
      .typeFullName(Constants.ANY)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(identifierNode)
  }

  def fieldIdentifierNode(name: String, lineAndColumn: LineAndColumn): nodes.NewFieldIdentifier = {
    val fieldIdentifierNode = nodes
      .NewFieldIdentifier()
      .code(name)
      .canonicalName(name)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(fieldIdentifierNode)
  }

  def literalNode(string: String, dynamicTypeHint: Option[String], lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    val literalNode = nodes
      .NewLiteral()
      .code(string)
      .typeFullName(Constants.ANY)
      .dynamicTypeHintFullName(dynamicTypeHint.toList)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(literalNode)
  }

  def stringLiteralNode(string: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    literalNode(string, Some(Constants.builtinStrType), lineAndColumn)
  }

  def bytesLiteralNode(string: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    literalNode(string, Some(Constants.builtinBytesType), lineAndColumn)
  }

  def intLiteralNode(string: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    literalNode(string, Some(Constants.builtinIntType), lineAndColumn)
  }

  def floatLiteralNode(string: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    literalNode(string, Some(Constants.builtinFloatType), lineAndColumn)
  }

  def complexLiteralNode(string: String, lineAndColumn: LineAndColumn): nodes.NewLiteral = {
    literalNode(string, Some(Constants.builtinComplexType), lineAndColumn)
  }

  def blockNode(code: String, lineAndColumn: LineAndColumn): nodes.NewBlock = {
    val blockNode = nodes
      .NewBlock()
      .code(code)
      .typeFullName(Constants.ANY)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(blockNode)
  }

  def controlStructureNode(
    code: String,
    controlStructureName: String,
    lineAndColumn: LineAndColumn
  ): nodes.NewControlStructure = {
    val controlStructureNode = nodes
      .NewControlStructure()
      .code(code)
      .controlStructureType(controlStructureName)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(controlStructureNode)
  }

  def localNode(name: String, closureBindingId: Option[String] = None): nodes.NewLocal = {
    val localNode = nodes
      .NewLocal()
      .code(name)
      .name(name)
      .closureBindingId(closureBindingId)
      .typeFullName(Constants.ANY)
    addNodeToDiff(localNode)
  }

  def fileNode(fileName: String, content: Option[String]): nodes.NewFile = {
    val fileNode = nodes
      .NewFile()
      .name(fileName)

    content.foreach(fileNode.content(_))
    addNodeToDiff(fileNode)
  }

  def namespaceBlockNode(name: String, fullName: String, fileName: String): nodes.NewNamespaceBlock = {
    val namespaceBlockNode = nodes
      .NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
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

  def unknownNode(code: String, parserTypeName: String, lineAndColumn: LineAndColumn): nodes.NewUnknown = {
    val unknownNode = nodes
      .NewUnknown()
      .code(code)
      .parserTypeName(parserTypeName)
      .typeFullName(Constants.ANY)
      .lineNumber(lineAndColumn.line)
      .columnNumber(lineAndColumn.column)
    addNodeToDiff(unknownNode)
  }
}
