package io.joern.x2cpg

import io.joern.x2cpg.AstNodeBuilder.methodReturnNodeWithExplicitPositionInfo
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.Block.PropertyDefaults as BlockDefaults
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewAnnotation,
  NewAnnotationLiteral,
  NewBinding,
  NewBlock,
  NewCall,
  NewControlStructure,
  NewFieldIdentifier,
  NewIdentifier,
  NewImport,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewMethodRef,
  NewMethodReturn,
  NewModifier,
  NewNamespaceBlock,
  NewReturn,
  NewTypeDecl,
  NewTypeRef,
  NewUnknown
}
import org.apache.commons.lang3.StringUtils

import scala.util.Try
trait AstNodeBuilder[Node, NodeProcessor] { this: NodeProcessor =>
  protected def line(node: Node): Option[Int]
  protected def column(node: Node): Option[Int]
  protected def lineEnd(node: Node): Option[Int]
  protected def columnEnd(element: Node): Option[Int]

  private val MinCodeLength: Int        = 50
  private val DefaultMaxCodeLength: Int = 1000
  // maximum length of code fields in number of characters
  private lazy val MaxCodeLength: Int =
    sys.env.get("JOERN_MAX_CODE_LENGTH").flatMap(_.toIntOption).getOrElse(DefaultMaxCodeLength)

  protected def code(node: Node): String

  protected def shortenCode(code: String): String =
    StringUtils.abbreviate(code, math.max(MinCodeLength, MaxCodeLength))

  protected def offset(node: Node): Option[(Int, Int)] = None

  protected def unknownNode(node: Node, code: String): NewUnknown = {
    NewUnknown()
      .parserTypeName(Try(node.getClass.getSimpleName).toOption.getOrElse(Defines.Unknown))
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def annotationNode(node: Node, code: String, name: String, fullName: String): NewAnnotation = {
    NewAnnotation()
      .code(code)
      .name(name)
      .fullName(fullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def annotationLiteralNode(node: Node, name: String): NewAnnotationLiteral = {
    NewAnnotationLiteral()
      .name(name)
      .code(name)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def methodRefNode(node: Node, code: String, methodFullName: String, typeFullName: String): NewMethodRef = {
    NewMethodRef()
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def memberNode(node: Node, name: String, code: String, typeFullName: String): NewMember =
    memberNode(node, name, code, typeFullName, Seq())

  protected def memberNode(
    node: Node,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq(),
    genericSignature: Option[String] = None
  ): NewMember = {
    val member = NewMember()
      .code(code)
      .name(name)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHints)
      .lineNumber(line(node))
      .columnNumber(column(node))
    genericSignature.foreach(member.genericSignature(_))
    member
  }

  protected def newImportNode(code: String, importedEntity: String, importedAs: String, include: Node): NewImport = {
    NewImport()
      .code(code)
      .importedEntity(importedEntity)
      .importedAs(importedAs)
      .lineNumber(line(include))
      .columnNumber(column(include))
  }

  protected def literalNode(
    node: Node,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq()
  ): NewLiteral = {
    NewLiteral()
      .code(code)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHints)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def typeRefNode(node: Node, code: String, typeFullName: String): NewTypeRef = {
    NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  def typeDeclNode(
    node: Node,
    name: String,
    fullName: String,
    fileName: String,
    inheritsFrom: Seq[String],
    alias: Option[String]
  ): NewTypeDecl =
    typeDeclNode(node, name, fullName, fileName, name, "", "", inheritsFrom, alias)

  protected def typeDeclNode(
    node: Node,
    name: String,
    fullName: String,
    filename: String,
    code: String,
    astParentType: String = "",
    astParentFullName: String = "",
    inherits: Seq[String] = Seq.empty,
    alias: Option[String] = None,
    genericSignature: Option[String] = None
  ): NewTypeDecl = {
    val node_ = NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .inheritsFromTypeFullName(inherits)
      .aliasTypeFullName(alias)
      .lineNumber(line(node))
      .columnNumber(column(node))
    offset(node).foreach { case (offset, offsetEnd) =>
      node_.offset(offset).offsetEnd(offsetEnd)
    }
    genericSignature.foreach(node_.genericSignature(_))
    node_
  }

  protected def parameterInNode(
    node: Node,
    name: String,
    code: String,
    index: Int,
    isVariadic: Boolean,
    evaluationStrategy: String,
    typeFullName: String
  ): NewMethodParameterIn =
    parameterInNode(node, name, code, index, isVariadic, evaluationStrategy, Option(typeFullName))

  protected def parameterInNode(
    node: Node,
    name: String,
    code: String,
    index: Int,
    isVariadic: Boolean,
    evaluationStrategy: String,
    typeFullName: Option[String] = None
  ): NewMethodParameterIn = {
    NewMethodParameterIn()
      .name(name)
      .code(code)
      .index(index)
      .order(index)
      .isVariadic(isVariadic)
      .evaluationStrategy(evaluationStrategy)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .typeFullName(typeFullName.getOrElse("ANY"))
  }

  def callNode(node: Node, code: String, name: String, methodFullName: String, dispatchType: String): NewCall =
    callNode(node, code, name, methodFullName, dispatchType, None, None)

  def callNode(
    node: Node,
    code: String,
    name: String,
    methodFullName: String,
    dispatchType: String,
    signature: Option[String],
    typeFullName: Option[String]
  ): NewCall = {
    val out =
      NewCall()
        .code(code)
        .name(name)
        .methodFullName(methodFullName)
        .dispatchType(dispatchType)
        .lineNumber(line(node))
        .columnNumber(column(node))
    signature.foreach { s => out.signature(s) }
    typeFullName.foreach { t => out.typeFullName(t) }
    out
  }

  protected def operatorCallNode(node: Node, name: String, typeFullName: Option[String]): NewCall = {
    callNode(node, code(node), name, name, DispatchTypes.STATIC_DISPATCH, Option(""), typeFullName)
  }

  protected def operatorCallNode(node: Node, code: String, name: String, typeFullName: Option[String]): NewCall = {
    val typeOrAny = typeFullName.getOrElse("ANY")
    callNode(node, code, name, name, DispatchTypes.STATIC_DISPATCH, Option(""), Option(typeOrAny))
  }

  protected def returnNode(node: Node, code: String): NewReturn = {
    NewReturn()
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def controlStructureNode(node: Node, controlStructureType: String, code: String): NewControlStructure = {
    NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def blockNode(node: Node): NewBlock = {
    blockNode(node, BlockDefaults.Code, Defines.Any)
  }

  protected def blockNode(node: Node, code: String, typeFullName: String): NewBlock = {
    NewBlock()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def fieldIdentifierNode(node: Node, name: String, code: String): NewFieldIdentifier = {
    NewFieldIdentifier()
      .canonicalName(name)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def localNode(
    node: Node,
    name: String,
    code: String,
    typeFullName: String,
    closureBindingId: Option[String] = None,
    genericSignature: Option[String] = None
  ): NewLocal = {
    val (nodeOffset, nodeOffsetEnd) =
      offset(node).map((offset, offsetEnd) => (Option(offset), Option(offsetEnd))).getOrElse((None, None))
    AstNodeBuilder.localNodeWithExplicitPositionInfo(
      name,
      code,
      typeFullName,
      closureBindingId,
      genericSignature,
      line(node),
      column(node),
      nodeOffset,
      nodeOffsetEnd
    )
  }

  protected def identifierNode(
    node: Node,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq()
  ): NewIdentifier = {
    NewIdentifier()
      .name(name)
      .typeFullName(typeFullName)
      .code(code)
      .dynamicTypeHintFullName(dynamicTypeHints)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  def methodNode(node: Node, name: String, fullName: String, signature: String, fileName: String): NewMethod = {
    methodNode(node, name, name, fullName, Option(signature), fileName)
  }

  protected def methodNode(
    node: Node,
    name: String,
    code: String,
    fullName: String,
    signature: Option[String],
    fileName: String,
    astParentType: Option[String] = None,
    astParentFullName: Option[String] = None,
    genericSignature: Option[String] = None
  ): NewMethod = {
    val node_ =
      NewMethod()
        .name(StringUtils.normalizeSpace(name))
        .code(code)
        .fullName(StringUtils.normalizeSpace(fullName))
        .filename(fileName)
        .astParentType(astParentType.getOrElse("<empty>"))
        .astParentFullName(astParentFullName.getOrElse("<empty>"))
        .isExternal(false)
        .lineNumber(line(node))
        .columnNumber(column(node))
        .lineNumberEnd(lineEnd(node))
        .columnNumberEnd(columnEnd(node))
    signature.foreach { s => node_.signature(StringUtils.normalizeSpace(s)) }
    genericSignature.foreach(node_.genericSignature(_))
    offset(node).foreach { case (offset, offsetEnd) =>
      node_.offset(offset).offsetEnd(offsetEnd)
    }
    node_
  }

  protected def methodReturnNode(
    node: Node,
    typeFullName: String,
    dynamicTypeHintFullName: Option[String] = None
  ): NewMethodReturn = {
    // TODO: Add offsets
    methodReturnNodeWithExplicitPositionInfo(typeFullName, dynamicTypeHintFullName, line(node), column(node))
  }

  protected def jumpTargetNode(
    node: Node,
    name: String,
    code: String,
    parserTypeName: Option[String] = None
  ): NewJumpTarget = {
    NewJumpTarget()
      .parserTypeName(parserTypeName.getOrElse(node.getClass.getSimpleName))
      .name(name)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }

  protected def namespaceBlockNode(node: Node, name: String, fullName: String, fileName: String): NewNamespaceBlock = {
    NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(fileName)
      .lineNumber(line(node))
      .columnNumber(column(node))
  }
}

/** It is sometimes necessary to create nodes without an origin node to use as a reference for positional information
  * (for example in passes after AST creation). These methods provide a way to do that, but the AstNodeBuilder trait
  * methods are STRONGLY preferred and should be used instead of these static methods whenever possible.
  */
object AstNodeBuilder {

  private[joern] def localNodeWithExplicitPositionInfo(
    name: String,
    code: String,
    typeFullName: String,
    closureBindingId: Option[String] = None,
    genericSignature: Option[String] = None,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    offset: Option[Int] = None,
    offsetEnd: Option[Int] = None
  ): NewLocal = {
    val node_ = NewLocal()
      .name(name)
      .code(code)
      .typeFullName(typeFullName)
      .closureBindingId(closureBindingId)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .offset(offset)
      .offsetEnd(offsetEnd)
    genericSignature.foreach(node_.genericSignature(_))
    node_
  }

  private[joern] def methodReturnNodeWithExplicitPositionInfo(
    typeFullName: String,
    dynamicTypeHintFullName: Option[String] = None,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None,
    offset: Option[Int] = None,
    offsetEnd: Option[Int] = None
  ): NewMethodReturn = {
    NewMethodReturn()
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHintFullName)
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .offset(offset)
      .offsetEnd(offsetEnd)
  }
}
