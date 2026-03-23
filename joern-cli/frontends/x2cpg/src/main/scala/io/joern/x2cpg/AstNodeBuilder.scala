package io.joern.x2cpg

import io.joern.x2cpg.AstNodeBuilder.methodReturnNodeWithExplicitPositionInfo
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, PropertyDefaults}
import io.shiftleft.codepropertygraph.generated.nodes.*
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
  private lazy val maxCodeLength: Int =
    sys.env.get("JOERN_MAX_CODE_LENGTH").flatMap(_.toIntOption).getOrElse(DefaultMaxCodeLength)

  private def setOffset[T <: AstNodeNew](node: Node, astNode: T): T = {
    offset(node).foreach { case (offset, offsetEnd) =>
      astNode.offset(offset).offsetEnd(offsetEnd)
    }

    astNode
  }

  protected def code(node: Node): String

  /** Abbreviates a code snippet to a bounded length suitable for CPG node `code` fields.
    *
    * The default maximum is derived from the `JOERN_MAX_CODE_LENGTH` environment variable (falls back to 1000). The
    * width passed to `StringUtils.abbreviate` is clamped to a minimum of 50 characters to avoid over‑aggressive
    * truncation.
    *
    * If either the requested maximum or the input length is smaller than 4, the original string is returned unchanged
    * to prevent `IllegalArgumentException` from `StringUtils.abbreviate`.
    *
    * @param code
    *   the original code snippet
    * @param maxCodeLength
    *   desired maximum length; defaults to `maxCodeLength`
    * @return
    *   the possibly abbreviated code string
    */
  protected def shortenCode(code: String, maxCodeLength: Int = maxCodeLength): String = {
    // check this; otherwise, StringUtils.abbreviate throws an IllegalArgumentException
    if (maxCodeLength < 4 || code.length < 4) {
      code
    } else {
      StringUtils.abbreviate(code, math.max(MinCodeLength, maxCodeLength))
    }
  }

  protected def offset(node: Node): Option[(Int, Int)] = None

  protected def unknownNode(node: Node, code: String): NewUnknown = {
    val node_ = NewUnknown()
      .parserTypeName(Try(node.getClass.getSimpleName).toOption.getOrElse(Defines.Unknown))
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def annotationNode(node: Node, code: String, name: String, fullName: String): NewAnnotation = {
    val node_ = NewAnnotation()
      .code(code)
      .name(name)
      .fullName(fullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def annotationLiteralNode(node: Node, name: String): NewAnnotationLiteral = {
    val node_ = NewAnnotationLiteral()
      .name(name)
      .code(name)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def commentNode(node: Node, code: String, filename: String): NewComment = {
    val node_ = NewComment().code(code).filename(filename).lineNumber(line(node)).columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def methodRefNode(node: Node, code: String, methodFullName: String, typeFullName: String): NewMethodRef = {
    val node_ = NewMethodRef()
      .code(code)
      .methodFullName(methodFullName)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
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
    val node_ = NewMember()
      .code(code)
      .name(name)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHints)
      .lineNumber(line(node))
      .columnNumber(column(node))
    genericSignature.foreach(node_.genericSignature(_))
    setOffset(node, node_)
  }

  protected def newImportNode(code: String, importedEntity: String, importedAs: String, include: Node): NewImport = {
    val node_ = NewImport()
      .code(code)
      .importedEntity(importedEntity)
      .importedAs(importedAs)
      .lineNumber(line(include))
      .columnNumber(column(include))
    setOffset(include, node_)
  }

  protected def literalNode(
    node: Node,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq()
  ): NewLiteral = {
    val node_ = NewLiteral()
      .code(code)
      .typeFullName(typeFullName)
      .dynamicTypeHintFullName(dynamicTypeHints)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def typeRefNode(node: Node, code: String, typeFullName: String): NewTypeRef = {
    val node_ = NewTypeRef()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
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
    genericSignature.foreach(node_.genericSignature(_))
    setOffset(node, node_)
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
    typeFullName: Option[String] = None,
    dynamicTypeHintFullName: Seq[String] = Nil
  ): NewMethodParameterIn = {
    val node_ = NewMethodParameterIn()
      .name(name)
      .code(code)
      .index(index)
      .order(index)
      .isVariadic(isVariadic)
      .evaluationStrategy(evaluationStrategy)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .typeFullName(typeFullName.getOrElse("ANY"))
      .dynamicTypeHintFullName(dynamicTypeHintFullName)
    setOffset(node, node_)
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
    typeFullName: Option[String],
    staticReceiver: Option[String] = None
  ): NewCall = {
    val node_ =
      NewCall()
        .code(code)
        .name(name)
        .methodFullName(methodFullName)
        .dispatchType(dispatchType)
        .staticReceiver(staticReceiver)
        .lineNumber(line(node))
        .columnNumber(column(node))
        .typeFullName(typeFullName.getOrElse(Defines.Any))
    signature.foreach { s => node_.signature(s) }
    typeFullName.foreach { t => node_.typeFullName(t) }
    setOffset(node, node_)
  }

  protected def operatorCallNode(node: Node, name: String, typeFullName: Option[String]): NewCall = {
    callNode(node, code(node), name, name, DispatchTypes.STATIC_DISPATCH, Option(""), typeFullName)
  }

  protected def operatorCallNode(node: Node, code: String, name: String, typeFullName: Option[String]): NewCall = {
    val typeOrAny = typeFullName.getOrElse("ANY")
    callNode(node, code, name, name, DispatchTypes.STATIC_DISPATCH, Option(""), Option(typeOrAny))
  }

  protected def returnNode(node: Node, code: String): NewReturn = {
    val node_ = NewReturn()
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def controlStructureNode(node: Node, controlStructureType: String, code: String): NewControlStructure = {
    val node_ = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def blockNode(node: Node): NewBlock = {
    blockNode(node, PropertyDefaults.Code, Defines.Any)
  }

  protected def blockNode(node: Node, code: String, typeFullName: String): NewBlock = {
    val node_ = NewBlock()
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def fieldIdentifierNode(node: Node, name: String, code: String): NewFieldIdentifier = {
    val node_ = NewFieldIdentifier()
      .canonicalName(name)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def localNode(
    node: Node,
    name: String,
    code: String,
    typeFullName: String,
    closureBindingId: Option[String] = None,
    genericSignature: Option[String] = None
  ): NewLocal = {
    val node_ = AstNodeBuilder.localNodeWithExplicitPositionInfo(
      name,
      code,
      typeFullName,
      closureBindingId,
      genericSignature,
      line(node),
      column(node)
    )
    setOffset(node, node_)
  }

  protected def identifierNode(
    node: Node,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq()
  ): NewIdentifier = {
    val node_ = NewIdentifier()
      .name(name)
      .typeFullName(typeFullName)
      .code(code)
      .dynamicTypeHintFullName(dynamicTypeHints)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
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
    setOffset(node, node_)
  }

  protected def methodReturnNode(
    node: Node,
    typeFullName: String,
    dynamicTypeHintFullName: Option[String] = None
  ): NewMethodReturn = {
    val node_ =
      methodReturnNodeWithExplicitPositionInfo(typeFullName, dynamicTypeHintFullName, line(node), column(node))
    setOffset(node, node_)
  }

  protected def jumpTargetNode(
    node: Node,
    name: String,
    code: String,
    parserTypeName: Option[String] = None
  ): NewJumpTarget = {
    val node_ = NewJumpTarget()
      .parserTypeName(parserTypeName.getOrElse(node.getClass.getSimpleName))
      .name(name)
      .code(code)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def namespaceBlockNode(node: Node, name: String, fullName: String, fileName: String): NewNamespaceBlock = {
    val node_ = NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(fileName)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
  }

  protected def modifierNode(node: Node, modifierType: String): NewModifier = {
    val node_ = NewModifier()
      .modifierType(modifierType)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, node_)
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

  private[joern] def bindingNode(name: String, signature: String, methodFullName: String): NewBinding = {
    NewBinding()
      .name(name)
      .methodFullName(methodFullName)
      .signature(signature)
  }

  private[joern] def closureBindingNode(closureBindingId: String, evaluationStrategy: String): NewClosureBinding = {
    NewClosureBinding().closureBindingId(closureBindingId).evaluationStrategy(evaluationStrategy)
  }

  private[joern] def dependencyNode(name: String, groupId: String, version: String): NewDependency = {
    NewDependency()
      .name(name)
      .dependencyGroupId(groupId)
      .version(version)
  }
}
