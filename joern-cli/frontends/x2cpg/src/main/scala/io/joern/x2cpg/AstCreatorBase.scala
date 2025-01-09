package io.joern.x2cpg

import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.IntervalKeyPool
import io.joern.x2cpg.utils.NodeBuilders.{newFieldIdentifierNode, newMethodReturnNode, newOperatorCallNode}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Cpg, DiffGraphBuilder, ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

abstract class AstCreatorBase(filename: String)(implicit withSchemaValidation: ValidationMode) {
  val diffGraph: DiffGraphBuilder = Cpg.newDiffGraphBuilder

  private val closureKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  def createAst(): DiffGraphBuilder

  /** Create a global namespace block for the given `filename`
    */
  def globalNamespaceBlock(): NewNamespaceBlock = {
    val name     = NamespaceTraversal.globalNamespaceName
    val fullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))
    NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(filename)
      .order(1)
  }

  /** Creates an AST that represents an annotation, including its content (annotation parameter assignments).
    */
  def annotationAst(annotation: NewAnnotation, children: Seq[Ast]): Ast = {
    val annotationAst = Ast(annotation)
    annotationAst.withChildren(children)
  }

  /** Creates an AST that represents an annotation assignment with a name for the assigned value, its overall code, and
    * the respective assignment AST.
    */
  def annotationAssignmentAst(assignmentValueName: String, code: String, assignmentAst: Ast): Ast = {
    val parameter      = NewAnnotationParameter().code(assignmentValueName)
    val assign         = NewAnnotationParameterAssign().code(code)
    val assignChildren = List(Ast(parameter), assignmentAst)
    setArgumentIndices(assignChildren)
    Ast(assign)
      .withChild(Ast(parameter))
      .withChild(assignmentAst)
  }

  /** Creates an AST that represents an entire method, including its content.
    */
  def methodAst(
    method: NewMethod,
    parameters: Seq[Ast],
    body: Ast,
    methodReturn: NewMethodReturn,
    modifiers: Seq[NewModifier] = Nil
  ): Ast =
    methodAstWithAnnotations(method, parameters, body, methodReturn, modifiers, annotations = Nil)

  /** Creates an AST that represents an entire method, including its content and with support for both method and
    * parameter annotations.
    */
  def methodAstWithAnnotations(
    method: NewMethod,
    parameters: Seq[Ast],
    body: Ast,
    methodReturn: NewMethodReturn,
    modifiers: Seq[NewModifier] = Nil,
    annotations: Seq[Ast] = Nil
  ): Ast =
    Ast(method)
      .withChildren(parameters)
      .withChild(body)
      .withChildren(modifiers.map(Ast(_)))
      .withChildren(annotations)
      .withChild(Ast(methodReturn))

  /** Creates an AST that represents a method stub, containing information about the method, its parameters, and the
    * return type.
    */
  def methodStubAst(
    method: NewMethod,
    parameters: Seq[Ast],
    methodReturn: NewMethodReturn,
    modifiers: Seq[NewModifier] = Nil
  ): Ast =
    Ast(method)
      .withChildren(parameters)
      .withChild(Ast(NewBlock().typeFullName(Defines.Any)))
      .withChildren(modifiers.map(Ast(_)))
      .withChild(Ast(methodReturn))

  def staticInitMethodAst(
    initAsts: List[Ast],
    fullName: String,
    signature: Option[String],
    returnType: String,
    fileName: Option[String] = None,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ): Ast = {
    val methodNode = NewMethod()
      .name(Defines.StaticInitMethodName)
      .fullName(fullName)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
    if (signature.isDefined) {
      methodNode.signature(signature.get)
    }
    if (fileName.isDefined) {
      methodNode.filename(fileName.get)
    }
    val staticModifier = NewModifier().modifierType(ModifierTypes.STATIC)
    val body           = blockAst(NewBlock().typeFullName(Defines.Any), initAsts)
    val methodReturn   = newMethodReturnNode(returnType, None, None, None)
    methodAst(methodNode, Nil, body, methodReturn, List(staticModifier))
  }

  /** For a given return node and arguments, create an AST that represents the return instruction. The main purpose of
    * this method is to automatically assign the correct argument indices.
    */
  def returnAst(returnNode: NewReturn, arguments: Seq[Ast] = List()): Ast = {
    setArgumentIndices(arguments)
    Ast(returnNode)
      .withChildren(arguments)
      .withArgEdges(returnNode, arguments.flatMap(_.root))
  }

  /** For a given node, condition AST and children ASTs, create an AST that represents the control structure. The main
    * purpose of this method is to automatically assign the correct condition edges.
    */
  def controlStructureAst(
    controlStructureNode: NewControlStructure,
    condition: Option[Ast],
    children: Seq[Ast] = Seq(),
    placeConditionLast: Boolean = false
  ): Ast = {
    condition match {
      case Some(conditionAst) =>
        Ast(controlStructureNode)
          .withChildren(if (placeConditionLast) children :+ conditionAst else conditionAst +: children)
          .withConditionEdges(controlStructureNode, List(conditionAst.root).flatten)
      case _ =>
        Ast(controlStructureNode)
          .withChildren(children)
    }
  }

  def wrapMultipleInBlock(asts: Seq[Ast], lineNumber: Option[Int]): Ast = {
    asts.toList match {
      case Nil        => blockAst(NewBlock().typeFullName(Defines.Any).lineNumber(lineNumber))
      case ast :: Nil => ast
      case astList    => blockAst(NewBlock().typeFullName(Defines.Any).lineNumber(lineNumber), astList)
    }
  }

  def whileAst(
    condition: Option[Ast],
    body: Seq[Ast],
    code: Option[String] = None,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ): Ast = {
    var whileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.WHILE)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
    if (code.isDefined) {
      whileNode = whileNode.code(code.get)
    }
    controlStructureAst(whileNode, condition, body)
  }

  def doWhileAst(
    condition: Option[Ast],
    body: Seq[Ast],
    code: Option[String] = None,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ): Ast = {
    var doWhileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.DO)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
    if (code.isDefined) {
      doWhileNode = doWhileNode.code(code.get)
    }
    controlStructureAst(doWhileNode, condition, body, placeConditionLast = true)
  }

  def forAst(
    forNode: NewControlStructure,
    locals: Seq[Ast],
    initAsts: Seq[Ast],
    conditionAsts: Seq[Ast],
    updateAsts: Seq[Ast],
    bodyAst: Ast
  ): Ast =
    forAst(forNode, locals, initAsts, conditionAsts, updateAsts, Seq(bodyAst))

  private def setOrderExplicitly(ast: Ast, order: Int): Ast = {
    ast.root match {
      case Some(value: ExpressionNew) => value.order(order); ast
      case _                          => ast
    }
  }

  def forAst(
    forNode: NewControlStructure,
    locals: Seq[Ast],
    initAsts: Seq[Ast],
    conditionAsts: Seq[Ast],
    updateAsts: Seq[Ast],
    bodyAsts: Seq[Ast]
  ): Ast = {
    val lineNumber  = forNode.lineNumber
    val numOfLocals = locals.size
    // for the expected orders see CfgCreator.cfgForForStatement
    if (bodyAsts.nonEmpty) setOrderExplicitly(bodyAsts.head, numOfLocals + 4)
    Ast(forNode)
      .withChildren(locals)
      .withChild(setOrderExplicitly(wrapMultipleInBlock(initAsts, lineNumber), numOfLocals + 1))
      .withChild(setOrderExplicitly(wrapMultipleInBlock(conditionAsts, lineNumber), numOfLocals + 2))
      .withChild(setOrderExplicitly(wrapMultipleInBlock(updateAsts, lineNumber), numOfLocals + 3))
      .withChildren(bodyAsts)
      .withConditionEdges(forNode, conditionAsts.flatMap(_.root).toList)
  }

  /** For the given try body, catch ASTs and finally AST, create a try-catch-finally AST with orders set correctly for
    * the ossdataflow engine.
    */
  @deprecated(
    "This will be removed once all frontends switched to `tryCatchAst` using ControlStructure nodes for catches/finally. Use `tryCatchAst` instead."
  )
  def tryCatchAstWithOrder(
    tryNode: NewControlStructure,
    tryBodyAst: Ast,
    catchAsts: Seq[Ast],
    finallyAst: Option[Ast]
  ): Ast = {
    tryBodyAst.root.collect { case x: ExpressionNew => x }.foreach(_.order = 1)
    catchAsts.flatMap(_.root).collect { case x: ExpressionNew => x }.foreach(_.order = 2)
    finallyAst.flatMap(_.root).collect { case x: ExpressionNew => x }.foreach(_.order = 3)
    Ast(tryNode)
      .withChild(tryBodyAst)
      .withChildren(catchAsts)
      .withChildren(finallyAst.toList)
  }

  /** For the given try body, catch ASTs, and finally AST, create a try-catch-finally AST.
    */
  def tryCatchAst(tryNode: NewControlStructure, tryBodyAst: Ast, catchAsts: Seq[Ast], finallyAst: Option[Ast]): Ast = {
    setArgumentIndices(tryBodyAst +: (catchAsts ++ finallyAst.toSeq))
    Ast(tryNode)
      .withChild(tryBodyAst)
      .withChildren(catchAsts)
      .withChildren(finallyAst.toSeq)
  }

  /** For a given block node and statement ASTs, create an AST that represents the block. The main purpose of this
    * method is to increase the readability of the code which creates block asts.
    */
  def blockAst(blockNode: NewBlock, statements: List[Ast] = List()): Ast = {
    Ast(blockNode).withChildren(statements)
  }

  /** Create an abstract syntax tree for a call, including CPG-specific edges required for arguments and the receiver.
    *
    * Our call representation is inspired by ECMAScript, that is, in addition to arguments, a call has a base and a
    * receiver. For languages other than Javascript, leave `receiver` empty for now.
    *
    * @param callNode
    *   the node that represents the entire call
    * @param arguments
    *   arguments (without the base argument (instance))
    * @param base
    *   the value to use as `this` in the method call.
    * @param receiver
    *   the object in which the property lookup is performed
    */
  def callAst(
    callNode: NewCall,
    arguments: Seq[Ast] = List(),
    base: Option[Ast] = None,
    receiver: Option[Ast] = None
  ): Ast = {

    setArgumentIndices(arguments)

    val baseRoot = base.flatMap(_.root).toList
    val bse      = base.getOrElse(Ast())
    baseRoot match {
      case List(x: ExpressionNew) =>
        x.argumentIndex = 0
      case _ =>
    }

    val receiverRoot = if (receiver.isEmpty && base.nonEmpty) {
      baseRoot
    } else {
      val r = receiver.flatMap(_.root).toList
      r match {
        case List(x: ExpressionNew) =>
          x.argumentIndex = -1
        case _ =>
      }
      r
    }

    val rcvAst = receiver.getOrElse(Ast())

    Ast(callNode)
      .withChild(rcvAst)
      .withChild(bse)
      .withChildren(arguments)
      .withArgEdges(callNode, baseRoot)
      .withArgEdges(callNode, arguments.flatMap(_.root))
      .withReceiverEdges(callNode, receiverRoot)
  }

  def setArgumentIndices(arguments: Seq[Ast], start: Int = 1): Unit = {
    var currIndex = start
    arguments.foreach { a =>
      a.root match {
        case Some(x: ExpressionNew) =>
          x.argumentIndex = currIndex
          currIndex = currIndex + 1
        case None => // do nothing
        case _ =>
          currIndex = currIndex + 1
      }
    }
  }

  def fieldAccessAst(
    base: Ast,
    code: String,
    lineNo: Option[Int],
    columnNo: Option[Int],
    fieldName: String,
    fieldTypeFullName: String,
    fieldLineNo: Option[Int],
    fieldColumnNo: Option[Int]
  ): Ast = {
    val callNode = newOperatorCallNode(Operators.fieldAccess, code, Some(fieldTypeFullName), lineNo, columnNo)
    val fieldIdentifierNode = newFieldIdentifierNode(fieldName, fieldLineNo, fieldColumnNo)
    callAst(callNode, Seq(base, Ast(fieldIdentifierNode)))
  }

  def withIndex[T, X](nodes: Seq[T])(f: (T, Int) => X): Seq[X] =
    nodes.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  def withIndex[T, X](nodes: Array[T])(f: (T, Int) => X): Seq[X] =
    nodes.toIndexedSeq.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }

  def withArgumentIndex[T <: ExpressionNew](node: T, argIdxOpt: Option[Int]): T = {
    argIdxOpt match {
      case Some(argIdx) =>
        node.argumentIndex = argIdx
        node
      case None => node
    }
  }

  def withArgumentName[T <: ExpressionNew](node: T, argNameOpt: Option[String]): T = {
    node.argumentName = argNameOpt
    node
  }

  /** Absolute path for the given file name
    */
  def absolutePath(filename: String): String =
    better.files.File(filename).path.toAbsolutePath.normalize().toString

  /** @return
    *   the next available name for a closure in this context
    */
  def nextClosureName(): String = s"${Defines.ClosurePrefix}${closureKeyPool.next}"

}
