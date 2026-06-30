package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewControlStructure, NewJumpLabel, NewLiteral}

private[x2cpg] trait ControlStructureAstBuilder[Node, NodeProcessor] {
  this: AstCreatorBase[Node, NodeProcessor] =>

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

  def whileAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast = {
    val whileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.WHILE)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, whileNode)
    val astWithChildren = controlStructureAst(whileNode, condition, body)
    body.headOption.flatMap(_.root) match {
      case Some(bodyRoot) => astWithChildren.withTrueBodyEdge(whileNode, bodyRoot)
      case None           => astWithChildren
    }
  }

  def doWhileAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast = {
    val doWhileNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.DO)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, doWhileNode)
    val astWithChildren = controlStructureAst(doWhileNode, condition, body, placeConditionLast = true)
    body.headOption.flatMap(_.root) match {
      case Some(doBodyRoot) => astWithChildren.withDoBodyEdge(doWhileNode, doBodyRoot)
      case None             => astWithChildren
    }
  }

  def switchAst(node: Node, condition: Ast, body: Seq[Ast], code: Option[String] = None): Ast = {
    val switchNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.SWITCH)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, switchNode)
    val astWithChildren = controlStructureAst(switchNode, Option(condition), body)
    body.headOption.flatMap(_.root) match {
      case Some(bodyRoot) => astWithChildren.withTrueBodyEdge(switchNode, bodyRoot)
      case None           => astWithChildren
    }
  }

  def forAst(
    node: Node,
    locals: Seq[Ast],
    initAsts: Seq[Ast],
    conditionAsts: Seq[Ast],
    updateAsts: Seq[Ast],
    bodyAsts: Seq[Ast],
    code: Option[String] = None
  ): Ast = {
    val forNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.FOR)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, forNode)
    val lineNumber     = forNode.lineNumber
    val numOfLocals    = locals.size
    val initBlock      = setOrderExplicitly(wrapMultipleInBlock(initAsts, lineNumber), numOfLocals + 1)
    val conditionBlock = setOrderExplicitly(wrapMultipleInBlock(conditionAsts, lineNumber), numOfLocals + 2)
    val updateBlock    = setOrderExplicitly(wrapMultipleInBlock(updateAsts, lineNumber), numOfLocals + 3)
    // for the expected orders see CfgCreator.cfgForForStatement
    if (bodyAsts.nonEmpty) setOrderExplicitly(bodyAsts.head, numOfLocals + 4)
    val astWithChildren = Ast(forNode)
      .withChildren(locals)
      .withChild(initBlock)
      .withChild(conditionBlock)
      .withChild(updateBlock)
      .withChildren(bodyAsts)
      .withConditionEdges(forNode, conditionAsts.flatMap(_.root).toList)

    val astWithForInit = initBlock.root match {
      case Some(initRoot) => astWithChildren.withForInitEdge(forNode, initRoot)
      case None           => astWithChildren
    }

    val astWithForUpdate = updateBlock.root match {
      case Some(updateRoot) => astWithForInit.withForUpdateEdge(forNode, updateRoot)
      case None             => astWithForInit
    }

    bodyAsts.headOption.flatMap(_.root) match {
      case Some(bodyRoot) => astWithForUpdate.withForBodyEdge(forNode, bodyRoot)
      case None           => astWithForUpdate
    }
  }

  /** Creates an AST for a break statement. When `labelName` is present a `JumpLabel` child is created at order 1 and
    * connected to the break node via a `JUMP_ARGUMENT` edge.
    */
  def breakAst(node: Node, codeStr: String, labelName: Option[String]): Ast =
    labeledJumpAst(node, ControlStructureTypes.BREAK, codeStr, labelName)

  /** Creates an AST for a break statement. When `labelNumber` is present a `Literal` child is created at order 1 and
    * connected to the break node via a `JUMP_ARGUMENT` edge.
    */
  def breakAst(node: Node, codeStr: String, labelNumber: Option[Int])(implicit dummy: DummyImplicit): Ast =
    integerJumpAst(node, ControlStructureTypes.BREAK, codeStr, labelNumber)

  /** Creates an AST for a break statement without a jump argument. */
  def breakAst(node: Node, codeStr: String): Ast =
    jumpAst(node, ControlStructureTypes.BREAK, codeStr)

  /** Creates an AST for a continue statement. When `labelName` is present a `JumpLabel` child is created at order 1 and
    * connected to the continue node via a `JUMP_ARGUMENT` edge.
    */
  def continueAst(node: Node, codeStr: String, labelName: Option[String]): Ast =
    labeledJumpAst(node, ControlStructureTypes.CONTINUE, codeStr, labelName)

  /** Creates an AST for a continue statement. When `labelNumber` is present a `Literal` child is created at order 1 and
    * connected to the continue node via a `JUMP_ARGUMENT` edge.
    */
  def continueAst(node: Node, codeStr: String, labelNumber: Option[Int])(implicit dummy: DummyImplicit): Ast =
    integerJumpAst(node, ControlStructureTypes.CONTINUE, codeStr, labelNumber)

  /** Creates an AST for a continue statement without a jump argument. */
  def continueAst(node: Node, codeStr: String): Ast =
    jumpAst(node, ControlStructureTypes.CONTINUE, codeStr)

  def labeledJumpAst(node: Node, jumpType: String, codeStr: String, labelNumber: Option[Int])(implicit
    dummy: DummyImplicit
  ): Ast = integerJumpAst(node, jumpType, codeStr, labelNumber)

  def labeledJumpAst(node: Node, jumpType: String, codeStr: String): Ast =
    jumpAst(node, jumpType, codeStr)

  def labeledJumpAst(node: Node, jumpType: String, codeStr: String, labelName: Option[String]): Ast = {
    val jumpNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(jumpType)
      .code(codeStr)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, jumpNode)
    labelName match {
      case Some(name) =>
        val jumpLabelNode = NewJumpLabel()
          .parserTypeName(node.getClass.getSimpleName)
          .name(name)
          .code(name)
          .lineNumber(line(node))
          .columnNumber(column(node))
          .order(1)
        Ast(jumpNode)
          .withChild(Ast(jumpLabelNode))
          .withJumpArgumentEdge(jumpNode, jumpLabelNode)
      case None =>
        Ast(jumpNode)
    }
  }

  def integerJumpAst(node: Node, jumpType: String, codeStr: String, labelNumber: Option[Int]): Ast = {
    val jumpNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(jumpType)
      .code(codeStr)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, jumpNode)
    labelNumber match {
      case Some(number) =>
        val integerJump = NewLiteral()
          .code(number.toString)
          .typeFullName(Defines.Any)
          .lineNumber(line(node))
          .columnNumber(column(node))
          .order(1)
        Ast(jumpNode)
          .withChild(Ast(integerJump))
          .withJumpArgumentEdge(jumpNode, integerJump)
      case None =>
        Ast(jumpNode)
    }
  }

  /** For the given try body, catch ASTs, and finally AST, create a try-catch-finally AST.
    */
  def tryCatchAst(
    node: Node,
    tryBodyAst: Ast,
    catchAsts: Seq[Ast],
    finallyAst: Option[Ast],
    code: Option[String] = None
  ): Ast = {
    val tryNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.TRY)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, tryNode)

    setArgumentIndices(tryBodyAst +: (catchAsts ++ finallyAst.toSeq))
    val astWithChildren = Ast(tryNode)
      .withChild(tryBodyAst)
      .withChildren(catchAsts)
      .withChildren(finallyAst.toSeq)

    val astWithTryBody = tryBodyAst.root match {
      case Some(tryBodyRoot) => astWithChildren.withTryBodyEdge(tryNode, tryBodyRoot)
      case None              => astWithChildren
    }

    val astWithCatchBodies = astWithTryBody.withCatchBodyEdges(tryNode, catchAsts.flatMap(_.root).toList)

    finallyAst.flatMap(_.root) match {
      case Some(finallyRoot) => astWithCatchBodies.withFinallyBodyEdge(tryNode, finallyRoot)
      case None              => astWithCatchBodies
    }
  }

  def ifThenElseAst(
    node: Node,
    elseNode: Option[Node],
    conditionAst: Option[Ast],
    thenAst: Ast,
    elseAst: Option[Ast],
    code: Option[String] = None
  ): Ast = {
    val ifNode = NewControlStructure()
      .controlStructureType(ControlStructureTypes.IF)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, ifNode)

    val elseAstWithElseNodes = elseAst.map { elseAstElement =>
      val elseControlStructureNode = controlStructureNode(elseNode.get, ControlStructureTypes.ELSE, "else")
      setOffset(elseNode.get, elseControlStructureNode)
      Ast(elseControlStructureNode).withChild(elseAstElement)
    }

    val astWithChildren = controlStructureAst(ifNode, conditionAst, thenAst :: elseAstWithElseNodes.toList)
    val astWithTrueBody = thenAst.root match {
      case Some(thenRoot) => astWithChildren.withTrueBodyEdge(ifNode, thenRoot)
      case None           => astWithChildren
    }
    elseAstWithElseNodes.flatMap(_.root) match {
      case Some(elseRoot) => astWithTrueBody.withFalseBodyEdge(ifNode, elseRoot)
      case None           => astWithTrueBody
    }
  }

  private def jumpAst(node: Node, jumpType: String, codeStr: String): Ast = {
    val jumpNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(jumpType)
      .code(codeStr)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, jumpNode)
    Ast(jumpNode)
  }

  private def setOrderExplicitly(ast: Ast, order: Int): Ast = {
    ast.root match {
      case Some(value: ExpressionNew) => value.order(order); ast
      case _                          => ast
    }
  }
}
