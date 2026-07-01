package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewControlStructure, NewJumpLabel}

/** Mixin that provides helpers for building control-structure ASTs. */
private[x2cpg] trait ControlStructureAstBuilder[Node, NodeProcessor] {
  this: AstCreatorBase[Node, NodeProcessor] =>

  /** Low-level primitive that creates an AST rooted at `controlStructureNode`, wiring `condition` via a `CONDITION`
    * edge.
    *
    * This is the shared building block for the typed creator functions below; frontends should always go through one of
    * those creators rather than constructing a `NewControlStructure` and calling this directly.
    *
    * @param controlStructureNode
    *   the pre-built control-structure CPG node
    * @param condition
    *   optional condition expression AST; when present a `CONDITION` edge is added from `controlStructureNode` to its
    *   root
    * @param children
    *   ordered child ASTs (body, branches, etc.)
    * @param placeConditionLast
    *   when `true` the condition AST is appended after `children` instead of prepended (used for `do-while`)
    */
  private def controlStructureAst(
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

  /** Creates an AST for a `while` loop.
    *
    * The condition AST is placed before the body and connected via a `CONDITION` edge. The first body child is
    * connected via a `TRUE_BODY` edge.
    *
    * @param node
    *   the source AST node representing the `while` statement (used for position and code)
    * @param condition
    *   optional condition expression AST
    * @param body
    *   ordered sequence of body statement ASTs
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def whileAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast = {
    val whileNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
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

  /** Creates an AST for a `do-while` loop.
    *
    * The condition AST is placed after the body (`placeConditionLast = true`) and connected via a `CONDITION` edge. The
    * first body child is connected via a `DO_BODY` edge.
    *
    * @param node
    *   the source AST node representing the `do-while` statement (used for position and code)
    * @param condition
    *   optional condition expression AST
    * @param body
    *   ordered sequence of body statement ASTs
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def doWhileAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast = {
    val doWhileNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
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

  /** Creates an AST for a `switch` statement.
    *
    * The condition AST is connected via a `CONDITION` edge and the first body child via a `TRUE_BODY` edge.
    *
    * @param node
    *   the source AST node representing the `switch` statement (used for position and code)
    * @param condition
    *   the optional switch discriminant expression AST
    * @param body
    *   ordered sequence of case/body ASTs
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def switchAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast = {
    val switchNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(ControlStructureTypes.SWITCH)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, switchNode)
    val astWithChildren = controlStructureAst(switchNode, condition, body)
    body.headOption.flatMap(_.root) match {
      case Some(bodyRoot) => astWithChildren.withTrueBodyEdge(switchNode, bodyRoot)
      case None           => astWithChildren
    }
  }

  /** Creates an AST for a `match` expression (e.g. PHP's `match`).
    *
    * The condition AST is connected via a `CONDITION` edge and the first body child via a `TRUE_BODY` edge, mirroring
    * [[switchAst]] (the CFG creator treats `match` like a switch).
    *
    * @param node
    *   the source AST node representing the `match` expression (used for position and code)
    * @param condition
    *   the optional match subject expression AST
    * @param body
    *   ordered sequence of match-arm ASTs
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def matchAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast = {
    val matchNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(ControlStructureTypes.MATCH)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, matchNode)
    val astWithChildren = controlStructureAst(matchNode, condition, body)
    body.headOption.flatMap(_.root) match {
      case Some(bodyRoot) => astWithChildren.withTrueBodyEdge(matchNode, bodyRoot)
      case None           => astWithChildren
    }
  }

  /** Creates an AST for a C-style `for` loop (`for (init; condition; update) body`).
    *
    * Multiple init, condition, or update expressions are each wrapped in a synthetic block. The resulting blocks and
    * body are placed at explicit orders so that [[io.joern.x2cpg.passes.cfg.CfgCreator]] can reconstruct the correct
    * CFG edges. Condition, init, update, and body roots receive the corresponding CFG-typed edges (`CONDITION`,
    * `FOR_INIT`, `FOR_UPDATE`, `FOR_BODY`).
    *
    * @param node
    *   the source AST node representing the `for` statement (used for position and code)
    * @param locals
    *   variable declarations inside the `for` header (placed at the lowest orders)
    * @param initAsts
    *   initialiser expression ASTs
    * @param conditionAsts
    *   loop-condition expression ASTs
    * @param updateAsts
    *   per-iteration update expression ASTs
    * @param bodyAsts
    *   ordered sequence of loop-body statement ASTs
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
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
      .parserTypeName(node.getClass.getSimpleName)
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

  /** Creates an AST for a `break` statement with an optional string label target.
    *
    * When `labelName` is [[Some]], a `JumpLabel` child is created at order 1 and connected via a `JUMP_ARGUMENT` edge.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the break statement
    * @param labelName
    *   optional string name of the target label
    */
  def breakAst(node: Node, codeStr: String, labelName: Option[String]): Ast =
    labeledJumpAst(node, ControlStructureTypes.BREAK, codeStr, labelName)

  /** Creates an AST for a `break` statement carrying a pre-built jump argument (e.g. an integer literal encoding how
    * many loop/switch levels the break applies to).
    *
    * When `argument` is [[Some]], its root is connected via a `JUMP_ARGUMENT` edge; the caller controls the argument
    * node (and thus its `typeFullName`). The `DummyImplicit` witness disambiguates this overload from the
    * [[String]]-label variant at the call site.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the break statement
    * @param argument
    *   optional pre-built jump-argument AST
    */
  def breakAst(node: Node, codeStr: String, argument: Option[Ast])(implicit dummy: DummyImplicit): Ast =
    jumpAst(node, ControlStructureTypes.BREAK, codeStr, argument)

  /** Creates an AST for a `break` statement without a jump argument.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the break statement
    */
  def breakAst(node: Node, codeStr: String): Ast =
    jumpAst(node, ControlStructureTypes.BREAK, codeStr)

  /** Creates an AST for a `continue` statement with an optional string label target.
    *
    * When `labelName` is [[Some]], a `JumpLabel` child is created at order 1 and connected via a `JUMP_ARGUMENT` edge.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the continue statement
    * @param labelName
    *   optional string name of the target label
    */
  def continueAst(node: Node, codeStr: String, labelName: Option[String]): Ast =
    labeledJumpAst(node, ControlStructureTypes.CONTINUE, codeStr, labelName)

  /** Creates an AST for a `continue` statement carrying a pre-built jump argument (e.g. an integer literal encoding how
    * many loop levels the continue applies to).
    *
    * When `argument` is [[Some]], its root is connected via a `JUMP_ARGUMENT` edge; the caller controls the argument
    * node (and thus its `typeFullName`). The `DummyImplicit` witness disambiguates this overload from the
    * [[String]]-label variant at the call site.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the continue statement
    * @param argument
    *   optional pre-built jump-argument AST
    */
  def continueAst(node: Node, codeStr: String, argument: Option[Ast])(implicit dummy: DummyImplicit): Ast =
    jumpAst(node, ControlStructureTypes.CONTINUE, codeStr, argument)

  /** Creates an AST for a `continue` statement without a jump argument.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the continue statement
    */
  def continueAst(node: Node, codeStr: String): Ast =
    jumpAst(node, ControlStructureTypes.CONTINUE, codeStr)

  /** Creates an AST for a `goto` statement targeting the label `labelName`.
    *
    * A [[io.shiftleft.codepropertygraph.generated.nodes.NewJumpLabel]] child is created at order 1 and connected via a
    * `JUMP_ARGUMENT` edge, from which [[io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator]] resolves the jump
    * target.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the goto statement
    * @param labelName
    *   name of the target label
    */
  def gotoAst(node: Node, codeStr: String, labelName: String): Ast =
    labeledJumpAst(node, ControlStructureTypes.GOTO, codeStr, Option(labelName))

  /** Creates an AST for a jump statement with an optional string label target (e.g. `break outerLoop` or `goto label`).
    *
    * When `labelName` is [[Some]], a [[io.shiftleft.codepropertygraph.generated.nodes.NewJumpLabel]] child is created
    * at order 1 and connected via a `JUMP_ARGUMENT` edge. When it is [[None]], a bare jump node is returned.
    *
    * @param node
    *   the source AST node (used for position)
    * @param jumpType
    *   the [[io.shiftleft.codepropertygraph.generated.ControlStructureTypes]] constant
    * @param codeStr
    *   source-code string for the jump statement
    * @param labelName
    *   optional string name of the target label
    */
  private def labeledJumpAst(node: Node, jumpType: String, codeStr: String, labelName: Option[String]): Ast = {
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

  /** Creates an AST for a `try-catch-finally` statement.
    *
    * The try body, catch clauses, and optional finally block receive `TRY_BODY`, `CATCH_BODY`, and `FINALLY_BODY` edges
    * respectively. Argument indices are set across all three sections so that CFG construction can order them
    * correctly.
    *
    * @param node
    *   the source AST node representing the `try` keyword (used for position and code)
    * @param tryBodyAst
    *   AST for the protected body
    * @param catchAsts
    *   ordered sequence of catch-clause ASTs
    * @param finallyAst
    *   optional AST for the finally block
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def tryCatchAst(
    node: Node,
    tryBodyAst: Ast,
    catchAsts: Seq[Ast],
    finallyAst: Option[Ast],
    code: Option[String] = None
  ): Ast = {
    val tryNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
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

  /** Creates an AST for an `if-then-else` expression or statement.
    *
    * When `elseAst` is present, a synthetic `ELSE` control-structure node wraps it, and the whole subtree is connected
    * via a `FALSE_BODY` edge. The `then` branch is connected via a `TRUE_BODY` edge and the condition via a `CONDITION`
    * edge.
    *
    * @param node
    *   the source AST node representing the `if` keyword/expression (used for position and code)
    * @param elseNode
    *   the source AST node representing the `else` keyword; required when `elseAst` is [[Some]]
    * @param conditionAst
    *   optional condition expression AST
    * @param thenAst
    *   AST for the true branch
    * @param elseAst
    *   optional AST for the false branch
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def ifThenElseAst(
    node: Node,
    elseNode: Option[Node],
    conditionAst: Option[Ast],
    thenAst: Ast,
    elseAst: Option[Ast],
    code: Option[String] = None
  ): Ast = {
    val ifNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
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

  private def jumpAst(node: Node, jumpType: String, codeStr: String, argument: Option[Ast]): Ast = {
    val jumpNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(jumpType)
      .code(codeStr)
      .lineNumber(line(node))
      .columnNumber(column(node))
    setOffset(node, jumpNode)
    argument.flatMap(_.root) match {
      case Some(argumentRoot) =>
        Ast(jumpNode)
          .withChild(argument.get)
          .withJumpArgumentEdge(jumpNode, argumentRoot)
      case None =>
        Ast(jumpNode)
    }
  }

  private def setOrderExplicitly(ast: Ast, order: Int): Ast = {
    ast.root match {
      case Some(value: ExpressionNew) => value.order(order); ast
      case _                          => ast
    }
  }
}
