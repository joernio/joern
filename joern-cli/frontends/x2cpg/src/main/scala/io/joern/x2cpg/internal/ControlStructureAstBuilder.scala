package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewControlStructure, NewJumpLabel, NewLiteral}

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

  private def controlStructureFromNode(
    node: Node,
    controlStructureType: String,
    code: Option[String] = None
  ): NewControlStructure = {
    val structureNode = NewControlStructure()
      .parserTypeName(node.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .lineNumber(line(node))
      .columnNumber(column(node))
      .code(code.getOrElse(this.code(node)))
    setOffset(node, structureNode)
    structureNode
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
  def whileAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast =
    conditionalBodyAst(node, ControlStructureTypes.WHILE, condition, body, code)

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
  def doWhileAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast =
    doWhileAstFinish(doWhileAstInit(node, code), condition, body)

  /** Creates the `NewControlStructure` node for a `do-while` loop, without wiring any children.
    *
    * This is the "split" counterpart to [[doWhileAst]] for callers that need the `do` node before its body ASTs are
    * built — e.g. to push it as a block scope so that body-local declarations resolve against it. Pass the returned
    * node to [[doWhileAstFinish]] once the child ASTs are available.
    *
    * @param node
    *   the source AST node representing the `do-while` statement (used for position and code)
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def doWhileAstInit(node: Node, code: Option[String] = None): NewControlStructure =
    controlStructureFromNode(node, ControlStructureTypes.DO, code)

  /** Wires the condition and body of a `do-while` loop onto a `doWhileNode` created by [[doWhileAstInit]].
    *
    * The condition AST is placed after the body (`placeConditionLast = true`) and connected via a `CONDITION` edge. The
    * first body child is connected via a `DO_BODY` edge.
    *
    * @param doWhileNode
    *   the `do` control-structure node created by [[doWhileAstInit]]
    * @param condition
    *   optional condition expression AST
    * @param body
    *   ordered sequence of body statement ASTs
    */
  def doWhileAstFinish(doWhileNode: NewControlStructure, condition: Option[Ast], body: Seq[Ast]): Ast = {
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
  def switchAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast =
    conditionalBodyAst(node, ControlStructureTypes.SWITCH, condition, body, code)

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
  def matchAst(node: Node, condition: Option[Ast], body: Seq[Ast], code: Option[String] = None): Ast =
    conditionalBodyAst(node, ControlStructureTypes.MATCH, condition, body, code)

  /** Shared implementation for control structures consisting of a condition and a body whose first child is reached via
    * a `TRUE_BODY` edge (`while`, `switch`, `match`).
    *
    * @param node
    *   the source AST node representing the control structure (used for position and code)
    * @param controlStructureType
    *   the [[io.shiftleft.codepropertygraph.generated.ControlStructureTypes]] constant
    * @param condition
    *   optional condition expression AST connected via a `CONDITION` edge
    * @param body
    *   ordered sequence of body ASTs; the first child is connected via a `TRUE_BODY` edge
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  private def conditionalBodyAst(
    node: Node,
    controlStructureType: String,
    condition: Option[Ast],
    body: Seq[Ast],
    code: Option[String]
  ): Ast = {
    val structureNode   = controlStructureFromNode(node, controlStructureType, code)
    val astWithChildren = controlStructureAst(structureNode, condition, body)
    body.headOption.flatMap(_.root) match {
      case Some(bodyRoot) => astWithChildren.withTrueBodyEdge(structureNode, bodyRoot)
      case None           => astWithChildren
    }
  }

  /** Creates an AST for a C-style `for` loop (`for (init; condition; update) body`).
    *
    * Multiple init, condition, or update expressions are each wrapped in a synthetic block. The resulting blocks and
    * body are placed at explicit orders so that [[io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator]] can
    * reconstruct the correct CFG edges. Condition, init, update, and body roots receive the corresponding CFG-typed
    * edges (`CONDITION`, `FOR_INIT`, `FOR_UPDATE`, `FOR_BODY`).
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
  ): Ast =
    forAstFinish(forAstInit(node, code), locals, initAsts, conditionAsts, updateAsts, bodyAsts)

  /** Creates the `NewControlStructure` node for a C-style `for` loop, without wiring any children.
    *
    * This is the "split" counterpart to [[forAst]] for callers that need the `for` node before its body ASTs are built
    * — e.g. to push it as a block scope so that header-local declarations resolve against it. Pass the returned node to
    * [[forAstFinish]] once the child ASTs are available.
    *
    * @param node
    *   the source AST node representing the `for` statement (used for position and code)
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def forAstInit(node: Node, code: Option[String] = None): NewControlStructure =
    controlStructureFromNode(node, ControlStructureTypes.FOR, code)

  /** Wires the children of a C-style `for` loop onto a `forNode` created by [[forAstInit]].
    *
    * Multiple init, condition, or update expressions are each wrapped in a synthetic block. The resulting blocks and
    * body are placed at explicit orders so that [[io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator]] can
    * reconstruct the correct CFG edges. Condition, init, update, and body roots receive the corresponding CFG-typed
    * edges (`CONDITION`, `FOR_INIT`, `FOR_UPDATE`, `FOR_BODY`).
    *
    * @param forNode
    *   the `for` control-structure node created by [[forAstInit]]
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
    */
  def forAstFinish(
    forNode: NewControlStructure,
    locals: Seq[Ast],
    initAsts: Seq[Ast],
    conditionAsts: Seq[Ast],
    updateAsts: Seq[Ast],
    bodyAsts: Seq[Ast]
  ): Ast = {
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
    jumpAst(node, ControlStructureTypes.BREAK, codeStr, labelName)

  /** Creates an AST for a `break` statement carrying a numeric jump argument (e.g. `break 2`, encoding how many
    * loop/switch levels the break applies to).
    *
    * When `labelNumber` is [[Some]], a `Literal` child with the given `typeFullName` is created at order 1 and
    * connected via a `JUMP_ARGUMENT` edge.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the break statement
    * @param labelNumber
    *   optional numeric jump target (loop/switch nesting level)
    * @param typeFullName
    *   the `typeFullName` for the generated literal
    */
  def breakAst(node: Node, codeStr: String, labelNumber: Option[Int], typeFullName: String): Ast =
    jumpAst(node, ControlStructureTypes.BREAK, codeStr, labelNumber, typeFullName)

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
    jumpAst(node, ControlStructureTypes.CONTINUE, codeStr, labelName)

  /** Creates an AST for a `continue` statement carrying a numeric jump argument (e.g. `continue 2`, encoding how many
    * loop levels the continue applies to).
    *
    * When `labelNumber` is [[Some]], a `Literal` child with the given `typeFullName` is created at order 1 and
    * connected via a `JUMP_ARGUMENT` edge.
    *
    * @param node
    *   the source AST node (used for position)
    * @param codeStr
    *   source-code string for the continue statement
    * @param labelNumber
    *   optional numeric jump target (loop nesting level)
    * @param typeFullName
    *   the `typeFullName` for the generated literal
    */
  def continueAst(node: Node, codeStr: String, labelNumber: Option[Int], typeFullName: String): Ast =
    jumpAst(node, ControlStructureTypes.CONTINUE, codeStr, labelNumber, typeFullName)

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
    jumpAst(node, ControlStructureTypes.GOTO, codeStr, Option(labelName))

  /** Creates an AST for a `throw` statement.
    *
    * When `thrownExprAsts` contains exactly one element it is attached directly; when it contains more than one they
    * are first wrapped in a synthetic block via `wrapMultipleInBlock`. The resulting child is connected via an
    * `ARGUMENT` edge so that the CFG creator can reach it via `_argumentOut`.
    *
    * @param node
    *   the source AST node representing the `throw` statement (used for position and code)
    * @param thrownExprAsts
    *   ASTs for the thrown expression(s); pass an empty sequence when there is no operand
    * @param code
    *   explicit source-code string; falls back to `this.code(node)` when absent
    */
  def throwAst(node: Node, thrownExprAsts: Seq[Ast], code: Option[String] = None): Ast = {
    val throwNode = controlStructureFromNode(node, ControlStructureTypes.THROW, code)
    thrownExprAsts match {
      case Nil => Ast(throwNode)
      case _ =>
        val argAst = wrapMultipleInBlock(thrownExprAsts, line(node))
        argAst.root match {
          case Some(argRoot) => Ast(throwNode).withChild(argAst).withArgEdge(throwNode, argRoot)
          case None          => Ast(throwNode).withChild(argAst)
        }
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
    val tryNode = controlStructureFromNode(node, ControlStructureTypes.TRY, code)

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
    * The `then` branch is connected via a `TRUE_BODY` edge, a present `else` branch via a `FALSE_BODY` edge, and the
    * condition via a `CONDITION` edge.
    *
    * @param node
    *   the source AST node representing the `if` keyword/expression (used for position and code)
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
    conditionAst: Option[Ast],
    thenAst: Ast,
    elseAst: Option[Ast],
    code: Option[String] = None
  ): Ast = {
    val ifNode = controlStructureFromNode(node, ControlStructureTypes.IF, code)

    val astWithChildren = controlStructureAst(ifNode, conditionAst, thenAst :: elseAst.toList)
    val astWithTrueBody = thenAst.root match {
      case Some(thenRoot) => astWithChildren.withTrueBodyEdge(ifNode, thenRoot)
      case None           => astWithChildren
    }
    elseAst.flatMap(_.root) match {
      case Some(elseRoot) => astWithTrueBody.withFalseBodyEdge(ifNode, elseRoot)
      case None           => astWithTrueBody
    }
  }

  /** Creates a bare jump node (`break`, `continue`, `goto`) without a jump argument. */
  private def jumpAst(node: Node, jumpType: String, codeStr: String): Ast =
    Ast(jumpNode(node, jumpType, codeStr))

  /** Creates a jump node with an optional string label target (e.g. `break outerLoop` or `goto label`). When
    * `labelName` is [[Some]], a `JumpLabel` child is created at order 1 and connected via a `JUMP_ARGUMENT` edge.
    */
  private def jumpAst(node: Node, jumpType: String, codeStr: String, labelName: Option[String]): Ast = {
    val jump = jumpNode(node, jumpType, codeStr)
    labelName match {
      case Some(name) =>
        val jumpLabelNode = NewJumpLabel()
          .parserTypeName(node.getClass.getSimpleName)
          .name(name)
          .code(name)
          .lineNumber(line(node))
          .columnNumber(column(node))
          .order(1)
        Ast(jump)
          .withChild(Ast(jumpLabelNode))
          .withJumpArgumentEdge(jump, jumpLabelNode)
      case None =>
        Ast(jump)
    }
  }

  /** Creates a jump node with an optional numeric jump argument (e.g. `break 2`). When `labelNumber` is [[Some]], a
    * `Literal` child with the given `typeFullName` is created at order 1 and connected via a `JUMP_ARGUMENT` edge.
    */
  private def jumpAst(
    node: Node,
    jumpType: String,
    codeStr: String,
    labelNumber: Option[Int],
    typeFullName: String
  ): Ast = {
    val jump = jumpNode(node, jumpType, codeStr)
    labelNumber match {
      case Some(number) =>
        val literal = NewLiteral()
          .code(number.toString)
          .typeFullName(typeFullName)
          .lineNumber(line(node))
          .columnNumber(column(node))
          .order(1)
        Ast(jump)
          .withChild(Ast(literal))
          .withJumpArgumentEdge(jump, literal)
      case None =>
        Ast(jump)
    }
  }

  private def jumpNode(node: Node, jumpType: String, codeStr: String): NewControlStructure =
    controlStructureFromNode(node, jumpType, Some(codeStr))

  private def setOrderExplicitly(ast: Ast, order: Int): Ast = {
    ast.root match {
      case Some(value: ExpressionNew) => value.order(order); ast
      case _                          => ast
    }
  }
}
