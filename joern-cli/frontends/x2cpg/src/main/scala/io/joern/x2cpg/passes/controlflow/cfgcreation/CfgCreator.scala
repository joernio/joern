package io.joern.x2cpg.passes.controlflow.cfgcreation

import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

/** Translation of abstract syntax trees into control flow graphs
  *
  * The problem of translating an abstract syntax tree into a corresponding control flow graph can be formulated as a
  * recursive problem in which sub trees of the syntax tree are translated and their corresponding control flow graphs
  * are connected according to the control flow semantics of the root node. For example, consider the abstract syntax
  * tree for an if-statement:
  * {{{
  *               (  if )
  *              /       \
  *          (x < 10)  (x += 1)
  *            / \       / \
  *           x  10     x   1
  * }}}
  * This tree can be translated into a control flow graph, by translating the sub tree rooted in `x < 10` and that of
  * `x+= 1` and connecting their control flow graphs according to the semantics of `if`:
  * {{{
  *            [x < 10]----
  *               |t     f|
  *            [x +=1 ]   |
  *               |
  * }}}
  *
  * The semantics of if dictate that the first sub tree to the left is a condition, which is connected to the CFG of the
  * second sub tree - the body of the if statement - via a control flow edge with the `true` label (indicated in the
  * illustration by `t`), and to the CFG of any follow-up code via a `false` edge (indicated by `f`).
  *
  * A problem that becomes immediately apparent in the illustration is that the result of translating a sub tree may
  * leave us with edges for which a source node is known but the destination node depends on parents or siblings that
  * were not considered in the translation. For example, we know that an outgoing edge from [x<10] must exist, but we do
  * not yet know where it should lead. We refer to the set of nodes of the control flow graph with outgoing edges for
  * which the destination node is yet to be determined as the "fringe" of the control flow graph.
  */
class CfgCreator(entryNode: Method, diffGraph: DiffGraphBuilder) {

  import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.*
  import io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator.*

  /** Control flow graph definitions often feature a designated entry and exit node for each method. While these nodes
    * are no-ops from a computational point of view, they are useful to guarantee that a method has exactly one entry
    * and one exit.
    *
    * For the CPG-based control flow graph, we do not need to introduce fake entry and exit node. Instead, we can use
    * the METHOD and METHOD_RETURN nodes as entry and exit nodes respectively. Note that METHOD_RETURN nodes are the
    * nodes representing formal return parameters, of which there exists exactly one per method.
    */
  private val exitNode: MethodReturn = entryNode.methodReturn

  /** We return the CFG as a sequence of Diff Graphs that is calculated by first obtaining the CFG for the method and
    * then resolving gotos.
    */
  def run(): Unit = {
    cfgForMethod(entryNode).withResolvedJumpToLabel().edges.foreach { edge =>
      // TODO: we are ignoring edge.edgeType because the
      //  CFG spec doesn't define an edge type at the moment
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CFG)
    }
  }

  /** Conversion of a method to a CFG, showing the decomposition of the control flow graph generation problem into that
    * of translating sub trees according to the node type. In the particular case of a method, the CFG is obtained by
    * creating a CFG containing the single method node and a fringe containing the node and an outgoing AlwaysEdge, to
    * the CFG obtained by translating child CFGs one by one and appending them.
    */
  private def cfgForMethod(node: Method): Cfg =
    cfgForSingleNode(node) ++ cfgForChildren(node)

  /** For any single AST node, we can construct a CFG containing that single node by setting it as the entry node and
    * placing it in the fringe.
    */
  private def cfgForSingleNode(node: CfgNode): Cfg =
    Cfg(entryNode = Option(node), fringe = List((node, AlwaysEdge)))

  /** The CFG for all children is obtained by translating child ASTs one by one from left to right and appending them.
    */
  private def cfgForChildren(node: AstNode): Cfg =
    node.astChildren.l.map(cfgFor).reduceOption((x, y) => x ++ y).getOrElse(Cfg.empty)

  /** Returns true if this node is a child to some `try` control structure, false if otherwise.
    */
  private def withinATryBlock(x: AstNode): Boolean =
    x.inAst.isControlStructure.exists(_.controlStructureType == ControlStructureTypes.TRY)

  /** This method dispatches AST nodes by type and calls corresponding conversion methods.
    */
  protected def cfgFor(node: AstNode): Cfg =
    node match {
      case _: Method | _: MethodParameterIn | _: Modifier | _: Local | _: TypeDecl | _: Member =>
        Cfg.empty
      case _: MethodRef | _: TypeRef | _: MethodReturn =>
        cfgForSingleNode(node.asInstanceOf[CfgNode])
      case controlStructure: ControlStructure =>
        cfgForControlStructure(controlStructure)
      case jumpTarget: JumpTarget =>
        cfgForJumpTarget(jumpTarget)
      case ret: Return if withinATryBlock(ret) =>
        cfgForReturn(ret, inheritFringe = true)
      case ret: Return =>
        cfgForReturn(ret)
      case call: Call if call.name == Operators.logicalAnd =>
        cfgForAndExpression(call)
      case call: Call if call.name == Operators.logicalOr =>
        cfgForOrExpression(call)
      case call: Call if call.name == Operators.conditional =>
        cfgForConditionalExpression(call)
      case call: Call if call.dispatchType == DispatchTypes.INLINED =>
        cfgForInlinedCall(call)
      case block: Block if blockMatches(block) =>
        cfgForChildren(block)
      case _: Block =>
        cfgForChildren(node) ++ cfgForSingleNode(node.asInstanceOf[CfgNode])
      case _: Call | _: FieldIdentifier | _: Identifier | _: Literal | _: Block | _: Unknown =>
        cfgForChildren(node) ++ cfgForSingleNode(node.asInstanceOf[CfgNode])
      case _ =>
        cfgForChildren(node)
    }

  private def isLogicalOperator(node: AstNode): Boolean = node match {
    case call: Call =>
      call.name == Operators.conditional || call.name == Operators.logicalOr || call.name == Operators.logicalAnd
    case _ => false
  }

  private def isInlinedCall(node: AstNode): Boolean = node match {
    case call: Call => call.dispatchType == DispatchTypes.INLINED
    case _          => false
  }

  /** Only include block nodes that do not describe the entire method body or the bodies of control structures or
    * inlined calls or logical operators.
    */
  private def blockMatches(block: Block): Boolean = {
    if (block._astIn.hasNext) {
      val parentNode = block.astParent
      parentNode.isMethod || parentNode.isControlStructure || isLogicalOperator(parentNode) || isInlinedCall(parentNode)
    } else false
  }

  /** A second layer of dispatching for control structures. This could as well be part of `cfgFor` and has only been
    * placed into a separate function to increase readability.
    */
  protected def cfgForControlStructure(node: ControlStructure): Cfg =
    node.controlStructureType match {
      case ControlStructureTypes.BREAK =>
        cfgForBreakStatement(node)
      case ControlStructureTypes.CONTINUE =>
        cfgForContinueStatement(node)
      case ControlStructureTypes.WHILE =>
        cfgForWhileStatement(node)
      case ControlStructureTypes.DO =>
        cfgForDoStatement(node)
      case ControlStructureTypes.FOR =>
        cfgForForStatement(node)
      case ControlStructureTypes.GOTO =>
        cfgForGotoStatement(node)
      case ControlStructureTypes.IF =>
        cfgForIfStatement(node)
      case ControlStructureTypes.ELSE =>
        cfgForChildren(node)
      case ControlStructureTypes.SWITCH =>
        cfgForSwitchStatement(node)
      case ControlStructureTypes.TRY =>
        cfgForTryStatement(node)
      case ControlStructureTypes.CATCH =>
        cfgForChildren(node)
      case ControlStructureTypes.FINALLY =>
        cfgForChildren(node)
      case ControlStructureTypes.MATCH =>
        cfgForMatchExpression(node)
      case _ =>
        Cfg.empty
    }

  /** The CFG for a break/continue statements contains only the break/continue statement as a single entry node. The
    * fringe is empty, that is, appending another CFG to the break statement will not result in the creation of an edge
    * from the break statement to the entry point of the other CFG. Labeled breaks are treated like gotos and are added
    * to "jumpsToLabel".
    */
  protected def cfgForBreakStatement(node: ControlStructure): Cfg = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel: JumpLabel) =>
        val labelName = jumpLabel.name
        Cfg(entryNode = Option(node), jumpsToLabel = List((node, labelName)))
      case Some(literal: Literal) =>
        // In case we find a literal, it is assumed to be an integer literal which
        // indicates how many loop/switch levels the break shall apply to.
        val numberOfLevels = Integer.valueOf(literal.code)
        Cfg(entryNode = Option(node), breaks = List((node, numberOfLevels)))
      case Some(_) =>
        throw new NotImplementedError(
          "Only jump labels and integer literals are currently supported for break statements."
        )
      case None =>
        Cfg(entryNode = Option(node), breaks = List((node, 1)))
    }
  }

  protected def cfgForContinueStatement(node: ControlStructure): Cfg = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel: JumpLabel) =>
        val labelName = jumpLabel.name
        Cfg(entryNode = Option(node), jumpsToLabel = List((node, labelName)))
      case Some(literal: Literal) =>
        // In case we find a literal, it is assumed to be an integer literal which
        // indicates how many loop levels the continue shall apply to.
        val numberOfLevels = Integer.valueOf(literal.code)
        Cfg(entryNode = Option(node), continues = List((node, numberOfLevels)))
      case Some(_) =>
        throw new NotImplementedError(
          "Only jump labels and integer literals are currently supported for continue statements."
        )
      case None =>
        Cfg(entryNode = Option(node), continues = List((node, 1)))
    }
  }

  /** Jump targets ("labels") are included in the CFG. As these should be connected to the next appended CFG, we specify
    * that the label node is both the entry node and the only node in the fringe. This is achieved by calling
    * `cfgForSingleNode` on the label node. Just like for breaks and continues, we record labels. We store case/default
    * labels separately from other labels, but that is not a relevant implementation detail.
    */
  protected def cfgForJumpTarget(n: JumpTarget): Cfg = {
    val labelName = n.name
    val cfg       = cfgForSingleNode(n)
    if (labelName.startsWith("case") || labelName.startsWith("default")) {
      cfg.copy(caseLabels = List(n))
    } else {
      cfg.copy(labeledNodes = Map(labelName -> n))
    }
  }

  /** A CFG for a goto statement is one containing the goto node as an entry node and an empty fringe. Moreover, we
    * store the goto for dispatching with `withResolvedJumpToLabel` once the CFG for the entire method has been
    * calculated.
    */
  protected def cfgForGotoStatement(node: ControlStructure): Cfg = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel) =>
        val labelName = jumpLabel.asInstanceOf[JumpLabel].name
        Cfg(entryNode = Option(node), jumpsToLabel = List((node, labelName)))
      case None =>
        // Support for old format where the label name is parsed from the code field.
        val target = node.code.split(" ").lastOption.map(x => x.slice(0, x.length - 1))
        target.map(t => Cfg(entryNode = Some(node), jumpsToLabel = List((node, t)))).getOrElse(Cfg.empty)
    }
  }

  /** Return statements may contain expressions as return values, and therefore, the CFG for a return statement consists
    * of the CFG for calculation of that expression, appended to a CFG containing only the return node, connected with a
    * single edge to the method exit node. The fringe is empty.
    *
    * @param inheritFringe
    *   indicates if the resulting Cfg object must contain the fringe value of the return value's children.
    */
  protected def cfgForReturn(actualRet: Return, inheritFringe: Boolean = false): Cfg = {
    val childrenCfg = cfgForChildren(actualRet)
    childrenCfg ++
      Cfg(
        entryNode = Option(actualRet),
        edges = singleEdge(actualRet, exitNode),
        if (inheritFringe) childrenCfg.fringe else List()
      )
  }

  /** The right hand side of a logical AND expression is only evaluated if the left hand side is true as the entire
    * expression can only be true if both expressions are true. This is encoded in the corresponding control flow graph
    * by creating control flow graphs for the left and right hand expressions and appending the two, where the fringe
    * edge type of the left CFG is `TrueEdge`.
    */
  protected def cfgForAndExpression(call: Call): Cfg = {
    val leftCfg    = cfgFor(call.argument(1))
    val rightCfg   = cfgFor(call.argument(2))
    val diffGraphs = edgesFromFringeTo(leftCfg, rightCfg.entryNode, TrueEdge) ++ leftCfg.edges ++ rightCfg.edges
    Cfg
      .from(leftCfg, rightCfg)
      .copy(
        entryNode = leftCfg.entryNode,
        edges = diffGraphs,
        fringe = leftCfg.fringe ++ rightCfg.fringe
      ) ++ cfgForSingleNode(call)
  }

  /** Same construction recipe as for the AND expression, just that the fringe edge type of the left CFG is `FalseEdge`.
    */
  protected def cfgForOrExpression(call: Call): Cfg = {
    val leftCfg    = cfgFor(call.argument(1))
    val rightCfg   = cfgFor(call.argument(2))
    val diffGraphs = edgesFromFringeTo(leftCfg, rightCfg.entryNode, FalseEdge) ++ leftCfg.edges ++ rightCfg.edges
    Cfg
      .from(leftCfg, rightCfg)
      .copy(
        entryNode = leftCfg.entryNode,
        edges = diffGraphs,
        fringe = leftCfg.fringe ++ rightCfg.fringe
      ) ++ cfgForSingleNode(call)
  }

  /** A conditional expression is of the form `condition ? trueExpr ; falseExpr` where both `trueExpr` and `falseExpr`
    * are optional. We create the corresponding CFGs by creating CFGs for the three expressions and adding edges between
    * them. The new entry node is the condition entry node.
    */
  protected def cfgForConditionalExpression(call: Call): Cfg = {
    val conditionCfg = cfgFor(call.argument(1))
    val trueCfg      = call.argumentOption(2).map(cfgFor).getOrElse(Cfg.empty)
    val falseCfg     = call.argumentOption(3).map(cfgFor).getOrElse(Cfg.empty)
    val diffGraphs = edgesFromFringeTo(conditionCfg, trueCfg.entryNode, TrueEdge) ++
      edgesFromFringeTo(conditionCfg, falseCfg.entryNode, FalseEdge)

    val trueFridge = if (trueCfg.entryNode.isDefined) {
      trueCfg.fringe
    } else {
      conditionCfg.fringe.withEdgeType(TrueEdge)
    }
    val falseFridge = if (falseCfg.entryNode.isDefined) {
      falseCfg.fringe
    } else {
      conditionCfg.fringe.withEdgeType(FalseEdge)
    }

    Cfg
      .from(conditionCfg, trueCfg, falseCfg)
      .copy(
        entryNode = conditionCfg.entryNode,
        edges = conditionCfg.edges ++ trueCfg.edges ++ falseCfg.edges ++ diffGraphs,
        fringe = trueFridge ++ falseFridge
      ) ++ cfgForSingleNode(call)
  }

  /** For macros, the AST contains a CALL node, along with child sub trees for all arguments, and a final sub tree that
    * contains the inlined code. The corresponding CFG consists of the CFG for the call, an edge to the exit and an edge
    * to the CFG of the inlined code. We choose this representation because it allows both queries that use the macro
    * reference as well as queries that reference the inline code to be chosen as sources/sinks in data flow queries.
    */
  def cfgForInlinedCall(call: Call): Cfg = {
    val cfgForMacroCall = call.argument.l
      .map(cfgFor)
      .reduceOption((x, y) => x ++ y)
      .getOrElse(Cfg.empty) ++ cfgForSingleNode(call)
    val cfgForExpansion = call.astChildren.lastOption.map(cfgFor).getOrElse(Cfg.empty)
    val cfg = Cfg
      .from(cfgForMacroCall, cfgForExpansion)
      .copy(
        entryNode = cfgForMacroCall.entryNode,
        edges = cfgForMacroCall.edges ++ cfgForExpansion.edges ++ cfgForExpansion.entryNode.toList
          .flatMap(x => singleEdge(call, x)),
        fringe = cfgForMacroCall.fringe ++ cfgForExpansion.fringe
      )
    cfg
  }

  /** A for statement is of the form `for(initExpr; condition; loopExpr) body` and all four components may be empty. The
    * sequence (condition - body - loopExpr) form the inner part of the loop and we calculate the corresponding CFG
    * `innerCfg` so that it is no longer relevant which of these three actually exist and we still have an entry node
    * for the loop and a fringe.
    */
  protected def cfgForForStatement(node: ControlStructure): Cfg = {
    val children     = node.astChildren.l
    val nLocals      = children.count(_.isLocal)
    val initExprCfg  = children.find(_.order == nLocals + 1).map(cfgFor).getOrElse(Cfg.empty)
    val conditionCfg = children.find(_.order == nLocals + 2).map(cfgFor).getOrElse(Cfg.empty)
    val loopExprCfg  = children.find(_.order == nLocals + 3).map(cfgFor).getOrElse(Cfg.empty)
    val bodyCfg      = children.find(_.order == nLocals + 4).map(cfgFor).getOrElse(Cfg.empty)

    val innerCfg  = conditionCfg ++ bodyCfg ++ loopExprCfg
    val entryNode = (initExprCfg ++ innerCfg).entryNode

    val newEdges = edgesFromFringeTo(initExprCfg, innerCfg.entryNode) ++
      edgesFromFringeTo(innerCfg, innerCfg.entryNode) ++
      edgesFromFringeTo(conditionCfg, bodyCfg.entryNode, TrueEdge) ++ {
        if (loopExprCfg.entryNode.isDefined) {
          edges(takeCurrentLevel(bodyCfg.continues), loopExprCfg.entryNode)
        } else {
          edges(takeCurrentLevel(bodyCfg.continues), innerCfg.entryNode)
        }
      }

    Cfg
      .from(initExprCfg, conditionCfg, loopExprCfg, bodyCfg)
      .copy(
        entryNode = entryNode,
        edges = newEdges ++ initExprCfg.edges ++ innerCfg.edges,
        fringe = conditionCfg.fringe.withEdgeType(FalseEdge) ++ takeCurrentLevel(bodyCfg.breaks).map((_, AlwaysEdge)),
        breaks = reduceAndFilterLevel(bodyCfg.breaks),
        continues = reduceAndFilterLevel(bodyCfg.continues)
      )
  }

  /** A Do-Statement is of the form `do body while(condition)` where body may be empty. We again first calculate the
    * inner CFG as bodyCfg ++ conditionCfg and then connect edges according to the semantics of do-while.
    */
  protected def cfgForDoStatement(node: ControlStructure): Cfg = {
    val bodyCfg      = node.astChildren.where(_.order(1)).headOption.map(cfgFor).getOrElse(Cfg.empty)
    val conditionCfg = node.condition.headOption.map(cfgFor).getOrElse(Cfg.empty)
    val innerCfg     = bodyCfg ++ conditionCfg

    val diffGraphs =
      edges(takeCurrentLevel(bodyCfg.continues), conditionCfg.entryNode) ++
        edgesFromFringeTo(bodyCfg, conditionCfg.entryNode) ++
        edgesFromFringeTo(conditionCfg, innerCfg.entryNode, TrueEdge)

    Cfg
      .from(bodyCfg, conditionCfg, innerCfg)
      .copy(
        entryNode = if (bodyCfg != Cfg.empty) { bodyCfg.entryNode }
        else { conditionCfg.entryNode },
        edges = diffGraphs ++ bodyCfg.edges ++ conditionCfg.edges,
        fringe = conditionCfg.fringe.withEdgeType(FalseEdge) ++ takeCurrentLevel(bodyCfg.breaks).map((_, AlwaysEdge)),
        breaks = reduceAndFilterLevel(bodyCfg.breaks),
        continues = reduceAndFilterLevel(bodyCfg.continues)
      )
  }

  /** CFG creation for while statements of the form `while(condition) body1 else body2` where body1 and the else block
    * are optional.
    */
  protected def cfgForWhileStatement(node: ControlStructure): Cfg = {
    val conditionCfg = node.condition.headOption.map(cfgFor).getOrElse(Cfg.empty)
    val trueCfg      = node.whenTrue.headOption.map(cfgFor).getOrElse(Cfg.empty)
    val falseCfg     = node.whenFalse.headOption.map(cfgFor).getOrElse(Cfg.empty)

    val diffGraphs = edgesFromFringeTo(conditionCfg, trueCfg.entryNode) ++
      edgesFromFringeTo(trueCfg, falseCfg.entryNode) ++
      edgesFromFringeTo(trueCfg, conditionCfg.entryNode) ++
      edges(takeCurrentLevel(trueCfg.continues), conditionCfg.entryNode)

    Cfg
      .from(conditionCfg, trueCfg, falseCfg)
      .copy(
        entryNode = conditionCfg.entryNode,
        edges = diffGraphs ++ conditionCfg.edges ++ trueCfg.edges ++ falseCfg.edges,
        fringe = conditionCfg.fringe.withEdgeType(FalseEdge) ++ takeCurrentLevel(trueCfg.breaks)
          .map((_, AlwaysEdge)) ++ falseCfg.fringe,
        breaks = reduceAndFilterLevel(trueCfg.breaks),
        continues = reduceAndFilterLevel(trueCfg.continues)
      )
  }

  /** CFG creation for switch statements of the form `switch { case condition: ... }`.
    */
  protected def cfgForSwitchStatement(node: ControlStructure): Cfg = {
    val conditionCfg = node.condition.headOption.map(cfgFor).getOrElse(Cfg.empty)
    val bodyCfg      = node.whenTrue.headOption.map(cfgFor).getOrElse(Cfg.empty)

    cfgForSwitchLike(conditionCfg, bodyCfg :: Nil)
  }

  /** CFG creation for if statements of the form `if(condition) body`, optionally followed by `else body2`.
    */
  protected def cfgForIfStatement(node: ControlStructure): Cfg = {
    val conditionCfg = node.condition.headOption.map(cfgFor).getOrElse(Cfg.empty)
    val trueCfg      = node.whenTrue.headOption.map(cfgFor).getOrElse(Cfg.empty)
    val falseCfg     = node.whenFalse.headOption.map(cfgFor).getOrElse(Cfg.empty)

    val diffGraphs = edgesFromFringeTo(conditionCfg, trueCfg.entryNode) ++
      edgesFromFringeTo(conditionCfg, falseCfg.entryNode)

    Cfg
      .from(conditionCfg, trueCfg, falseCfg)
      .copy(
        entryNode = conditionCfg.entryNode,
        edges = diffGraphs ++ conditionCfg.edges ++ trueCfg.edges ++ falseCfg.edges,
        fringe = trueCfg.fringe ++ {
          if (falseCfg.entryNode.isDefined) {
            falseCfg.fringe
          } else {
            conditionCfg.fringe.withEdgeType(FalseEdge)
          }
        }
      )
  }

  /** CFG creation for try statements of the form `try { tryBody ] catch { catchBody } `, optionally followed by
    * `finally { finallyBody }`.
    *
    * To avoid very large CFGs for try statements, only edges from the last statement in the `try` block to each `catch`
    * block (and optionally the `finally` block) are created. The last statement in each `catch` block should then have
    * an outgoing edge to the `finally` block if it exists (and not to any subsequent catch blocks), or otherwise * be
    * part of the fringe.
    *
    * By default, the first child of the `TRY` node is treated as the try body, while every subsequent node is treated
    * as a `catch`, with no `finally` present. To treat the last child of the node as the `finally` block, the `code`
    * field of the `Block` node must be set to `finally`.
    */
  protected def cfgForTryStatement(node: ControlStructure): Cfg = {
    val maybeTryBlock =
      node.astChildren
        .order(1)
        .where(_.astChildren) // Filter out empty `try` bodies
        .headOption

    val tryBodyCfg: Cfg = maybeTryBlock.map(cfgFor).getOrElse(Cfg.empty)

    val catchControlStructures =
      (node.astChildren.isControlStructure.isCatch ++ node.astChildren.isControlStructure.isElse).toList
    val catchBodyCfgs = if (catchControlStructures.isEmpty) {
      node.astChildren.order(2).toList match {
        case Nil  => List(Cfg.empty)
        case asts => asts.map(cfgFor)
      }
    } else {
      catchControlStructures match {
        case Nil  => List(Cfg.empty)
        case asts => asts.map(cfgFor)
      }
    }

    val finallyControlStructures = node.astChildren.isControlStructure.isFinally.toList
    val maybeFinallyBodyCfg = if (catchControlStructures.isEmpty && finallyControlStructures.isEmpty) {
      node.astChildren
        .order(3)
        .map(cfgFor)
        .headOption // Assume there can only be one
        .toList
    } else {
      finallyControlStructures
        .map(cfgFor)
        .headOption // Assume there can only be one
        .toList
    }

    val tryToCatchEdges = catchBodyCfgs.flatMap { catchBodyCfg =>
      edgesFromFringeTo(tryBodyCfg, catchBodyCfg.entryNode)
    }

    val catchToFinallyEdges = (
      for (
        catchBodyCfg   <- catchBodyCfgs;
        finallyBodyCfg <- maybeFinallyBodyCfg
      ) yield edgesFromFringeTo(catchBodyCfg, finallyBodyCfg.entryNode)
    ).flatten

    val tryToFinallyEdges = maybeFinallyBodyCfg.flatMap { cfg =>
      edgesFromFringeTo(tryBodyCfg, cfg.entryNode)
    }

    val diffGraphs = tryToCatchEdges ++ catchToFinallyEdges ++ tryToFinallyEdges

    if (maybeTryBlock.isEmpty) {
      // This case deals with the situation where the try block is empty. In this case,
      // no catch block can be executed since nothing can be thrown, but the finally block
      // will still be executed.
      maybeFinallyBodyCfg.headOption.getOrElse(Cfg.empty)
    } else {
      Cfg
        .from(Seq(tryBodyCfg) ++ catchBodyCfgs ++ maybeFinallyBodyCfg*)
        .copy(
          entryNode = tryBodyCfg.entryNode,
          edges =
            diffGraphs ++ tryBodyCfg.edges ++ catchBodyCfgs.flatMap(_.edges) ++ maybeFinallyBodyCfg.flatMap(_.edges),
          fringe = if (maybeFinallyBodyCfg.flatMap(_.entryNode).nonEmpty) {
            maybeFinallyBodyCfg.head.fringe
          } else {
            tryBodyCfg.fringe ++ catchBodyCfgs.flatMap(_.fringe)
          }
        )
    }
  }

  /** The CFGs for match cases are modeled after PHP match expressions and assumes that a case will always consist of
    * one or more JumpTargets followed by a single expression. The CFG also assumes an implicit `break` at the end of
    * each match case.
    */
  protected def cfgsForMatchCases(body: AstNode): List[Cfg] = {
    body.astChildren
      .foldLeft(List(Cfg.empty)) {
        case (currCfg :: prevCfgs, astNode) =>
          astNode match {
            case jumpTarget: JumpTarget =>
              val jumpCfg = cfgForJumpTarget(jumpTarget)
              (currCfg ++ jumpCfg) :: prevCfgs

            case node: AstNode =>
              val nodeCfg = cfgFor(node)
              Cfg.empty :: (currCfg ++ nodeCfg) :: prevCfgs
          }
        case _ => List.empty
      }
      .reverse
  }

  /** CFG creation for match expressions of the form `match { case condition: expr ... }`
    */
  protected def cfgForMatchExpression(node: ControlStructure): Cfg = {
    val conditionCfg = node.condition.headOption.map(cfgFor).getOrElse(Cfg.empty)
    val bodyCfgs     = node.whenTrue.headOption.map(cfgsForMatchCases).getOrElse(Nil)

    cfgForSwitchLike(conditionCfg, bodyCfgs)
  }

  protected def cfgForSwitchLike(conditionCfg: Cfg, bodyCfgs: List[Cfg]): Cfg = {
    val hasDefaultCase = bodyCfgs.flatMap(_.caseLabels).exists(x => x.asInstanceOf[JumpTarget].name == "default")
    val caseEdges      = edgesToMultiple(conditionCfg.fringe.map(_._1), bodyCfgs.flatMap(_.caseLabels), CaseEdge)
    val breakFringe    = takeCurrentLevel(bodyCfgs.flatMap(_.breaks)).map((_, AlwaysEdge))

    Cfg
      .from(conditionCfg :: bodyCfgs*)
      .copy(
        entryNode = conditionCfg.entryNode,
        edges = caseEdges ++ conditionCfg.edges ++ bodyCfgs.flatMap(_.edges),
        fringe = {
          if (!hasDefaultCase) { conditionCfg.fringe.withEdgeType(FalseEdge) }
          else { Nil }
        } ++ breakFringe ++ bodyCfgs.flatMap(_.fringe),
        caseLabels = List(),
        breaks = reduceAndFilterLevel(bodyCfgs.flatMap(_.breaks)),
        continues = bodyCfgs.flatMap(_.continues)
      )
  }
}

object CfgCreator {

  implicit class FringeWrapper(fringe: List[(CfgNode, CfgEdgeType)]) {
    def withEdgeType(edgeType: CfgEdgeType): List[(CfgNode, CfgEdgeType)] = {
      fringe.map { case (x, _) => (x, edgeType) }
    }
  }

}
