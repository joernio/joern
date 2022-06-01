package io.joern.x2cpg.passes.controlflow.cfgcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

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
  * This tree can be translated into a control flow graph, by translating the sub tree rooted in `x < 10` and that of `x
  * += 1` and connecting their control flow graphs according to the semantics of `if`:
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
  *
  * Since functional-style recursive accumulations have a tendency to end up in worse complexity classes, we use a
  * mutable accumulator for the edges.
  */
class CfgCreator(entryNode: Method, diffGraph: DiffGraphBuilder) {

  import Cfg._
  import CfgCreator._

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
    val acc = new CfgAccumulator()
    cfgForMethod(acc, entryNode)
    for ((node, jumpLabel) <- acc.jumpsToLabel) {
      if (jumpLabel == "*") {
        // We come here for: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
        // For such GOTOs we cannot statically determine the target label. As a quick
        // hack we simply put edges to all labels found. This might be an over-taint.
        for (dst <- acc.labeledNodes.valuesIterator) {
          acc.addEdges(edges(node :: Nil, Some(dst)))
        }
      } else {
        acc.labeledNodes.get(jumpLabel) match {
          case Some(dst) =>
            acc.addEdges(edges(node :: Nil, Some(dst)))
          case _ =>
            logger.info(s"Unable to wire jump statement. Missing label ${jumpLabel}.")
        }
      }
    }
    for (edge <- acc.edgeBuffer) {
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
  private def cfgForMethod(acc: CfgAccumulator, node: Method): Cfg =
    cfgForSingleNode(node).connect(acc, cfgForChildren(acc, node))

  /** For any single AST node, we can construct a CFG containing that single node by setting it as the entry node and
    * placing it in the fringe.
    */
  private def cfgForSingleNode(node: CfgNode): Cfg =
    Cfg(entryNode = Some(node), fringe = List((node, AlwaysEdge)))

  private def cfgForSingleNodeNoFringe(node: CfgNode): Cfg = Cfg(entryNode = Some(node))

  /** The CFG for all children is obtained by translating child ASTs one by one from left to right and appending them.
    */
  private def cfgForChildren(acc: CfgAccumulator, node: AstNode): Cfg =
    Cfg.empty.connect(acc, node.astChildren.l.map { cfgFor(acc, _) }: _*)

  /** This method dispatches AST nodes by type and calls corresponding conversion methods.
    */
  protected def cfgFor(acc: CfgAccumulator, node: AstNode): Cfg =
    node match {
      case _: Method | _: MethodParameterIn | _: Modifier | _: Local | _: TypeDecl | _: Member =>
        Cfg.empty
      case _: MethodRef | _: TypeRef | _: MethodReturn =>
        cfgForSingleNode(node.asInstanceOf[CfgNode])
      case n: ControlStructure =>
        cfgForControlStructure(acc, n)
      case n: JumpTarget =>
        cfgForJumpTarget(acc, n)
      case actualRet: Return =>
        cfgForReturn(acc, actualRet)
      case call: Call if call.name == Operators.logicalAnd =>
        cfgForAndExpression(acc, call)
      case call: Call if call.name == Operators.logicalOr =>
        cfgForOrExpression(acc, call)
      case call: Call if call.name == Operators.conditional =>
        cfgForConditionalExpression(acc, call)
      case call: Call if call.dispatchType == DispatchTypes.INLINED =>
        cfgForInlinedCall(acc, call)
      case _: Call | _: FieldIdentifier | _: Identifier | _: Literal | _: Unknown =>
        cfgForChildren(acc, node).connect(acc, cfgForSingleNode(node.asInstanceOf[CfgNode]))
      case _ =>
        cfgForChildren(acc, node)
    }

  /** A second layer of dispatching for control structures. This could as well be part of `cfgFor` and has only been
    * placed into a separate function to increase readability.
    */
  protected def cfgForControlStructure(acc: CfgAccumulator, node: ControlStructure): Cfg =
    node.controlStructureType match {
      case ControlStructureTypes.BREAK =>
        cfgForBreakStatement(acc, node)
      case ControlStructureTypes.CONTINUE =>
        cfgForContinueStatement(acc, node)
      case ControlStructureTypes.WHILE =>
        cfgForWhileStatement(acc, node)
      case ControlStructureTypes.DO =>
        cfgForDoStatement(acc, node)
      case ControlStructureTypes.FOR =>
        cfgForForStatement(acc, node)
      case ControlStructureTypes.GOTO =>
        cfgForGotoStatement(acc, node)
      case ControlStructureTypes.IF =>
        cfgForIfStatement(acc, node)
      case ControlStructureTypes.ELSE =>
        cfgForChildren(acc, node)
      case ControlStructureTypes.SWITCH =>
        cfgForSwitchStatement(acc, node)
      case ControlStructureTypes.TRY =>
        cfgForTryStatement(acc, node)
      case _ =>
        Cfg.empty
    }

  /** The CFG for a break/continue statements contains only the break/continue statement as a single entry node. The
    * fringe is empty, that is, appending another CFG to the break statement will not result in the creation of an edge
    * from the break statement to the entry point of the other CFG. Labeled breaks are treated like gotos and are added
    * to "jumpsToLabel".
    */
  protected def cfgForBreakStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel) =>
        val labelName = jumpLabel.asInstanceOf[JumpLabel].name
        acc.jumpsToLabel.append((node, labelName))
      case None =>
        acc.breaks.append(node)
    }
    cfgForSingleNodeNoFringe(node)
  }

  protected def cfgForContinueStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel) =>
        val labelName = jumpLabel.asInstanceOf[JumpLabel].name
        acc.jumpsToLabel.append((node, labelName))
      case None =>
        acc.continues.append(node)
    }
    cfgForSingleNodeNoFringe(node)
  }

  /** Jump targets ("labels") are included in the CFG. As these should be connected to the next appended CFG, we specify
    * that the label node is both the entry node and the only node in the fringe. This is achieved by calling
    * `cfgForSingleNode` on the label node. Just like for breaks and continues, we record labels. We store case/default
    * labels separately from other labels, but that is not a relevant implementation detail.
    */
  protected def cfgForJumpTarget(acc: CfgAccumulator, n: JumpTarget): Cfg = {
    val labelName = n.name
    if (labelName.startsWith("case") || labelName.startsWith("default")) {
      acc.caseLabels.append(n)
    } else {
      // fixme: We should check whether the label already exists and warn if so
      acc.labeledNodes.put(labelName, n)
    }
    cfgForSingleNode(n)
  }

  /** A CFG for a goto statement is one containing the goto node as an entry node and an empty fringe. Moreover, we
    * store the goto for dispatching with `withResolvedJumpToLabel` once the CFG for the entire method has been
    * calculated.
    */
  protected def cfgForGotoStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    val labelName = node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel) =>
        jumpLabel.asInstanceOf[JumpLabel].name
      case None =>
        // Support for old format where the label name is parsed from the code field.
        node.code.split(" ").lastOption.map(x => x.slice(0, x.length - 1)).get
    }
    acc.jumpsToLabel.append((node, labelName))
    cfgForSingleNodeNoFringe(node)
  }

  /** Return statements may contain expressions as return values, and therefore, the CFG for a return statement consists
    * of the CFG for calculation of that expression, appended to a CFG containing only the return node, connected with a
    * single edge to the method exit node. The fringe is empty.
    */
  protected def cfgForReturn(acc: CfgAccumulator, actualRet: Return): Cfg = {
    acc.addEdges(singleEdge(actualRet, exitNode))
    cfgForChildren(acc, actualRet).connect(acc, cfgForSingleNodeNoFringe(actualRet))
  }

  /** The right hand side of a logical AND expression is only evaluated if the left hand side is true as the entire
    * expression can only be true if both expressions are true. This is encoded in the corresponding control flow graph
    * by creating control flow graphs for the left and right hand expressions and appending the two, where the fringe
    * edge type of the left CFG is `TrueEdge`.
    */
  protected def cfgForAndExpression(acc: CfgAccumulator, call: Call): Cfg = {
    val leftCfg  = cfgFor(acc, call.argument(1))
    val rightCfg = cfgFor(acc, call.argument(2))
    val callCfg  = cfgForSingleNode(call)
    leftCfg.withFringeEdgeType(TrueEdge).connect(acc, rightCfg)
    rightCfg.connect(acc, callCfg)
    leftCfg.withFringeEdgeType(FalseEdge).connect(acc, callCfg)
  }

  /** Same construction recipe as for the AND expression, just that the fringe edge type of the left CFG is `FalseEdge`.
    */
  protected def cfgForOrExpression(acc: CfgAccumulator, call: Call): Cfg = {
    val leftCfg  = cfgFor(acc, call.argument(1))
    val rightCfg = cfgFor(acc, call.argument(2))
    val callCfg  = cfgForSingleNode(call)
    leftCfg.withFringeEdgeType(FalseEdge).connect(acc, rightCfg)
    rightCfg.connect(acc, callCfg)
    leftCfg.withFringeEdgeType(TrueEdge).connect(acc, callCfg)
  }

  /** A conditional expression is of the form `condition ? trueExpr ; falseExpr` where both `trueExpr` and `falseExpr`
    * are optional. We create the corresponding CFGs by creating CFGs for the three expressions and adding edges between
    * them. The new entry node is the condition entry node.
    */
  protected def cfgForConditionalExpression(acc: CfgAccumulator, call: Call): Cfg = {
    val conditionCfg = cfgFor(acc, call.argument(1))
    val callCfg      = cfgForSingleNode(call)
    conditionCfg
      .withFringeEdgeType(TrueEdge)
      .connect(acc, (call.argumentOption(2).map(cfgFor(acc, _)).toList :+ callCfg): _*)
    conditionCfg
      .withFringeEdgeType(FalseEdge)
      .connect(acc, (call.argumentOption(3).map(cfgFor(acc, _)).toList :+ callCfg): _*)
    Cfg(entryNode = conditionCfg.entryNode, fringe = callCfg.fringe)
  }

  /** For macros, the AST contains a CALL node, along with child sub trees for all arguments, and a final sub tree that
    * contains the inlined code. The corresponding CFG consists of the CFG for the call, an edge to the exit and an edge
    * to the CFG of the inlined code. We choose this representation because it allows both queries that use the macro
    * reference as well as queries that reference the inline code to be chosen as sources/sinks in data flow queries.
    */
  def cfgForInlinedCall(acc: CfgAccumulator, call: Call): Cfg = {
    val cfgForMacroCall =
      Cfg.empty.connect(acc, call.argument.l.map { cfgFor(acc, _) }: _*).connect(acc, cfgForSingleNode(call))
    val cfgForExpansion = call.astChildren.lastOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    cfgForMacroCall.connect(acc, cfgForExpansion).copy(fringe = cfgForMacroCall.fringe ++ cfgForExpansion.fringe)
  }

  /** A for statement is of the form `for(initExpr; condition; loopExpr) body` and all four components may be empty. The
    * sequence (condition - body - loopExpr) form the inner part of the loop and we calculate the corresponding CFG
    * `innerCfg` so that it is no longer relevant which of these three actually exist and we still have an entry node
    * for the loop and a fringe.
    *
    * Python-style for/else is not supported.
    */
  protected def cfgForForStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    val children     = node.astChildren.l
    val nLocals      = children.count(_.isLocal)
    val initExprCfg  = children.find(_.order == nLocals + 1).map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val conditionCfg = children.find(_.order == nLocals + 2).map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val loopExprCfg  = children.find(_.order == nLocals + 3).map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val oldBreaks    = acc.swapBreaks()
    val oldContinues = acc.swapContinues()
    val bodyCfg      = children.find(_.order == nLocals + 4).map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val breaks       = acc.swapBreaks(oldBreaks)
    val continues    = acc.swapContinues(oldContinues)

    val innerCfg = conditionCfg.withFringeEdgeType(TrueEdge).connect(acc, bodyCfg).connect(acc, loopExprCfg)
    innerCfg.connect(acc, innerCfg)
    val continueTarget = if (loopExprCfg.entryNode.isDefined) loopExprCfg.entryNode else innerCfg.entryNode
    val entryNode      = initExprCfg.connect(acc, innerCfg).entryNode
    acc.addEdges(edges(continues.toList, continueTarget))
    Cfg(
      entryNode = entryNode,
      fringe = breaks.toList.map { (_, AlwaysEdge) } ++ conditionCfg.withFringeEdgeType(FalseEdge).fringe
    )
  }

  /** A Do-Statement is of the form `do body while(condition)` where body may be empty. We again first calculate the
    * inner CFG as bodyCfg ++ conditionCfg and then connect edges according to the semantics of do-while.
    */
  protected def cfgForDoStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    val (oldContinues, oldBreaks) = (acc.swapContinues(), acc.swapBreaks())
    val bodyCfg             = node.astChildren.where(_.order(1)).headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val (continues, breaks) = (acc.swapContinues(oldContinues), acc.swapBreaks(oldBreaks))
    val conditionCfg        = Traversal.fromSingle(node).condition.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val innerCfg            = bodyCfg.connect(acc, conditionCfg)
    innerCfg.withFringeEdgeType(TrueEdge).connect(acc, innerCfg)
    val continueTarget = if (conditionCfg.entryNode.isDefined) conditionCfg.entryNode else innerCfg.entryNode
    acc.addEdges(edges(continues.toList, continueTarget, AlwaysEdge))
    Cfg(
      entryNode = innerCfg.entryNode,
      fringe = innerCfg.withFringeEdgeType(FalseEdge).fringe ++ breaks.toList.map((_, FalseEdge))
    )
  }

  /** CFG creation for while statements of the form `while(condition) body1 else body2` where body1 and the else block
    * are optional. (the else block is Python-style)
    */
  protected def cfgForWhileStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    val conditionCfg              = Traversal.fromSingle(node).condition.headOption.map(cfgFor(acc, _)).get
    val (oldContinues, oldBreaks) = (acc.swapContinues(), acc.swapBreaks())
    val bodyCfg             = Traversal.fromSingle(node).whenTrue.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val (continues, breaks) = (acc.swapContinues(oldContinues), acc.swapBreaks(oldBreaks))
    val elseCfg             = Traversal.fromSingle(node).whenFalse.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    conditionCfg.withFringeEdgeType(TrueEdge).connect(acc, bodyCfg).connect(acc, conditionCfg)
    acc.addEdges(edges(continues.toList, conditionCfg.entryNode, AlwaysEdge))
    val res = conditionCfg.withFringeEdgeType(FalseEdge).connect(acc, elseCfg)
    res.copy(fringe = res.fringe ++ breaks.toList.map((_, AlwaysEdge)))
  }

  /** CFG creation for switch statements of the form `switch(condition) { case caseLabel: ... }`.
    */
  protected def cfgForSwitchStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    val conditionCfg = Traversal.fromSingle(node).condition.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val (oldCaseLabels, oldBreaks) = (acc.swapCaseLabels(), acc.swapBreaks())
    val bodyCfg              = Traversal.fromSingle(node).whenTrue.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val (caseLabels, breaks) = (acc.swapCaseLabels(oldCaseLabels), acc.swapBreaks(oldBreaks))

    acc.addEdges(edgesToMultiple(conditionCfg.fringe.map(_._1), caseLabels.toList, CaseEdge))
    val hasDefaultCase = caseLabels.exists(x => x.asInstanceOf[JumpTarget].name == "default")
    Cfg(
      entryNode = conditionCfg.entryNode,
      fringe = (if (hasDefaultCase) Nil
                else
                  conditionCfg.fringe.withEdgeType(FalseEdge)) ++ bodyCfg.fringe ++ breaks.toList.map((_, AlwaysEdge))
    )
  }

  /** CFG creation for if statements of the form `if(condition) body`, optionally followed by `else body2`.
    */
  protected def cfgForIfStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    val conditionCfg = Traversal.fromSingle(node).condition.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val trueCfg      = Traversal.fromSingle(node).whenTrue.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    val falseCfg     = Traversal.fromSingle(node).whenFalse.headOption.map(cfgFor(acc, _)).getOrElse(Cfg.empty)
    conditionCfg.withFringeEdgeType(TrueEdge).connect(acc, trueCfg)
    Cfg(
      entryNode = conditionCfg.entryNode,
      fringe = trueCfg.fringe ++ conditionCfg.withFringeEdgeType(FalseEdge).connect(acc, falseCfg).fringe
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
  protected def cfgForTryStatement(acc: CfgAccumulator, node: ControlStructure): Cfg = {
    // fixme: This code does not do what the docstring says. Which one is correct?
    val maybeTryBlock =
      Traversal
        .fromSingle(node)
        .astChildren
        .where(_.order(1))
        .where(_.astChildren) // Filter out empty `try` bodies
        .headOption

    val tryBodyCfg: Cfg =
      maybeTryBlock
        .map(cfgFor(acc, _))
        .getOrElse(Cfg.empty)

    val catchBodyCfgs: List[Cfg] =
      Traversal
        .fromSingle(node)
        .astChildren
        .where(_.order(2))
        .toList
        .map(cfgFor(acc, _))

    var fringeBuffer = List.empty[(CfgNode, CfgEdgeType)]

    val maybeFinallyBodyCfg =
      Traversal
        .fromSingle(node)
        .astChildren
        .where(_.order(3))
        .map(cfgFor(acc, _))
        .headOption // Assume there can only be one
    catchBodyCfgs.foreach { catchBody =>
      tryBodyCfg.connect(acc, catchBody)
      if (maybeFinallyBodyCfg.isDefined)
        catchBody.connect(acc, maybeFinallyBodyCfg.get)
      else fringeBuffer = catchBody.fringe ++ fringeBuffer
    }
    if (maybeFinallyBodyCfg.isDefined)
      tryBodyCfg.connect(acc, maybeFinallyBodyCfg.get)
    else if (!tryBodyCfg.isEmpty) Cfg(entryNode = tryBodyCfg.entryNode, fringe = tryBodyCfg.fringe ++ fringeBuffer)
    else Cfg.empty
  }
}

object CfgCreator {
  private val logger = LoggerFactory.getLogger(getClass)

  implicit class FringeWrapper(fringe: List[(CfgNode, CfgEdgeType)]) {
    def withEdgeType(edgeType: CfgEdgeType): List[(CfgNode, CfgEdgeType)] = {
      fringe.map { case (x, _) => (x, edgeType) }
    }
  }

}
