package io.joern.x2cpg.passes.controlflow.cfgcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.controlflow.cfgcreation.CfgEdgeType
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.Traversal

import scala.collection.mutable

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
  */
class CfgCreator(entryNode: Method, diffGraph: DiffGraphBuilder) {

  private val edgeBuffer = mutable.ArrayBuffer[CfgEdge]()
  private val jumpToLabelBuffer = mutable.ArrayBuffer[(CfgNode, String)]()
  private val labeledNodes = mutable.HashMap[String, CfgNode]()
  private val logger = LoggerFactory.getLogger(getClass)


  /** A control flow graph that is under construction, consisting of:
    *
    * @param entryNode
    *   the control flow graph's first node, that is, the node to which a CFG that appends this CFG should attach itself
    *   to.
    * @param edges
    *   control flow edges between nodes of the code property graph.
    * @param fringe
    *   nodes of the CFG for which an outgoing edge type is already known but the destination node is not. These nodes are
    *   connected when another CFG is appended to this CFG.
    *
    * In addition to these three core building blocks, we store labels and jump statements that have not been resolved and
    * may be resolvable as parent sub trees or sibblings are translated.
    *
    * @param labeledNodes
    *   labels contained in the abstract syntax tree from which this CPG was generated
    * @param caseLabels
    *   labels beginning with "case"
    * @param breaks
    *   unresolved breaks collected along the way
    * @param continues
    *   unresolved continues collected along the way
    * @param jumpsToLabel
    *   unresolved gotos, labeled break and labeld continues collected along the way
    */
  case class CfgContext(
                  entryNode: Option[CfgNode] = None,
                  fringe: List[(CfgNode, CfgEdgeType)] = List(),
                  breaks: List[CfgNode] = List(),
                  continues: List[CfgNode] = List(),
                  caseLabels: List[CfgNode] = List()) {

    import CfgContext._


    /** Create a new CFG in which `other` is appended to this CFG. All nodes of the fringe are connected to `other`'s
      * entry node and the new fringe is `other`'s fringe. The diffgraphs, jumps, and labels are the sum of those present
      * in `this` and `other`.
      */
    def ++(other: CfgContext): CfgContext = {
      if (other eq CfgContext.empty) {
        this
      } else if (this eq CfgContext.empty) {
        other
      } else {
        edgeBuffer.appendAll(edgesFromFringeTo(this, other.entryNode))
        this.copy(
          fringe = other.fringe,
          breaks = this.breaks ++ other.breaks,
          continues = this.continues ++ other.continues,
          caseLabels = this.caseLabels ++ other.caseLabels
        )
      }
    }

    def withFringeEdgeType(cfgEdgeType: CfgEdgeType): CfgContext = {
      this.copy(fringe = fringe.map { case (x, _) => (x, cfgEdgeType) })
    }
  }


  object CfgContext {

    def from(cfgs: CfgContext*): CfgContext = {
      CfgContext(
        breaks = cfgs.map(_.breaks).reduceOption((x, y) => x ++ y).getOrElse(List()),
        continues = cfgs.map(_.continues).reduceOption((x, y) => x ++ y).getOrElse(List()),
        caseLabels = cfgs.map(_.caseLabels).reduceOption((x, y) => x ++ y).getOrElse(List()),
      )
    }

    /** The safe "null" Cfg.
      * Construction cannot use default arguments, cf https://github.com/scala/bug/issues/12174
      */
    val empty: CfgContext = new CfgContext(None, Nil, Nil, Nil, Nil)



    /** Create edges from all nodes of cfg's fringe to `node`.
      */
    def edgesFromFringeTo(cfg: CfgContext, node: Option[CfgNode]): List[CfgEdge] = {
      edgesFromFringeTo(cfg.fringe, node)
    }

    /** Create edges from all nodes of cfg's fringe to `node`, ignoring fringe edge types and using `cfgEdgeType` instead.
      */
    def edgesFromFringeTo(cfg: CfgContext, node: Option[CfgNode], cfgEdgeType: CfgEdgeType): List[CfgEdge] = {
      edges(cfg.fringe.map(_._1), node, cfgEdgeType)
    }

    /** Create edges from a list (node, cfgEdgeType) pairs to `node`
      */
    def edgesFromFringeTo(fringeElems: List[(CfgNode, CfgEdgeType)], node: Option[CfgNode]): List[CfgEdge] = {
      fringeElems.flatMap { case (sourceNode, cfgEdgeType) =>
        node.map { dstNode =>
          CfgEdge(sourceNode, dstNode, cfgEdgeType)
        }
      }
    }

    /** Create edges of given type from a list of source nodes to a destination node
      */
    def edges(sources: List[CfgNode], dstNode: Option[CfgNode], cfgEdgeType: CfgEdgeType = AlwaysEdge): List[CfgEdge] = {
      edgesToMultiple(sources, dstNode.toList, cfgEdgeType)
    }

    def singleEdge(source: CfgNode, destination: CfgNode, cfgEdgeType: CfgEdgeType = AlwaysEdge): List[CfgEdge] = {
      edgesToMultiple(List(source), List(destination), cfgEdgeType)
    }

    /** Create edges of given type from all nodes in `sources` to `node`.
      */
    def edgesToMultiple(
                         sources: List[CfgNode],
                         destinations: List[CfgNode],
                         cfgEdgeType: CfgEdgeType = AlwaysEdge
                       ): List[CfgEdge] = {

      sources.flatMap { l =>
        destinations.map { n =>
          CfgEdge(l, n, cfgEdgeType)
        }
      }
    }
  }


  import CfgContext._
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

  /** Upon completing traversal of the abstract syntax tree, this method creates CFG edges between jumps like gotos,
    * labeled breaks, labeled continues and respective labels.
    */
  def resolveJumpsToLabels: Unit = {
    for((node, label) <- jumpToLabelBuffer){
      if(label != "*")
      labeledNodes.get(label) match {
        case None => logger.info(s"Unable to wire jump statement. Missing label ${label}.")
        case Some(target) => edgeBuffer.append(CfgEdge(node, target, AlwaysEdge))
    } else {
        // We come here for: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
        // For such GOTOs we cannot statically determine the target label. As a quick
        // hack we simply put edges to all labels found. This might be an over-taint.
        labeledNodes.flatMap { case (_, labeledNode) =>
          Some(CfgEdge(node, labeledNode, AlwaysEdge))
        }
      }
  }
    jumpToLabelBuffer.clear()
  }

  /** We return the CFG as a sequence of Diff Graphs that is calculated by first obtaining the CFG for the method and
    * then resolving gotos.
    */
  def run(): Unit = {
    cfgForMethod(entryNode)
    resolveJumpsToLabels
    edgeBuffer.foreach { edge =>
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
  private def cfgForMethod(node: Method): CfgContext =
    cfgForSingleNode(node) ++ cfgForChildren(node)

  /** For any single AST node, we can construct a CFG containing that single node by setting it as the entry node and
    * placing it in the fringe.
    */
  private def cfgForSingleNode(node: CfgNode): CfgContext =
    CfgContext(entryNode = Some(node), fringe = List((node, AlwaysEdge)))

  /** The CFG for all children is obtained by translating child ASTs one by one from left to right and appending them.
    */
  private def cfgForChildren(node: AstNode): CfgContext =
    node.astChildren.l.map(cfgFor).reduceOption((x, y) => x ++ y).getOrElse(CfgContext.empty)

  /** This method dispatches AST nodes by type and calls corresponding conversion methods.
    */
  protected def cfgFor(node: AstNode): CfgContext =
    node match {
      case _: Method | _: MethodParameterIn | _: Modifier | _: Local | _: TypeDecl | _: Member =>
        CfgContext.empty
      case _: MethodRef | _: TypeRef | _: MethodReturn =>
        cfgForSingleNode(node.asInstanceOf[CfgNode])
      case n: ControlStructure =>
        cfgForControlStructure(n)
      case n: JumpTarget =>
        cfgForJumpTarget(n)
      case actualRet: Return =>
        cfgForReturn(actualRet)
      case call: Call if call.name == Operators.logicalAnd =>
        cfgForAndExpression(call)
      case call: Call if call.name == Operators.logicalOr =>
        cfgForOrExpression(call)
      case call: Call if call.name == Operators.conditional =>
        cfgForConditionalExpression(call)
      case call: Call if call.dispatchType == DispatchTypes.INLINED =>
        cfgForInlinedCall(call)
      case _: Call | _: FieldIdentifier | _: Identifier | _: Literal | _: Unknown =>
        cfgForChildren(node) ++ cfgForSingleNode(node.asInstanceOf[CfgNode])
      case _ =>
        cfgForChildren(node)
    }

  /** A second layer of dispatching for control structures. This could as well be part of `cfgFor` and has only been
    * placed into a separate function to increase readability.
    */
  protected def cfgForControlStructure(node: ControlStructure): CfgContext =
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
      case _ =>
        CfgContext.empty
    }

  /** The CFG for a break/continue statements contains only the break/continue statement as a single entry node. The
    * fringe is empty, that is, appending another CFG to the break statement will not result in the creation of an edge
    * from the break statement to the entry point of the other CFG. Labeled breaks are treated like gotos and are added
    * to "jumpsToLabel".
    */
  protected def cfgForBreakStatement(node: ControlStructure): CfgContext = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel) =>
        val labelName = jumpLabel.asInstanceOf[JumpLabel].name
        jumpToLabelBuffer.append((node, labelName))
        CfgContext(entryNode = Some(node))
      case None =>
        CfgContext(entryNode = Some(node), breaks = List(node))
    }
  }

  protected def cfgForContinueStatement(node: ControlStructure): CfgContext = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel) =>
        val labelName = jumpLabel.asInstanceOf[JumpLabel].name
        jumpToLabelBuffer.append((node, labelName))
        CfgContext(entryNode = Some(node))
      case None =>
        CfgContext(entryNode = Some(node), continues = List(node))
    }
  }

  /** Jump targets ("labels") are included in the CFG. As these should be connected to the next appended CFG, we specify
    * that the label node is both the entry node and the only node in the fringe. This is achieved by calling
    * `cfgForSingleNode` on the label node. Just like for breaks and continues, we record labels. We store case/default
    * labels separately from other labels, but that is not a relevant implementation detail.
    */
  protected def cfgForJumpTarget(n: JumpTarget): CfgContext = {
    val labelName = n.name
    val cfg       = cfgForSingleNode(n)
    if (labelName.startsWith("case") || labelName.startsWith("default")) {
      cfg.copy(caseLabels = List(n))
    } else {
      labeledNodes.put(labelName, n)
      cfg
    }
  }

  /** A CFG for a goto statement is one containing the goto node as an entry node and an empty fringe. Moreover, we
    * store the goto for dispatching with `withResolvedJumpToLabel` once the CFG for the entire method has been
    * calculated.
    */
  protected def cfgForGotoStatement(node: ControlStructure): CfgContext = {
    node.astChildren.find(_.order == 1) match {
      case Some(jumpLabel) =>
        val labelName = jumpLabel.asInstanceOf[JumpLabel].name
        jumpToLabelBuffer.append((node, labelName))
        CfgContext(entryNode = Some(node))
      case None =>
        // Support for old format where the label name is parsed from the code field.
        val labelName = node.code.split(" ").lastOption.map(x => x.slice(0, x.length - 1)).get
        jumpToLabelBuffer.append((node, labelName))
        CfgContext(entryNode = Some(node))
    }
  }

  /** Return statements may contain expressions as return values, and therefore, the CFG for a return statement consists
    * of the CFG for calculation of that expression, appended to a CFG containing only the return node, connected with a
    * single edge to the method exit node. The fringe is empty.
    */
  protected def cfgForReturn(actualRet: Return): CfgContext = {
    edgeBuffer.appendAll(singleEdge(actualRet, exitNode))
    cfgForChildren(actualRet) ++
      CfgContext(entryNode = Some(actualRet), fringe = Nil)
  }

  /** The right hand side of a logical AND expression is only evaluated if the left hand side is true as the entire
    * expression can only be true if both expressions are true. This is encoded in the corresponding control flow graph
    * by creating control flow graphs for the left and right hand expressions and appending the two, where the fringe
    * edge type of the left CFG is `TrueEdge`.
    */
  protected def cfgForAndExpression(call: Call): CfgContext = {
    val leftCfg    = cfgFor(call.argument(1))
    val rightCfg   = cfgFor(call.argument(2))
    edgeBuffer.appendAll(edgesFromFringeTo(leftCfg, rightCfg.entryNode, TrueEdge))
    CfgContext
      .from(leftCfg, rightCfg)
      .copy(
        entryNode = leftCfg.entryNode,
        fringe = leftCfg.fringe ++ rightCfg.fringe
      ) ++ cfgForSingleNode(call)
  }

  /** Same construction recipe as for the AND expression, just that the fringe edge type of the left CFG is `FalseEdge`.
    */
  protected def cfgForOrExpression(call: Call): CfgContext = {
    val leftCfg    = cfgFor(call.argument(1))
    val rightCfg   = cfgFor(call.argument(2))
    edgeBuffer.appendAll(edgesFromFringeTo(leftCfg, rightCfg.entryNode, TrueEdge))
    CfgContext
      .from(leftCfg, rightCfg)
      .copy(
        entryNode = leftCfg.entryNode,
        fringe = leftCfg.fringe ++ rightCfg.fringe
      ) ++ cfgForSingleNode(call)
  }

  /** A conditional expression is of the form `condition ? trueExpr ; falseExpr` where both `trueExpr` and `falseExpr`
    * are optional. We create the corresponding CFGs by creating CFGs for the three expressions and adding edges between
    * them. The new entry node is the condition entry node.
    */
  protected def cfgForConditionalExpression(call: Call): CfgContext = {
    val conditionCfg = cfgFor(call.argument(1))
    val trueCfg      = call.argumentOption(2).map(cfgFor).getOrElse(CfgContext.empty)
    val falseCfg     = call.argumentOption(3).map(cfgFor).getOrElse(CfgContext.empty)
    edgeBuffer.appendAll(edgesFromFringeTo(conditionCfg, trueCfg.entryNode, TrueEdge) ++
      edgesFromFringeTo(conditionCfg, falseCfg.entryNode, FalseEdge))

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

    CfgContext
      .from(conditionCfg, trueCfg, falseCfg)
      .copy(
        entryNode = conditionCfg.entryNode,
        fringe = trueFridge ++ falseFridge
      ) ++ cfgForSingleNode(call)
  }

  /** For macros, the AST contains a CALL node, along with child sub trees for all arguments, and a final sub tree that
    * contains the inlined code. The corresponding CFG consists of the CFG for the call, an edge to the exit and an edge
    * to the CFG of the inlined code. We choose this representation because it allows both queries that use the macro
    * reference as well as queries that reference the inline code to be chosen as sources/sinks in data flow queries.
    */
  def cfgForInlinedCall(call: Call): CfgContext = {
    val cfgForMacroCall = call.argument.l
      .map(cfgFor)
      .reduceOption((x, y) => x ++ y)
      .getOrElse(CfgContext.empty) ++ cfgForSingleNode(call)
    val cfgForExpansion = call.astChildren.lastOption.map(cfgFor).getOrElse(CfgContext.empty)
    val cfg = CfgContext
      .from(cfgForMacroCall, cfgForExpansion)
      .copy(
        entryNode = cfgForMacroCall.entryNode,
        fringe = cfgForMacroCall.fringe ++ cfgForExpansion.fringe
      )
    cfgForExpansion.entryNode.toList
      .foreach(x => edgeBuffer.appendAll(singleEdge(call, x)))
    cfg
  }

  /** A for statement is of the form `for(initExpr; condition; loopExpr) body` and all four components may be empty. The
    * sequence (condition - body - loopExpr) form the inner part of the loop and we calculate the corresponding CFG
    * `innerCfg` so that it is no longer relevant which of these three actually exist and we still have an entry node
    * for the loop and a fringe.
    */
  protected def cfgForForStatement(node: ControlStructure): CfgContext = {
    val children     = node.astChildren.l
    val nLocals      = children.count(_.isLocal)
    val initExprCfg  = children.find(_.order == nLocals + 1).map(cfgFor).getOrElse(CfgContext.empty)
    val conditionCfg = children.find(_.order == nLocals + 2).map(cfgFor).getOrElse(CfgContext.empty)
    val loopExprCfg  = children.find(_.order == nLocals + 3).map(cfgFor).getOrElse(CfgContext.empty)
    val bodyCfg      = children.find(_.order == nLocals + 4).map(cfgFor).getOrElse(CfgContext.empty)

    val innerCfg  = conditionCfg ++ bodyCfg ++ loopExprCfg
    val entryNode = if(initExprCfg.entryNode.isDefined) initExprCfg.entryNode else innerCfg.entryNode

    val newEdges = edgesFromFringeTo(initExprCfg, innerCfg.entryNode) ++
      edgesFromFringeTo(innerCfg, innerCfg.entryNode) ++
      edgesFromFringeTo(conditionCfg, bodyCfg.entryNode, TrueEdge) ++ {
        if (loopExprCfg.entryNode.isDefined) {
          edges(bodyCfg.continues, loopExprCfg.entryNode)
        } else {
          edges(bodyCfg.continues, innerCfg.entryNode)
        }
      }
    edgeBuffer.appendAll(newEdges)
    CfgContext
      .from(initExprCfg, conditionCfg, loopExprCfg, bodyCfg)
      .copy(
        entryNode = entryNode,
        fringe = conditionCfg.fringe.withEdgeType(FalseEdge) ++ bodyCfg.breaks.map((_, AlwaysEdge))
      )
  }

  /** A Do-Statement is of the form `do body while(condition)` where body may be empty. We again first calculate the
    * inner CFG as bodyCfg ++ conditionCfg and then connect edges according to the semantics of do-while.
    */
  protected def cfgForDoStatement(node: ControlStructure): CfgContext = {
    val bodyCfg      = node.astChildren.where(_.order(1)).headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val conditionCfg = Traversal.fromSingle(node).condition.headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val innerCfg     = bodyCfg ++ conditionCfg

    val diffGraphs =
      edges(bodyCfg.continues, conditionCfg.entryNode) ++
        edgesFromFringeTo(bodyCfg, conditionCfg.entryNode) ++
        edgesFromFringeTo(conditionCfg, innerCfg.entryNode, TrueEdge)

    edgeBuffer.appendAll(diffGraphs)
    CfgContext
      .from(bodyCfg, conditionCfg, innerCfg)
      .copy(
        entryNode = if (bodyCfg != CfgContext.empty) { bodyCfg.entryNode }
        else { conditionCfg.entryNode },
        fringe = conditionCfg.fringe.withEdgeType(FalseEdge) ++ bodyCfg.breaks.map((_, AlwaysEdge))
      )
  }

  /** CFG creation for while statements of the form `while(condition) body1 else body2` where body1 and the else block
    * are optional.
    */
  protected def cfgForWhileStatement(node: ControlStructure): CfgContext = {
    val conditionCfg = Traversal.fromSingle(node).condition.headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val trueCfg      = Traversal.fromSingle(node).whenTrue.headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val falseCfg     = Traversal.fromSingle(node).whenFalse.headOption.map(cfgFor).getOrElse(CfgContext.empty)

    val diffGraphs = edgesFromFringeTo(conditionCfg, trueCfg.entryNode) ++
      edgesFromFringeTo(trueCfg, falseCfg.entryNode) ++
      edgesFromFringeTo(trueCfg, conditionCfg.entryNode) ++
      edges(trueCfg.continues, conditionCfg.entryNode)

    edgeBuffer.appendAll(diffGraphs)
    CfgContext
      .from(conditionCfg, trueCfg, falseCfg)
      .copy(
        entryNode = conditionCfg.entryNode,
        fringe = conditionCfg.fringe.withEdgeType(FalseEdge) ++ trueCfg.breaks.map((_, AlwaysEdge)) ++ falseCfg.fringe
      )
  }

  /** CFG creation for switch statements of the form `switch { case condition: ... }`.
    */
  protected def cfgForSwitchStatement(node: ControlStructure): CfgContext = {
    val conditionCfg = Traversal.fromSingle(node).condition.headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val bodyCfg      = Traversal.fromSingle(node).whenTrue.headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val diffGraphs   = edgesToMultiple(conditionCfg.fringe.map(_._1), bodyCfg.caseLabels, CaseEdge)

    val hasDefaultCase = bodyCfg.caseLabels.exists(x => x.asInstanceOf[JumpTarget].name == "default")
    edgeBuffer.appendAll(diffGraphs)
    CfgContext
      .from(conditionCfg, bodyCfg)
      .copy(
        entryNode = conditionCfg.entryNode,
        fringe = {
          if (!hasDefaultCase) { conditionCfg.fringe.withEdgeType(FalseEdge) }
          else { List() }
        } ++ bodyCfg.breaks
          .map((_, AlwaysEdge)) ++ bodyCfg.fringe
      )
  }

  /** CFG creation for if statements of the form `if(condition) body`, optionally followed by `else body2`.
    */
  protected def cfgForIfStatement(node: ControlStructure): CfgContext = {
    val conditionCfg = Traversal.fromSingle(node).condition.headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val trueCfg      = Traversal.fromSingle(node).whenTrue.headOption.map(cfgFor).getOrElse(CfgContext.empty)
    val falseCfg     = Traversal.fromSingle(node).whenFalse.headOption.map(cfgFor).getOrElse(CfgContext.empty)

    val diffGraphs = edgesFromFringeTo(conditionCfg, trueCfg.entryNode) ++
      edgesFromFringeTo(conditionCfg, falseCfg.entryNode)

    edgeBuffer.appendAll(diffGraphs)
    CfgContext
      .from(conditionCfg, trueCfg, falseCfg)
      .copy(
        entryNode = conditionCfg.entryNode,
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
  protected def cfgForTryStatement(node: ControlStructure): CfgContext = {
    val maybeTryBlock =
      Traversal
        .fromSingle(node)
        .astChildren
        .where(_.order(1))
        .where(_.astChildren) // Filter out empty `try` bodies
        .headOption

    val tryBodyCfg: CfgContext =
      maybeTryBlock
        .map(cfgFor)
        .getOrElse(CfgContext.empty)

    val catchBodyCfgs: List[CfgContext] =
      Traversal
        .fromSingle(node)
        .astChildren
        .where(_.order(2))
        .toList match {
        case Nil  => List(CfgContext.empty)
        case asts => asts.map(cfgFor)
      }

    val maybeFinallyBodyCfg: List[CfgContext] =
      Traversal
        .fromSingle(node)
        .astChildren
        .where(_.order(3))
        .map(cfgFor)
        .headOption // Assume there can only be one
        .toList

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
      maybeFinallyBodyCfg.headOption.getOrElse(CfgContext.empty)
    } else {
      edgeBuffer.appendAll(diffGraphs)
      CfgContext
        .from(Seq(tryBodyCfg) ++ catchBodyCfgs ++ maybeFinallyBodyCfg: _*)
        .copy(
          entryNode = tryBodyCfg.entryNode,
          fringe = if (maybeFinallyBodyCfg.flatMap(_.entryNode).nonEmpty) {
            maybeFinallyBodyCfg.head.fringe
          } else {
            tryBodyCfg.fringe ++ catchBodyCfgs.flatMap(_.fringe)
          }
        )
    }
  }
}

object CfgCreator {

  implicit class FringeWrapper(fringe: List[(CfgNode, CfgEdgeType)]) {
    def withEdgeType(edgeType: CfgEdgeType): List[(CfgNode, CfgEdgeType)] = {
      fringe.map { case (x, _) => (x, edgeType) }
    }
  }

}


trait CfgEdgeType
object TrueEdge extends CfgEdgeType {
  override def toString: String = "TrueEdge"
}
object FalseEdge extends CfgEdgeType {
  override def toString: String = "FalseEdge"
}
object AlwaysEdge extends CfgEdgeType {
  override def toString: String = "AlwaysEdge"
}
object CaseEdge extends CfgEdgeType {
  override def toString: String = "CaseEdge"
}
case class CfgEdge(src: CfgNode, dst: CfgNode, edgeType: CfgEdgeType)
