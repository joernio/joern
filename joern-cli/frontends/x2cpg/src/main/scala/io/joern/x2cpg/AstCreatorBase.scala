package io.joern.x2cpg

import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.IntervalKeyPool
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import java.nio.file.Paths

/** Abstract base for all language-specific CPG AST creators.
  *
  * Mixes in builder traits for annotations, calls, methods, and control structures. Each concrete frontend subclasses
  * this and implements [[createAst]], populating [[diffGraph]] with the CPG nodes and edges derived from `filename`.
  *
  * @tparam Node
  *   the parser's AST node type
  * @tparam NodeProcessor
  *   the concrete AST creator type (self-type requirement ensures mixin methods can call creator helpers)
  * @param filename
  *   path of the source file being processed; used for namespace blocks and absolute-path resolution
  */
abstract class AstCreatorBase[Node, NodeProcessor](filename: String)(implicit val withSchemaValidation: ValidationMode)
    extends AstNodeBuilder[Node, NodeProcessor]
    with internal.AnnotationAstBuilder[Node, NodeProcessor]
    with internal.CallAstBuilder[Node, NodeProcessor]
    with internal.MethodAstBuilder[Node, NodeProcessor]
    with internal.ControlStructureAstBuilder[Node, NodeProcessor] { this: NodeProcessor =>

  val diffGraph: DiffGraphBuilder = Cpg.newDiffGraphBuilder

  private val closureKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  /** Entry point: build the CPG diffgraph for `filename`.
    *
    * Implementations traverse the parsed AST, create CPG nodes and edges, and add them to [[diffGraph]].
    *
    * @return
    *   the populated [[DiffGraphBuilder]] ready to be applied to the CPG
    */
  def createAst(): DiffGraphBuilder

  /** Creates a global namespace block for the source file being processed.
    *
    * Uses [[io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName]] as the name and
    * derives the full name from `filename` via [[io.joern.x2cpg.passes.frontend.MetaDataPass]]. The block is placed at
    * order 1 by convention.
    *
    * @return
    *   a new [[NewNamespaceBlock]] representing the file's top-level scope
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

  /** Wraps a sequence of ASTs in a [[NewBlock]] when there is more than one element.
    *
    *   - Empty sequence → an empty block node.
    *   - Exactly one AST → returned as-is (no wrapper).
    *   - Two or more ASTs → all children wrapped in a new block.
    *
    * @param asts
    *   the ASTs to wrap
    * @param lineNumber
    *   source line used for the synthesised block node
    */
  def wrapMultipleInBlock(asts: Seq[Ast], lineNumber: Option[Int]): Ast = {
    asts.toList match {
      case Nil        => blockAst(NewBlock().typeFullName(Defines.Any).lineNumber(lineNumber))
      case ast :: Nil => ast
      case astList    => blockAst(NewBlock().typeFullName(Defines.Any).lineNumber(lineNumber), astList)
    }
  }

  /** Creates an AST rooted at `blockNode` with `statements` as its ordered children.
    *
    * @param blockNode
    *   the block node that becomes the AST root
    * @param statements
    *   child ASTs representing the statements inside the block
    */
  def blockAst(blockNode: NewBlock, statements: List[Ast] = List()): Ast = {
    Ast(blockNode).withChildren(statements)
  }

  /** Assigns monotonically increasing `argumentIndex` values to a sequence of argument ASTs.
    *
    * Only AST roots that implement [[ExpressionNew]] are updated; other roots are silently skipped. Indices start at
    * `start` (default 1) and increment by 1 per argument.
    *
    * @param arguments
    *   ordered list of argument ASTs whose roots will be updated
    * @param start
    *   first argument index to assign (default 1)
    */
  def setArgumentIndices(arguments: Seq[Ast], start: Int = 1): Unit = {
    arguments.zipWithIndex.foreach { case (ast, i) =>
      ast.root match {
        case Some(x: ExpressionNew) => x.argumentIndex = start + i
        case _                      => // do nothing
      }
    }
  }

  /** Maps over an indexed sequence, passing 1-based indices to the transform function.
    *
    * @param nodes
    *   elements to iterate
    * @param func
    *   receives each element and its 1-based position
    */
  def withIndex[T, X](nodes: Seq[T])(func: (T, Int) => X): Seq[X] =
    nodes.zipWithIndex.map { case (x, i) =>
      func(x, i + 1)
    }

  /** Maps over an indexed array, passing 1-based indices to the transform function.
    *
    * @param nodes
    *   elements to iterate
    * @param func
    *   receives each element and its 1-based position
    */
  def withIndex[T, X](nodes: Array[T])(func: (T, Int) => X): Seq[X] =
    nodes.toIndexedSeq.zipWithIndex.map { case (x, i) =>
      func(x, i + 1)
    }

  /** Optionally sets the `argumentIndex` on a CPG expression node.
    *
    * When `argIdxOpt` is [[Some]], the index is applied to `node` in-place and the same node is returned. When it is
    * [[None]], the node is returned unchanged.
    *
    * @param node
    *   the expression node to annotate
    * @param argIdxOpt
    *   the argument index to apply, if any
    */
  def withArgumentIndex[T <: ExpressionNew](node: T, argIdxOpt: Option[Int]): T = {
    argIdxOpt match {
      case Some(argIdx) =>
        node.argumentIndex = argIdx
        node
      case None => node
    }
  }

  /** Optionally sets the `argumentName` on a CPG expression node.
    *
    * The name is applied unconditionally (a [[None]] clears any previously set name). Returns the same node for
    * chaining.
    *
    * @param node
    *   the expression node to annotate
    * @param argNameOpt
    *   the named-argument label, or [[None]] to clear
    */
  def withArgumentName[T <: ExpressionNew](node: T, argNameOpt: Option[String]): T = {
    node.argumentName = argNameOpt
    node
  }

  /** Returns the absolute, normalised path for the given filename.
    *
    * @param filename
    *   a relative or absolute path to resolve
    * @return
    *   the canonical absolute path string
    */
  def absolutePath(filename: String): String =
    Paths.get(filename).toAbsolutePath.normalize().toString

  /** Returns the next unique closure name for this file, of the form `<closurePrefix><n>`.
    *
    * Names are allocated from a per-file monotonically increasing counter, so each anonymous function / lambda in the
    * same file receives a distinct, stable identifier.
    *
    * @return
    *   a fresh closure name, e.g. `<lambda>0`, `<lambda>1`, …
    */
  def nextClosureName(): String = s"${Defines.ClosurePrefix}${closureKeyPool.next}"

}
