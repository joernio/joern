package io.joern.x2cpg

import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.IntervalKeyPool
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import java.nio.file.Paths

abstract class AstCreatorBase[Node, NodeProcessor](filename: String)(implicit val withSchemaValidation: ValidationMode)
    extends AstNodeBuilder[Node, NodeProcessor]
    with internal.AnnotationAstBuilder[Node, NodeProcessor]
    with internal.CallAstBuilder[Node, NodeProcessor]
    with internal.MethodAstBuilder[Node, NodeProcessor]
    with internal.ControlStructureAstBuilder[Node, NodeProcessor] { this: NodeProcessor =>

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

  def wrapMultipleInBlock(asts: Seq[Ast], lineNumber: Option[Int]): Ast = {
    asts.toList match {
      case Nil        => blockAst(NewBlock().typeFullName(Defines.Any).lineNumber(lineNumber))
      case ast :: Nil => ast
      case astList    => blockAst(NewBlock().typeFullName(Defines.Any).lineNumber(lineNumber), astList)
    }
  }

  /** For a given block node and statement ASTs, create an AST that represents the block. The main purpose of this
    * method is to increase the readability of the code which creates block asts.
    */
  def blockAst(blockNode: NewBlock, statements: List[Ast] = List()): Ast = {
    Ast(blockNode).withChildren(statements)
  }

  def setArgumentIndices(arguments: Seq[Ast], start: Int = 1): Unit = {
    arguments.zipWithIndex.foreach { case (ast, i) =>
      ast.root match {
        case Some(x: ExpressionNew) => x.argumentIndex = start + i
        case _                      => // do nothing
      }
    }
  }

  def withIndex[T, X](nodes: Seq[T])(func: (T, Int) => X): Seq[X] =
    nodes.zipWithIndex.map { case (x, i) =>
      func(x, i + 1)
    }

  def withIndex[T, X](nodes: Array[T])(func: (T, Int) => X): Seq[X] =
    nodes.toIndexedSeq.zipWithIndex.map { case (x, i) =>
      func(x, i + 1)
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
    Paths.get(filename).toAbsolutePath.normalize().toString

  /** @return
    *   the next available name for a closure in this context
    */
  def nextClosureName(): String = s"${Defines.ClosurePrefix}${closureKeyPool.next}"

}
