package io.joern.x2cpg

import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.{
  ExpressionNew,
  NewBlock,
  NewCall,
  NewControlStructure,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewNamespaceBlock,
  NewReturn
}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import overflowdb.BatchedUpdate.DiffGraphBuilder

abstract class AstCreatorBase(filename: String) {
  val diffGraph: DiffGraphBuilder = new DiffGraphBuilder

  def createAst(): DiffGraphBuilder

  /** Create a global namespace block for the given `filename`
    */
  def globalNamespaceBlock(): NewNamespaceBlock = {
    val absPath  = absolutePath(filename)
    val name     = NamespaceTraversal.globalNamespaceName
    val fullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(absPath))
    NewNamespaceBlock()
      .name(name)
      .fullName(fullName)
      .filename(absPath)
      .order(1)
  }

  /** Creates an AST that represents an entire method, including its content.
    */
  def methodAst(
    method: NewMethod,
    parameters: Seq[NewMethodParameterIn],
    body: Ast,
    methodReturn: NewMethodReturn
  ): Ast =
    Ast(method)
      .withChildren(parameters.map(Ast(_)))
      .withChild(body)
      .withChild(Ast(methodReturn))

  /** Creates an AST that represents a method stub, containing information about the method, its parameters, and the
    * return type.
    */
  def methodStubAst(method: NewMethod, parameters: Seq[NewMethodParameterIn], methodReturn: NewMethodReturn): Ast =
    Ast(method)
      .withChildren(parameters.map(Ast(_)))
      .withChild(Ast(methodReturn))

  /** Create a method return node
    */
  def methodReturnNode(
    line: Option[Integer],
    column: Option[Integer],
    tfn: String,
    dtfn: Option[String] = None
  ): NewMethodReturn =
    NewMethodReturn()
      .typeFullName(tfn)
      .dynamicTypeHintFullName(dtfn)
      .code(tfn)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)

  /** For a given return node and arguments, create an AST that represents the return instruction. The main purpose of
    * this method is to automatically assign the correct argument indices.
    */
  def returnAst(returnNode: NewReturn, arguments: List[Ast] = List()): Ast = {
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
    children: List[Ast] = List(),
    placeConditionLast: Boolean = false
  ): Ast = {
    condition match {
      case Some(conditionAst) =>
        Ast(controlStructureNode)
          .withChildren(if (placeConditionLast) children ++ List(conditionAst) else conditionAst :: children)
          .withConditionEdges(controlStructureNode, List(conditionAst.root).flatten)
      case _ =>
        Ast(controlStructureNode)
          .withChildren(children)
    }
  }

  /** For a given block node and statement ASTs, create an AST that represents the block. The main purpose of this
    * method is to increase the readability of the code which creates block asts.
    */
  def blockAst(blockNode: NewBlock, statements: List[Ast] = List()): Ast = {
    Ast(blockNode)
      .withChildren(statements)
  }

  /** For a given call node, arguments, and optionally, a receiver, create an AST that represents the call site. The
    * main purpose of this method is to automatically assign the correct argument indices.
    */
  def callAst(callNode: NewCall, arguments: List[Ast] = List(), receiver: Option[Ast] = None): Ast = {

    val receiverRoot = receiver.flatMap(_.root).toList
    val rcv          = receiver.getOrElse(Ast())
    receiverRoot match {
      case List(x: ExpressionNew) =>
        x.argumentIndex = 0
      case _ =>
    }

    setArgumentIndices(arguments)
    Ast(callNode)
      .withChild(rcv)
      .withChildren(arguments)
      .withArgEdges(callNode, arguments.flatMap(_.root))
      .withReceiverEdges(callNode, receiverRoot)
  }

  private def setArgumentIndices(arguments: List[Ast]) = {
    withIndex(arguments) { case (a, i) =>
      a.root.collect { case x: ExpressionNew =>
        x.argumentIndex = i
      }
    }
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

  /** Absolute path for the given file name
    */
  def absolutePath(filename: String): String =
    better.files.File(filename).path.toAbsolutePath.normalize().toString

}
