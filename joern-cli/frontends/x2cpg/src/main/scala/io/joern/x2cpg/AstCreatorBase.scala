package io.joern.x2cpg

import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewNamespaceBlock
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
  def methodReturnNode(line: Option[Integer], column: Option[Integer], order: Int, tpe: String): NewMethodReturn =
    NewMethodReturn()
      .order(order)
      .typeFullName(tpe)
      .code(tpe)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .lineNumber(line)
      .columnNumber(column)

  /** Absolute path for the given file name
    */
  def absolutePath(filename: String): String =
    better.files.File(filename).path.toAbsolutePath.normalize().toString

}
