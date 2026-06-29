package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewAnnotation,
  NewAnnotationParameter,
  NewAnnotationParameterAssign
}

private[x2cpg] trait AnnotationAstBuilder[Node, NodeProcessor] {
  this: AstCreatorBase[Node, NodeProcessor] =>

  /** Creates an AST that represents an annotation, including its content (annotation parameter assignments).
    */
  def annotationAst(annotation: NewAnnotation, children: Seq[Ast]): Ast = {
    val annotationAst = Ast(annotation)
    annotationAst.withChildren(children)
  }

  /** Creates an AST that represents an annotation assignment with a name for the assigned value, its overall code, and
    * the respective assignment AST.
    */
  def annotationAssignmentAst(assignmentValueName: String, code: String, assignmentAst: Ast): Ast = {
    val parameter      = NewAnnotationParameter().code(assignmentValueName)
    val assign         = NewAnnotationParameterAssign().code(code)
    val assignChildren = List(Ast(parameter), assignmentAst)
    setArgumentIndices(assignChildren)
    Ast(assign)
      .withChild(Ast(parameter))
      .withChild(assignmentAst)
  }
}
