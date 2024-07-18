package io.shiftleft.semanticcpg.language.types.expressions.generalizations

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

@Traversal(elementType = classOf[AstNode])
class AstNodeTraversal[A <: AstNode](val traversal: Iterator[A]) extends AnyVal {

  /** Nodes of the AST rooted in this node, including the node itself.
    */
  @Doc(info = "All nodes of the abstract syntax tree")
  def ast: Iterator[AstNode] =
    traversal.repeat(_._astOut)(_.emit).cast[AstNode]

  /** All nodes of the abstract syntax tree rooted in this node, which match `predicate`. Equivalent of `match` in the
    * original CPG paper.
    */
  def ast(predicate: AstNode => Boolean): Iterator[AstNode] =
    ast.filter(predicate)

  def containsCallTo(regex: String): Iterator[A] =
    traversal.filter(_.ast.isCall.name(regex).nonEmpty)

  @Doc(info = "Depth of the abstract syntax tree")
  def depth: Iterator[Int] =
    traversal.map(_.depth)

  def depth(p: AstNode => Boolean): Iterator[Int] =
    traversal.map(_.depth(p))

  def isCallTo(regex: String): Iterator[Call] =
    isCall.name(regex)

  /** Nodes of the AST rooted in this node, minus the node itself
    */
  def astMinusRoot: Iterator[AstNode] =
    traversal.repeat(_._astOut)(_.emitAllButFirst).cast[AstNode]

  /** Direct children of node in the AST. Siblings are ordered by their `order` fields
    */
  def astChildren: Iterator[AstNode] =
    traversal.flatMap(_.astChildren).sortBy(_.order).iterator

  /** Parent AST node
    */
  def astParent: Iterator[AstNode] =
    traversal._astIn.cast[AstNode]

  /** Siblings of this node in the AST, ordered by their `order` fields
    */
  def astSiblings: Iterator[AstNode] =
    traversal.flatMap(_.astSiblings)

  /** Traverses up the AST and returns the first block node.
    */
  def parentBlock: Iterator[Block] =
    traversal.repeat(_._astIn)(_.emit.until(_.hasLabel(Block.Label))).collectAll[Block]

  /** Nodes of the AST obtained by expanding AST edges backwards until the method root is reached
    */
  def inAst: Iterator[AstNode] =
    inAst(null)

  /** Nodes of the AST obtained by expanding AST edges backwards until the method root is reached, minus this node
    */
  def inAstMinusLeaf: Iterator[AstNode] =
    inAstMinusLeaf(null)

  /** Nodes of the AST obtained by expanding AST edges backwards until `root` or the method root is reached
    */
  def inAst(root: AstNode): Iterator[AstNode] = {
    traversal
      .repeat(_._astIn)(
        _.emit
          .until(_.or(_.hasLabel(Method.Label), _.filter(n => root != null && root == n)))
      )
      .cast[AstNode]
  }

  /** Nodes of the AST obtained by expanding AST edges backwards until `root` or the method root is reached, minus this
    * node
    */
  def inAstMinusLeaf(root: AstNode): Iterator[AstNode] = {
    traversal
      .repeat(_._astIn)(
        _.emitAllButFirst
          .until(_.or(_.hasLabel(Method.Label), _.filter(n => root != null && root == n)))
      )
      .cast[AstNode]
  }

  /** Traverse only to those AST nodes that are also control flow graph nodes
    */
  def isCfgNode: Iterator[CfgNode] =
    traversal.collectAll[CfgNode]

  def isAnnotation: Iterator[Annotation] =
    traversal.collectAll[Annotation]

  def isAnnotationLiteral: Iterator[AnnotationLiteral] =
    traversal.collectAll[AnnotationLiteral]

  def isArrayInitializer: Iterator[ArrayInitializer] =
    traversal.collectAll[ArrayInitializer]

  /** Traverse only to those AST nodes that are blocks
    */
  def isBlock: Iterator[Block] =
    traversal.collectAll[Block]

  /** Traverse only to those AST nodes that are control structures
    */
  def isControlStructure: Iterator[ControlStructure] =
    traversal.collectAll[ControlStructure]

  /** Traverse only to AST nodes that are expressions
    */
  def isExpression: Iterator[Expression] =
    traversal.collectAll[Expression]

  /** Traverse only to AST nodes that are calls
    */
  def isCall: Iterator[Call] =
    traversal.collectAll[Call]

  /** Cast to call if applicable and filter on call code `calleeRegex`
    */
  def isCall(calleeRegex: String): Iterator[Call] =
    isCall.where(_.code(calleeRegex))

  /** Traverse only to AST nodes that are literals
    */
  def isLiteral: Iterator[Literal] =
    traversal.collectAll[Literal]

  def isLocal: Iterator[Local] =
    traversal.collectAll[Local]

  /** Traverse only to AST nodes that are identifier
    */
  def isIdentifier: Iterator[Identifier] =
    traversal.collectAll[Identifier]

  /** Traverse only to AST nodes that are IMPORT nodes
    */
  def isImport: Iterator[Import] =
    traversal.collectAll[Import]

  /** Traverse only to FILE AST nodes
    */
  def isFile: Iterator[File] =
    traversal.collectAll[File]

  /** Traverse only to AST nodes that are field identifier
    */
  def isFieldIdentifier: Iterator[FieldIdentifier] =
    traversal.collectAll[FieldIdentifier]

  /** Traverse only to AST nodes that are return nodes
    */
  def isReturn: Iterator[Return] =
    traversal.collectAll[Return]

  /** Traverse only to AST nodes that are MEMBER
    */
  def isMember: Iterator[Member] =
    traversal.collectAll[Member]

  /** Traverse only to AST nodes that are method reference
    */
  def isMethodRef: Iterator[MethodRef] =
    traversal.collectAll[MethodRef]

  /** Traverse only to AST nodes that are type reference
    */
  def isTypeRef: Iterator[TypeRef] =
    traversal.collectAll[TypeRef]

  /** Traverse only to AST nodes that are METHOD
    */
  def isMethod: Iterator[Method] =
    traversal.collectAll[Method]

  /** Traverse only to AST nodes that are MODIFIER
    */
  def isModifier: Iterator[Modifier] =
    traversal.collectAll[Modifier]

  /** Traverse only to AST nodes that are NAMESPACE_BLOCK
    */
  def isNamespaceBlock: Iterator[NamespaceBlock] =
    traversal.collectAll[NamespaceBlock]

  /** Traverse only to AST nodes that are METHOD_PARAMETER_IN
    */
  def isParameter: Iterator[MethodParameterIn] =
    traversal.collectAll[MethodParameterIn]

  /** Traverse only to AST nodes that are TemplateDom nodes
    */
  def isTemplateDom: Iterator[TemplateDom] =
    traversal.collectAll[TemplateDom]

  /** Traverse only to AST nodes that are TYPE_DECL
    */
  def isTypeDecl: Iterator[TypeDecl] =
    traversal.collectAll[TypeDecl]

  def walkAstUntilReaching(labels: List[String]): Iterator[StoredNode] = {
    traversal
      .repeat(_._astOut)(_.emitAllButFirst.until(_.hasLabel(labels*)))
      .dedup
      .cast[StoredNode]
  }

}
