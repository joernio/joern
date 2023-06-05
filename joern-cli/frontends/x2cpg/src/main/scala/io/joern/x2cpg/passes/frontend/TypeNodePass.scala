package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.datastructures.{CodeTree, TreeNode}
import io.joern.x2cpg.passes.frontend.TypeNodePass.fullToShortName
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewNode, NewType, NewTypeDecl, NewTypeParameter}
import io.shiftleft.passes.{CpgPass, KeyPool}

import scala.collection.mutable

/** Creates a `TYPE` node for each type in `usedTypes`
  */
class TypeNodePass(
  usedTypes: List[String],
  cpg: Cpg,
  keyPool: Option[KeyPool] = None,
  nodesWithGenericTypes: Map[NewNode, CodeTree] = Map.empty
) extends CpgPass(cpg, "types", keyPool) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {

    diffGraph.addNode(
      NewType()
        .name("ANY")
        .fullName("ANY")
        .typeDeclFullName("ANY")
    )

    usedTypes.sorted.foreach { typeName =>
      val shortName = fullToShortName(typeName)
      val node = NewType()
        .name(shortName)
        .fullName(typeName)
        .typeDeclFullName(typeName)
      diffGraph.addNode(node)
    }

    generateGenericTypes(diffGraph, nodesWithGenericTypes)
  }

  private def generateGenericTypes(diffGraph: DiffGraphBuilder, nodesWithGenericTypes: Map[NewNode, CodeTree]): Unit = {

    def treeNodeToTypeParameter(x: TreeNode): NewTypeParameter = {
      val typeParameter = NewTypeParameter().name(x.value).code(x.value)
      diffGraph.addNode(typeParameter)
      generateTypeParametersFromChildren(x.children).foreach(tp => diffGraph.addEdge(typeParameter, tp, EdgeTypes.AST))
      typeParameter
    }

    def generateTypeParametersFromChildren(xs: List[TreeNode]): List[NewTypeParameter] = xs match {
      case head :: Nil  => List(treeNodeToTypeParameter(head))
      case head :: next => treeNodeToTypeParameter(head) +: generateTypeParametersFromChildren(next)
      case Nil          => List.empty
    }

    def generateTypeNodeFromTree(tree: CodeTree): NewType = {
      val fullName = tree.toString
      val shortType = tree.root.value match {
        case t if t.contains('.') && !t.endsWith(".") => t.substring(t.lastIndexOf('.') + 1)
        case t                                        => t
      }
      val typeNode = NewType().name(shortType).fullName(fullName).typeDeclFullName(fullName)
      val typeDecl = NewTypeDecl()
        .name(shortType)
        .fullName(fullName)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName("ANY")
      diffGraph.addNode(typeNode).addNode(typeDecl).addEdge(typeNode, typeDecl, EdgeTypes.REF)
      // TODO: How to do TYPE->TYPE_ARGUMENT or TYPE_DECL->TYPE_PARAMETER?
      //
      //      generateTypeParametersFromChildren(tree.root.children).foreach(ta =>
      //        diffGraph.addEdge(typeDecl, ta, EdgeTypes.AST)
      //      )
      typeNode
    }

    val typeToNode = mutable.HashMap.empty[String, NewType]

    nodesWithGenericTypes.foreach { case (node, tree) =>
      val associatedTypeNode = typeToNode.getOrElseUpdate(tree.toString, generateTypeNodeFromTree(tree))
      diffGraph.addEdge(node, associatedTypeNode, EdgeTypes.EVAL_TYPE)
    }
  }

}

object TypeNodePass {
  // Lambda typeDecl type names fit the structure
  // `a.b.c.d.ClassName.lambda$method$name:returnType(paramTypes)`
  // so this regex works by greedily matching the package and class names
  // at the start and cutting off the matched group before the signature.
  private val lambdaTypeRegex = raw".*\.(.*):.*\(.*\)".r

  def fullToShortName(typeName: String): String = {
    typeName match {
      case lambdaTypeRegex(methodName) => methodName
      case _                           => typeName.split('.').lastOption.getOrElse(typeName)
    }
  }
}
