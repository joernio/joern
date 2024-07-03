package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.codedumper.CodeDumper
import overflowdb.Node
import overflowdb.traversal.*
import io.shiftleft.codepropertygraph.generated.help.Doc

/** Steps for all node types
  *
  * This is the base class for all steps defined on
  */
@help.Traversal(elementType = classOf[StoredNode])
class NodeSteps[NodeType <: StoredNode](val traversal: Iterator[NodeType]) extends AnyVal {

  @Doc(
    info = "The source file this code is in",
    longInfo = """
      |Not all but most node in the graph can be associated with
      |a specific source file they appear in. `file` provides
      |the file node that represents that source file.
      |"""
  )
  def file: Iterator[File] =
    traversal
      .choose(_.label) {
        case NodeTypes.NAMESPACE => _.in(EdgeTypes.REF).out(EdgeTypes.SOURCE_FILE)
        case NodeTypes.COMMENT   => _.in(EdgeTypes.AST).hasLabel(NodeTypes.FILE)
        case _ =>
          _.repeat(_.coalesce(_.out(EdgeTypes.SOURCE_FILE), _.in(EdgeTypes.AST)))(_.until(_.hasLabel(NodeTypes.FILE)))
      }
      .cast[File]

  @Doc(
    info = "Location, including filename and line number",
    longInfo = """
      |Most nodes of the graph can be associated with a specific
      |location in code, and `location` provides this location.
      |The return value is an object providing, e.g., filename,
      |line number, and method name, as opposed to being a flat
      |string. For example `.location.lineNumber` provides access
      |to the line number alone, without requiring any parsing
      |on the user's side.
      |"""
  )
  def location(implicit finder: NodeExtensionFinder): Iterator[NewLocation] =
    traversal.map(_.location)

  @Doc(
    info = "Display code (with syntax highlighting)",
    longInfo = """
      |For methods, dump the method code. For expressions,
      |dump the method code along with an arrow pointing
      |to the expression. Uses ansi-color highlighting.
      |This only works for source frontends.
      |"""
  )
  def dump(implicit finder: NodeExtensionFinder): List[String] =
    _dump(highlight = true)

  @Doc(
    info = "Display code (without syntax highlighting)",
    longInfo = """
      |For methods, dump the method code. For expressions,
      |dump the method code along with an arrow pointing
      |to the expression. No color highlighting.
      |"""
  )
  def dumpRaw(implicit finder: NodeExtensionFinder): List[String] =
    _dump(highlight = false)

  private def _dump(highlight: Boolean)(implicit finder: NodeExtensionFinder): List[String] = {
    // initialized on first element as we need the graph for retrieving the metaData node.
    // TODO: there should be a step to retrieve the metaData node for any node
    //  so we could avoid instantiating a new Cpg everytime using dump
    var cpg: Cpg = null
    traversal.map { node =>
      if (cpg == null) cpg = new Cpg(node.graph)
      val language = cpg.metaData.language.headOption
      val rootPath = cpg.metaData.root.headOption
      CodeDumper.dump(node.location, language, rootPath, highlight)
    }.l
  }

  /* follow the incoming edges of the given type as long as possible */
  protected def walkIn(edgeType: String): Iterator[Node] =
    traversal
      .repeat(_.in(edgeType))(_.until(_.in(edgeType).countTrav.filter(_ == 0)))

  @Doc(
    info = "Tag node with `tagName`",
    longInfo = """
      |This method can be used to tag nodes in the graph such that
      |they can later be looked up easily via `cpg.tag`. Tags are
      |key value pairs, and they can be created with `newTagNodePair`.
      |Since in many cases, a key alone is sufficient, we provide the
      |utility method `newTagNode(key)`, which is equivalent to
      |`newTagNode(key, "")`.
      |""",
    example = """.newTagNode("foo")"""
  )
  def newTagNode(tagName: String): NewTagNodePairTraversal = newTagNodePair(tagName, "")

  @Doc(info = "Tag node with (`tagName`, `tagValue`)", longInfo = "", example = """.newTagNodePair("key","val")""")
  def newTagNodePair(tagName: String, tagValue: String): NewTagNodePairTraversal = {
    new NewTagNodePairTraversal(traversal.map { node =>
      NewTagNodePair()
        .tag(NewTag().name(tagName).value(tagValue))
        .node(node)
    })
  }

  @Doc(info = "Tags attached to this node")
  def tagList: List[List[Tag]] =
    traversal.map { taggedNode =>
      taggedNode.tag.l
    }.l

  @Doc(info = "Tags attached to this node")
  def tag: Iterator[Tag] = {
    traversal.flatMap { node =>
      node.tag
    }
  }

}
