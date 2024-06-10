package io.joern.console

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal.help.TraversalSource

package object scan {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  implicit class ScannerStarters(val cpg: Cpg) extends AnyVal {
    def finding: Iterator[Finding] =
      overflowdb.traversal.InitialTraversal.from[Finding](cpg.graph, NodeTypes.FINDING)
  }

  implicit class QueryWrapper(q: Query) {

    /** Obtain list of findings by running query on CPG
      */
    def apply(cpg: Cpg): List[NewFinding] = {
      try {
        q.traversal(cpg)
          .map(evidence =>
            QueryWrapper.finding(
              evidence = evidence,
              name = q.name,
              author = q.author,
              title = q.title,
              description = q.description,
              score = q.score
            )
          )
          .l
      } catch {
        case ex: Throwable =>
          logger.warn("Error executing query", ex)
          List()
      }

    }
  }

  object QueryWrapper {

    def finding(
      evidence: StoredNode,
      name: String,
      author: String,
      title: String,
      description: String,
      score: Double
    ): NewFinding = {
      NewFinding()
        .evidence(List(evidence))
        .keyValuePairs(
          List(
            NewKeyValuePair().key(FindingKeys.name).value(name),
            NewKeyValuePair().key(FindingKeys.author).value(author),
            NewKeyValuePair().key(FindingKeys.title).value(title),
            NewKeyValuePair().key(FindingKeys.description).value(description),
            NewKeyValuePair().key(FindingKeys.score).value(score.toString)
          )
        )
    }

  }

  private object FindingKeys {
    val name        = "name"
    val author      = "author"
    val title       = "title"
    val description = "description"
    val score       = "score"
  }

  implicit class ScannerFindingStep(val traversal: Iterator[Finding]) extends AnyRef {

    def name: Iterator[String] = traversal.map(_.name)

    def author: Iterator[String] = traversal.map(_.author)

    def title: Iterator[String] = traversal.map(_.title)

    def description: Iterator[String] = traversal.map(_.description)

    def score: Iterator[Double] = traversal.map(_.score)

  }

  implicit class ScannerFindingExtension(val node: Finding) extends AnyRef {

    def name: String = getValue(FindingKeys.name)

    def author: String = getValue(FindingKeys.author)

    def title: String = getValue(FindingKeys.title)

    def description: String = getValue(FindingKeys.description)

    def score: Double = getValue(FindingKeys.score).toDouble

    protected def getValue(key: String, default: String = ""): String =
      node.keyValuePairs.find(_.key == key).map(_.value).getOrElse(default)

  }

  @TraversalSource
  implicit class TaintAnalysisStarters(cpg: Cpg) {

    import TaintAnalysisKeys.*

    def sources: Iterator[CfgNode] =
      cpg.all.where(_.tag.nameExact(SOURCE)).collectAll[CfgNode]

    def sinks: Iterator[CfgNode] =
      cpg.all.where(_.tag.nameExact(SINK)).collectAll[CfgNode]

    def sanitizers: Iterator[CfgNode] =
      cpg.all.where(_.tag.nameExact(SANITIZER)).collectAll[CfgNode]

  }

  implicit class TaintAnalysisExt[T <: StoredNode](traversal: Iterator[T]) {

    import TaintAnalysisKeys.*

    def tagAsSource(implicit diffGraph: DiffGraphBuilder): Unit = traversal.newTagNode(SOURCE).store()

    def tagAsSink(implicit diffGraph: DiffGraphBuilder): Unit = traversal.newTagNode(SINK).store()

    def tagAsSanitizer(implicit diffGraph: DiffGraphBuilder): Unit = traversal.newTagNode(SANITIZER).store()

    def tagAsSource(value: String)(implicit diffGraph: DiffGraphBuilder): Unit =
      traversal.newTagNodePair(SOURCE, value).store()

    def tagAsSink(value: String)(implicit diffGraph: DiffGraphBuilder): Unit =
      traversal.newTagNodePair(SINK, value).store()

    def tagAsSanitizer(value: String)(implicit diffGraph: DiffGraphBuilder): Unit =
      traversal.newTagNodePair(SANITIZER, value).store()

    def isSanitizer: Iterator[T] = traversal.where(_.tag.nameExact(SANITIZER))

  }

  implicit class TaintAnalysisNodeExt[T <: StoredNode](node: T) {

    import TaintAnalysisKeys.*

    def tagAsSource(implicit diffGraph: DiffGraphBuilder): Unit = node.start.newTagNode(SOURCE).store()

    def tagAsSink(implicit diffGraph: DiffGraphBuilder): Unit = node.start.newTagNode(SINK).store()

    def tagAsSanitizer(implicit diffGraph: DiffGraphBuilder): Unit = node.start.newTagNode(SANITIZER).store()

    def tagAsSource(value: String)(implicit diffGraph: DiffGraphBuilder): Unit =
      node.start.newTagNodePair(SOURCE, value).store()

    def tagAsSink(value: String)(implicit diffGraph: DiffGraphBuilder): Unit =
      node.start.newTagNodePair(SINK, value).store()

    def tagAsSanitizer(value: String)(implicit diffGraph: DiffGraphBuilder): Unit =
      node.start.newTagNodePair(SANITIZER, value).store()

    def isSanitizer: Iterator[T] = node.start.where(_.tag.nameExact(SANITIZER))

  }

  object TaintAnalysisKeys {
    val SOURCE    = "source"
    val SINK      = "sink"
    val SANITIZER = "sanitizer"
  }

  def outputFindings(cpg: Cpg)(implicit finder: NodeExtensionFinder): Unit = {
    cpg.finding.sortBy(_.score.toInt).foreach { finding =>
      val evidence = finding.evidence.headOption
        .map { e =>
          s"${e.location.filename}:${e.location.lineNumber.getOrElse(0)}:${e.location.methodFullName}"
        }
        .getOrElse("")
      println(s"Result: ${finding.score} : ${finding.title}: $evidence")
    }
  }

}
