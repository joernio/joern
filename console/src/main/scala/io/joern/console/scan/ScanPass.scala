package io.joern.console.scan

import io.joern.console.Query
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPass
import io.joern.console.scan.*
import io.joern.console.scan.TaintAnalysisKeys.{SANITIZER, SINK, SOURCE}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate

/** Each query runs the data-flow engine, which is already parallelized. Another layer of parallelism causes undefined
  * behaviour on the underlying database. This is why we use `CpgPass` instead of `ConcurrentWriterCpgPass` or similar.
  */
class ScanPass(cpg: Cpg, queries: List[Query], maxCallDepth: Int) extends CpgPass(cpg) {

  private val QueryTagTaint = "taint"

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val (taintQueries, otherQueries) = queries.partition(_.tags.contains(QueryTagTaint))
    otherQueries.flatMap(_.apply(cpg)).foreach(diffGraph.addNode)
    runTaintAnalysis(taintQueries, diffGraph)
  }

  private def runTaintAnalysis(taintQueries: List[Query], diffGraph: DiffGraphBuilder): Unit = {
    implicit val taggingDiffGraph: DiffGraphBuilder = new DiffGraphBuilder()
    taintQueries.foreach { q =>
      val queryTags = q.tags.toSet
      if (queryTags.contains(TaintAnalysisKeys.SOURCE)) {
        q.traversal(cpg).tagAsSource(q.name)
      } else if (queryTags.contains(TaintAnalysisKeys.SINK)) {
        q.traversal(cpg).tagAsSink(q.name)
      } else if (queryTags.contains(TaintAnalysisKeys.SANITIZER)) {
        q.traversal(cpg).tagAsSanitizer(q.name)
      }
    }
    BatchedUpdate.applyDiff(cpg.graph, taggingDiffGraph)

    val queryLookup = taintQueries.map(q => q.name -> q).toMap

    val semantics =
      cpg.metaData.language.headOption.map(DefaultSemantics.semanticsFor).getOrElse(DefaultSemantics.semanticsFor(""))
    val engineContext = EngineContext(semantics, EngineConfig(maxCallDepth))

    logger.debug(
      s"Joern Scan matched on " +
        s"${cpg.sources.size} sources, " +
        s"${cpg.sinks.size} sinks, " +
        s"${cpg.sanitizers.size} sanitizers from ${taintQueries.size} queries"
    )
    val sinks   = cpg.sinks.l
    val sources = cpg.sources.l
    if (sinks.nonEmpty && sources.nonEmpty) {
      sinks
        .reachableByFlows(sources)(engineContext)
        .passesNot(_.isSanitizer)
        .zipWithIndex
        .map { case (Path(elements), idx) =>
          val sourceQuery = elements
            .flatMap(_.tag.nameExact(SOURCE, SANITIZER, SINK).value.headOption)
            .headOption
            .map(queryLookup.apply)
          val sinkQuery = elements
            .flatMap(_.tag.nameExact(SOURCE, SANITIZER, SINK).value.headOption)
            .lastOption
            .map(queryLookup.apply)
          val evidence             = elements.last
          val (title, description) = buildTitleAndDescription(sourceQuery, sinkQuery)
          val author = Seq(sourceQuery, sinkQuery)
            .flatMap(_.map(_.author))
            .distinct
            .reduceOption((a, b) => s"$a & $b")
            .getOrElse("Joern Scan")
          val name  = s"$idx-tainted-${evidence.code}"
          val score = (sourceQuery.map(_.score).getOrElse(5.0) + sinkQuery.map(_.score).getOrElse(5.0)) / 2d
          QueryWrapper.finding(evidence, name, author, title, description, score)
        }
        .foreach(diffGraph.addNode)
    }
  }

  private def buildTitleAndDescription(sourceQ: Option[Query], sinkQ: Option[Query]): (String, String) = {
    (sourceQ, sinkQ) match {
      case (Some(src), Some(snk)) => s"${src.title} flows to ${snk.title}" -> s"${src.description}\n${snk.description}"
      case (None, Some(snk)) => s"Attacker-controlled data flows to ${snk.title}" -> snk.description
      case (Some(src), None) => s"${src.title} data flows to a sensitive sink"    -> src.description
      case (None, None) =>
        s"Attacker-controlled data flows to a sensitive sink" ->
          "Attacker-controlled data defining arguments to a sensitive sink may be a cause of security vulnerabilities."
    }
  }

}
