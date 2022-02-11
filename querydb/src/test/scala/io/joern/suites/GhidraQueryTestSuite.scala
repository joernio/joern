package io.joern.suites

import io.joern.console.QueryBundle
import io.joern.console.scan._
import io.joern.ghidra2cpg.fixtures.DataFlowBinToCpgSuite
import io.joern.util.QueryUtil
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.Query
import io.shiftleft.semanticcpg.language._
import io.shiftleft.utils.ProjectRoot
import overflowdb.traversal.iterableToTraversal

class GhidraQueryTestSuite extends DataFlowBinToCpgSuite {
  val argumentProvider              = new QDBArgumentProvider(3)
  override val binDirectory: String = ProjectRoot.relativise("querydb/src/test/resources/testbinaries")

  override def beforeAll(): Unit = {
    semanticsFilename = argumentProvider.testSemanticsFilename
    super.beforeAll()
  }

  protected def queryBundle: QueryBundle = QueryUtil.EmptyBundle

  protected def allQueries: List[Query] = QueryUtil.allQueries(queryBundle, argumentProvider)

  def findMatchingCalls(query: Query): Set[String] = {
    query(cpg)
      .flatMap(_.evidence)
      .collect { case call: nodes.Call => call }
      .method
      .name
      .toSetImmutable
  }

  def methodNamesForMatchedPoints(query: Query): Set[String] = {
    nodes.MethodParameterIn
    query(cpg)
      .flatMap(_.evidence)
      .collect { case cfgNode: nodes.CfgNode =>
        cfgNode.method.name
      }
      .toSetImmutable
  }
}
