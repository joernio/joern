package io.joern.suites

import io.joern.ghidra2cpg.fixtures.{DataFlowBinToCpgSuite, GhidraBinToCpgSuite}
import io.joern.util.QueryUtil
import io.joern.console.scan._
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.console.QueryBundle
import io.shiftleft.console.Query
import io.shiftleft.dataflowengineoss.language.toExtendedCfgNode
import io.shiftleft.utils.ProjectRoot
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal

class GhidraQueryTestSuite extends DataFlowBinToCpgSuite {
  val argumentProvider = new QDBArgumentProvider(3)
  override val binDirectory: String = ProjectRoot.relativise("querydb/src/test/resources/testbinaries")

  override def beforeAll(): Unit = {
    semanticsFilename = argumentProvider.testSemanticsFilename
    super.beforeAll()
  }

  def queryBundle: QueryBundle = QueryUtil.EmptyBundle

  def allQueries = QueryUtil.allQueries(queryBundle, argumentProvider)

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
    //def source =
    //  cpg.call.methodFullName("getenv").cfgNext.isCall.argument(2).head
    //def sink = cpg.method.fullName("strcpy").parameter.index(2).head
    //println("SOURCE "+ source.code)
    //println("SINK "+ sink.code)
    //println(sink.reachableBy(source).l)
    query(cpg)
      .flatMap(_.evidence)
      .collect {
        case cfgNode: nodes.CfgNode => cfgNode.method.name
      }
      .toSetImmutable
  }
}
