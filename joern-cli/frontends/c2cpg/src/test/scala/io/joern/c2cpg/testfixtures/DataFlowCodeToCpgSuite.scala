package io.joern.c2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

class DataFlowCodeToCpgSuite extends CCodeToCpgSuite {

  var semanticsFilename: String = ProjectRoot.relativise("joern-cli/src/main/resources/default.semantics")

  var semantics: Semantics = _

  implicit var context: EngineContext = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    semantics = Semantics.fromList(new Parser().parseFile(semanticsFilename))
    context = EngineContext(semantics)
  }

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
  }

  protected implicit def int2IntegerOption(x: Int): Option[Integer] = Some(x)

  protected def flowToResultPairs(path: Path): List[(String, Option[Integer])] = {
    val pairs = path.elements.map {
      case point: MethodParameterIn =>
        val method      = point.method.head
        val method_name = method.name
        val code        = s"$method_name(${method.parameter.l.sortBy(_.order).map(_.code).mkString(", ")})"
        (code, point.lineNumber)
      case point => (point.statement.repr, point.lineNumber)
    }
    pairs.headOption.map(x => x :: pairs.sliding(2).collect { case Seq(a, b) if a != b => b }.toList).getOrElse(List())
  }

}
