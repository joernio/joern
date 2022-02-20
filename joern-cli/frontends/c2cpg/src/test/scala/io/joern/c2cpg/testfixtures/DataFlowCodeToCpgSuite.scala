package io.joern.c2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.dotextension.ImageViewer
import io.shiftleft.semanticcpg.layers.{Base, CallGraph, ControlFlow, LayerCreatorContext, TypeRelations}
import io.shiftleft.utils.ProjectRoot

import scala.sys.process.Process
import scala.util.Try

class DataFlowCodeToCpgSuite extends CCodeToCpgSuite {

  var semanticsFilename: String =
    ProjectRoot.relativise("dataflowengineoss/src/test/resources/default.semantics")

  var semantics: Semantics = _

  val viewer: ImageViewer = (pathStr: String) =>
    Try {
      Process(Seq("xdg-open", pathStr)).!!
    }

  implicit var context: EngineContext = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    semantics = Semantics.fromList(new Parser().parseFile(semanticsFilename))
    context = EngineContext(semantics)
  }

  override def passes(cpg: Cpg): Unit = {
    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
    new TypeRelations().run(context)
    new ControlFlow().run(context)
    new CallGraph().run(context)

    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
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
