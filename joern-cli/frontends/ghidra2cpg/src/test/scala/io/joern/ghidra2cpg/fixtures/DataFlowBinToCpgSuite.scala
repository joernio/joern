package io.joern.ghidra2cpg.fixtures

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.dotextension.ImageViewer
import io.shiftleft.semanticcpg.layers.*

import scala.compiletime.uninitialized
import scala.util.Try

class DataFlowBinToCpgSuite extends GhidraBinToCpgSuite {

  implicit var context: EngineContext = uninitialized

  override def beforeAll(): Unit = {
    super.beforeAll()
    context = EngineContext()
  }

  implicit val viewer: ImageViewer = (pathStr: String) =>
    Try {
      ExternalCommand
        .run(Seq("xdg-open", pathStr))
        .stdOut
        .mkString("\n")
    }

  override def passes(cpg: Cpg): Unit = {
    applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
  }

  protected implicit def int2IntegerOption(x: Int): Option[Int] =
    Some(x)

  protected def getMemberOfType(cpg: Cpg, typeName: String, memberName: String): Iterator[Member] =
    cpg.typeDecl.nameExact(typeName).member.nameExact(memberName)

  protected def getMethodOfType(cpg: Cpg, typeName: String, methodName: String): Iterator[Method] =
    cpg.typeDecl.nameExact(typeName).method.nameExact(methodName)

  protected def getLiteralOfType(cpg: Cpg, typeName: String, literalName: String): Iterator[Literal] =
    cpg.typeDecl.nameExact(typeName).method.isLiteral.codeExact(literalName)
}
