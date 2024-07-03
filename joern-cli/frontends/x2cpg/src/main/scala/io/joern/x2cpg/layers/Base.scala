package io.joern.x2cpg.layers

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import io.joern.x2cpg.passes.base.*

object Base {
  val overlayName: String = "base"
  val description: String = "base layer (linked frontend CPG)"
  def defaultOpts         = new LayerCreatorOptions()

  def passes(cpg: Cpg): Iterator[CpgPassBase] = Iterator(
    new FileCreationPass(cpg),
    new NamespaceCreator(cpg),
    new TypeDeclStubCreator(cpg),
    new MethodStubCreator(cpg),
    new ParameterIndexCompatPass(cpg),
    new MethodDecoratorPass(cpg),
    new AstLinkerPass(cpg),
    new ContainsEdgePass(cpg),
    new TypeRefPass(cpg),
    new TypeEvalPass(cpg)
  )

}

class Base extends LayerCreator {
  override val overlayName: String = Base.overlayName
  override val description: String = Base.description

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    cpg.graph.indexManager.createNodePropertyIndex(PropertyNames.FULL_NAME)
    Base.passes(cpg).zipWithIndex.foreach { case (pass, index) =>
      runPass(pass, context, index)
    }
  }

  // LayerCreators need one-arg constructor, because they're called by reflection from io.joern.console.Run
  def this(optionsUnused: LayerCreatorOptions) = this()
}
