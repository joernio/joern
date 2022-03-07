package io.joern.x2cpg.layers

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}
import io.joern.x2cpg.passes.typerelations.{AliasLinkerPass, TypeHierarchyPass}

object TypeRelations {
  val overlayName: String = "typerel"
  val description: String = "Type relations layer (hierarchy and aliases)"
  def defaultOpts         = new LayerCreatorOptions()

  def passes(cpg: Cpg): Iterator[CpgPassBase] = Iterator(new TypeHierarchyPass(cpg), new AliasLinkerPass(cpg))
}

class TypeRelations extends LayerCreator {
  override val overlayName: String = TypeRelations.overlayName
  override val description: String = TypeRelations.description
  override val dependsOn           = List(Base.overlayName)

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val cpg = context.cpg
    TypeRelations.passes(cpg).zipWithIndex.foreach { case (pass, index) =>
      runPass(pass, context, storeUndoInfo, index)
    }
  }

  // Layers need one-arg constructor, because they're called by reflection from io.joern.console.Run
  def this(optionsUnused: LayerCreatorOptions) = this()
}
