package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Config
import io.joern.jimple2cpg.astcreation.AstCreator
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import org.slf4j.LoggerFactory
import soot.{Scene, SootClass, SourceLocator}

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  */
class SootAstCreationPass(cpg: Cpg, config: Config)
    extends ForkJoinParallelCpgPassWithAccumulator[SootClass, AstCreationPass.Accumulator](cpg) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  private var _usedTypes: Set[String] = Set.empty

  def usedTypes(): Set[String] = _usedTypes

  override def createAccumulator(): AstCreationPass.Accumulator = AstCreationPass.Accumulator()

  override def mergeAccumulator(left: AstCreationPass.Accumulator, right: AstCreationPass.Accumulator): Unit = {
    left.usedTypes ++= right.usedTypes
  }

  override def onAccumulatorComplete(builder: DiffGraphBuilder, accumulator: AstCreationPass.Accumulator): Unit = {
    _usedTypes = accumulator.usedTypes.toSet
  }

  override def generateParts(): Array[? <: AnyRef] = Scene.v().getApplicationClasses.toArray()

  override def runOnPart(builder: DiffGraphBuilder, part: SootClass, accumulator: AstCreationPass.Accumulator): Unit = {
    val jimpleFile = SourceLocator.v().getSourceForClass(part.getName)
    try {
      // sootClass.setApplicationClass()
      val localDiff = new AstCreator(jimpleFile, part, accumulator)(config.schemaValidation).createAst()
      builder.absorb(localDiff)
    } catch {
      case e: Exception =>
        logger.warn(s"Cannot parse: $part", e)
    }
  }

}
