package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.KtFileWithMeta
import io.joern.kotlin2cpg.ast.AstCreator
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import org.jetbrains.kotlin.resolve.BindingContext
import org.slf4j.LoggerFactory

import scala.collection.immutable.SortedMap
import scala.collection.mutable

class AstCreationPass(
  filesWithMeta: Iterable[KtFileWithMeta],
  bindingContext: BindingContext,
  cpg: Cpg,
  disableFileContent: Boolean
)(implicit withSchemaValidation: ValidationMode)
    extends ForkJoinParallelCpgPassWithAccumulator[KtFileWithMeta, AstCreationPass.Accumulator](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  private var collectedTypes: Set[String]                                  = Set.empty
  private var collectedSamInfoEntries: Map[String, AstCreator.SamImplInfo] = Map.empty

  def usedTypes(): List[String] = collectedTypes.toList

  def samInfoEntries(): Iterable[AstCreator.SamImplInfo] = collectedSamInfoEntries.values

  override def createAccumulator(): AstCreationPass.Accumulator = {
    // We want the SAM types to have a deterministic iteration order. in onAccumulatorComplete
    // we sort by key by converting to a sorted map.

    // TypeNodePass sorts the types so using a regular sets for the types are fine
    AstCreationPass.Accumulator(mutable.HashSet.empty[String], mutable.HashMap.empty[String, AstCreator.SamImplInfo])
  }

  override def mergeAccumulator(left: AstCreationPass.Accumulator, right: AstCreationPass.Accumulator): Unit = {
    // Using a size-hint here might remove some pressure on allocations.
    // We trust the java people to have selected a proper default load factor.
    left.usedTypes.sizeHint(left.usedTypes.size + right.usedTypes.size)
    left.samInfoEntriesByImplClass.sizeHint(left.samInfoEntriesByImplClass.size + right.samInfoEntriesByImplClass.size)

    left.usedTypes ++= right.usedTypes
    right.samInfoEntriesByImplClass.foreach { case (samImplClass, samInfoEntry) =>
      left.samInfoEntriesByImplClass.getOrElseUpdate(samImplClass, samInfoEntry)
    }
  }

  override def onAccumulatorComplete(builder: DiffGraphBuilder, accumulator: AstCreationPass.Accumulator): Unit = {
    // A sorted Map would allow deterministic iteration order.
    collectedSamInfoEntries = SortedMap.from(accumulator.samInfoEntriesByImplClass)
    collectedTypes = (accumulator.usedTypes ++ collectedSamInfoEntries.keys).toSet
  }

  override def generateParts(): Array[KtFileWithMeta] = filesWithMeta.toArray

  override def runOnPart(
    diffGraph: DiffGraphBuilder,
    fileWithMeta: KtFileWithMeta,
    accumulator: AstCreationPass.Accumulator
  ): Unit = {
    val astCreator =
      new AstCreator(
        fileWithMeta,
        bindingContext,
        accumulator.usedTypes,
        accumulator.samInfoEntriesByImplClass,
        disableFileContent
      )
    diffGraph.absorb(astCreator.createAst())
    logger.debug(s"AST created for file at `${fileWithMeta.f.getVirtualFilePath}`.")
  }

}

object AstCreationPass {
  case class Accumulator(
    usedTypes: mutable.HashSet[String],
    samInfoEntriesByImplClass: mutable.HashMap[String, AstCreator.SamImplInfo]
  )
}
