package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Config
import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.jimple2cpg.util.ProgramHandlingUtil.ClassFile
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPassWithAccumulator
import org.slf4j.LoggerFactory
import io.shiftleft.utils.IOUtils
import soot.Scene

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Try

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  * @param classFiles
  *   List of class files and their fully qualified class names
  * @param cpg
  *   The CPG to add to
  */
class AstCreationPass(classFiles: List[ClassFile], cpg: Cpg, config: Config)
    extends ForkJoinParallelCpgPassWithAccumulator[ClassFile, AstCreationPass.Accumulator](cpg) {

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

  override def generateParts(): Array[? <: AnyRef] = classFiles.toArray

  override def runOnPart(builder: DiffGraphBuilder, classFile: ClassFile, accumulator: AstCreationPass.Accumulator): Unit = {
    try {
      val sootClass = Scene.v().loadClassAndSupport(classFile.fullyQualifiedClassName.get)
      sootClass.setApplicationClass()

      val file = Paths.get(classFile.file.toString.replace(".class", ".java"))

      val fileContent = Option
        .when(!config.disableFileContent && Files.exists(file)) {
          Try(IOUtils.readEntireFile(file))
            .orElse(Try(file.fileContent()))
            .orElse(Try(file.fileContent(StandardCharsets.ISO_8859_1)))
            .toOption
        }
        .flatten

      val localDiff =
        AstCreator(classFile.file.absolutePathAsString, sootClass, accumulator, fileContent = fileContent)(
          config.schemaValidation
        )
          .createAst()
      builder.absorb(localDiff)
    } catch {
      case e: Exception =>
        logger.warn(s"Exception on AST creation for ${classFile.file.absolutePathAsString}", e)
        Iterator()
    }
  }

}

object AstCreationPass {
  case class Accumulator(usedTypes: mutable.HashSet[String] = mutable.HashSet.empty) {
    def registerType(typeName: String): Unit = usedTypes.add(typeName)
  }
}
