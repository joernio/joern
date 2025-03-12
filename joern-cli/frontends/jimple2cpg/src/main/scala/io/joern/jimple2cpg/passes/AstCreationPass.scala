package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Config
import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.jimple2cpg.util.ProgramHandlingUtil.ClassFile
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import io.shiftleft.utils.IOUtils
import soot.Scene

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.Try

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  * @param classFiles
  *   List of class files and their fully qualified class names
  * @param cpg
  *   The CPG to add to
  */
class AstCreationPass(classFiles: List[ClassFile], cpg: Cpg, config: Config)
    extends ForkJoinParallelCpgPass[ClassFile](cpg) {

  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[? <: AnyRef] = classFiles.toArray

  override def runOnPart(builder: DiffGraphBuilder, classFile: ClassFile): Unit = {
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
        AstCreator(classFile.file.absolutePathAsString, sootClass, global, fileContent = fileContent)(
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
