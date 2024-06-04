package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Config
import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.jimple2cpg.util.ProgramHandlingUtil.ClassFile
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory
import better.files.{DefaultCharset, File}
import io.shiftleft.utils.IOUtils
import soot.Scene

import java.nio.charset.StandardCharsets
import scala.util.Try

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  * @param classFiles
  *   List of class files and their fully qualified class names
  * @param cpg
  *   The CPG to add to
  */
class AstCreationPass(classFiles: List[ClassFile], cpg: Cpg, config: Config)
    extends ConcurrentWriterCpgPass[ClassFile](cpg) {

  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[? <: AnyRef] = classFiles.toArray

  override def runOnPart(builder: DiffGraphBuilder, classFile: ClassFile): Unit = {
    try {
      val sootClass = Scene.v().loadClassAndSupport(classFile.fullyQualifiedClassName.get)
      sootClass.setApplicationClass()

      val file = File(classFile.file.pathAsString.replace(".class", ".java"))

      val fileContent = Option
        .when(!config.disableFileContent && file.exists) {
          Try(IOUtils.readEntireFile(file.path))
            .orElse(Try(file.contentAsString(DefaultCharset)))
            .orElse(Try(file.contentAsString(StandardCharsets.ISO_8859_1)))
            .toOption
        }
        .flatten

      val localDiff =
        AstCreator(classFile.file.canonicalPath, sootClass, global, fileContent = fileContent)(config.schemaValidation)
          .createAst()
      builder.absorb(localDiff)
    } catch {
      case e: Exception =>
        logger.warn(s"Exception on AST creation for ${classFile.file.canonicalPath}", e)
        Iterator()
    }
  }

}
