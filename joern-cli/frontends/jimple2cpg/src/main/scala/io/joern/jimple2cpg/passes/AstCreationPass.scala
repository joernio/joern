package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Config
import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.jimple2cpg.util.ProgramHandlingUtil.ClassFile
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory
import sootup.core.cache.provider.LRUCacheProvider
import sootup.java.core.JavaProject

import scala.jdk.OptionConverters.*

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  * @param classFiles
  *   List of class files and their fully qualified class names
  * @param cpg
  *   The CPG to add to
  */
class AstCreationPass(classFiles: List[ClassFile], project: JavaProject, cpg: Cpg, config: Config)
    extends ConcurrentWriterCpgPass[ClassFile](cpg) {

  val global: Global    = new Global()
  private val idFactory = project.getIdentifierFactory
  private val view      = project.createView(new LRUCacheProvider(50))
  private val logger    = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[_ <: AnyRef] = classFiles.toArray

  override def runOnPart(builder: DiffGraphBuilder, classFile: ClassFile): Unit = {
    try {
      val classType = idFactory.getClassType(classFile.fullyQualifiedClassName.get)
      view.getClass(classType).toScala.foreach { sootClass =>
        val localDiff = AstCreator(classFile.file.canonicalPath, sootClass, view, global)(config.schemaValidation).createAst()
        builder.absorb(localDiff)
      }

    } catch {
      case e: Exception =>
        logger.warn(s"Exception on AST creation for ${classFile.file.canonicalPath}", e)
        Iterator()
    }
  }

}
