package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Jimple2Cpg
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory
import soot.Scene

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  */
class AstCreationPass(filenames: List[String], cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[_ <: AnyRef] = filenames.toArray

  override def runOnPart(builder: DiffGraphBuilder, part: String): Unit = {
    val qualifiedClassName = Jimple2Cpg.getQualifiedClassPath(part)
    try {
      val sootClass = Scene.v().loadClassAndSupport(qualifiedClassName)
      sootClass.setApplicationClass()
      val localDiff = new AstCreator(part, sootClass, global).createAst()
      builder.absorb(localDiff)
    } catch {
      case e: Exception =>
        logger.warn(s"Cannot parse: $part ($qualifiedClassName)", e)
        Iterator()
    }
  }

}
