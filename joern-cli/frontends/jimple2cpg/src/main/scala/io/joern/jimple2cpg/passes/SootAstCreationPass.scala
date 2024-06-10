package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Config
import io.joern.jimple2cpg.astcreation.AstCreator
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory
import soot.{Scene, SootClass, SourceLocator}

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  */
class SootAstCreationPass(cpg: Cpg, config: Config) extends ConcurrentWriterCpgPass[SootClass](cpg) {

  val global: Global = new Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[? <: AnyRef] = Scene.v().getApplicationClasses.toArray()

  override def runOnPart(builder: DiffGraphBuilder, part: SootClass): Unit = {
    val jimpleFile = SourceLocator.v().getSourceForClass(part.getName)
    try {
      // sootClass.setApplicationClass()
      val localDiff = new AstCreator(jimpleFile, part, global)(config.schemaValidation).createAst()
      builder.absorb(localDiff)
    } catch {
      case e: Exception =>
        logger.warn(s"Cannot parse: $part", e)
    }
  }

}
