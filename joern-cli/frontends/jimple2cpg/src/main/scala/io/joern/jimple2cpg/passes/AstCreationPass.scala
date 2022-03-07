package io.joern.jimple2cpg.passes

import io.joern.jimple2cpg.Jimple2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, IntervalKeyPool}
import org.slf4j.LoggerFactory
import soot.Scene

import java.util.concurrent.ConcurrentSkipListSet

case class Global(usedTypes: ConcurrentSkipListSet[String] = new ConcurrentSkipListSet[String]())

/** Creates the AST layer from the given class file and stores all types in the given global parameter.
  */
class AstCreationPass(filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ConcurrentWriterCpgPass[String](cpg, keyPool = Some(keyPool)) {

  val global: Global = Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def generateParts(): Array[_ <: AnyRef] = filenames.toArray

  override def runOnPart(builder: DiffGraphBuilder, part: String): Unit = {
    val qualifiedClassName = Jimple2Cpg.getQualifiedClassPath(part)
    try {
      val sootClass = Scene.v().loadClassAndSupport(qualifiedClassName)
      new AstCreator(part, builder, global).createAst(sootClass)
    } catch {
      case e: Exception =>
        logger.warn(s"Cannot parse: $part ($qualifiedClassName)", e)
        Iterator()
    }
  }

}
