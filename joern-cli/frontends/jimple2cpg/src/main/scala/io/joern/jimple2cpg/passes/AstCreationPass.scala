package io.joern.jimple2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory
import soot.{Scene, SootClass}

import java.io.{File => JFile}
import java.util.concurrent.{ConcurrentHashMap, Semaphore}
import scala.tools.nsc

case class Global(
    usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean](),
    sootLock: Semaphore = new Semaphore(1)
)

class AstCreationPass(codePath: String, filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(filenames.size))) {

  val global: Global = Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  /** The base directory of the source code.
    */
  private val codeDir: String = if (new JFile(codePath).isDirectory) {
    codePath
  } else {
    new JFile(codePath).getParentFile.getAbsolutePath
  }

  override def partIterator: Iterator[String] = filenames.iterator

  override def runOnPart(filename: String): Iterator[DiffGraph] = {
    val qualifiedClassName = getQualifiedClassPath(filename)
    var sootClass: Option[SootClass] = None
    try {
      try {
        global.sootLock.acquire()
        sootClass = Option(Scene.v().loadClassAndSupport(qualifiedClassName))
      } finally {
        global.sootLock.release()
      }
      sootClass match {
        case Some(clazz) => new AstCreator(filename, global).createAst(clazz)
        case None        => null
      }
    } catch {
      case e: Exception =>
        logger.warn(s"Cannot parse: $filename ($qualifiedClassName)", e)
        Iterator()
    } finally {
      sootClass match {
        case Some(clazz) => Scene.v().removeClass(clazz)
        case None        =>
      }
    }
  }

  /** Formats the file name the way Soot refers to classes within a class path. e.g.
    * /unrelated/paths/class/path/Foo.class => class.path.Foo
    *
    * @param filename the file name to transform.
    * @return the correctly formatted class path.
    */
  def getQualifiedClassPath(filename: String): String = {
    filename
      .replace(codeDir + nsc.io.File.separator, "")
      .replace(".class", "")
      .replace(nsc.io.File.separator, ".")
  }

}
