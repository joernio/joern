package io.joern.php2cpg.passes

import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.PhpParser
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

class AstCreationPass(inputPath: String, cpg: Cpg) extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val PhpSourceFileExtensions: Set[String] = Set(".php")

  def pprint(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit = {

    val indent     = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
      case _: Iterable[Any] => ""
      case obj: Product     => obj.productPrefix
      case _                => obj.toString
    }

    println(s"$indent$prettyName$ptype")

    obj match {
      case seq: Iterable[Any] =>
        seq.foreach(pprint(_, depth + 1))
      case obj: Product =>
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => pprint(subObj, depth + 1, Some(paramName)) }
      case _ =>
    }
  }

  override def generateParts(): Array[String] = SourceFiles.determine(inputPath, PhpSourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    PhpParser.parseFile(filename) match {
      case Some(parseResult) =>
        diffGraph.absorb(new AstCreator(filename, parseResult).createAst())

      case None =>
        logger.error(s"Could not parse file $filename. Results will be missing!")
    }
  }
}
