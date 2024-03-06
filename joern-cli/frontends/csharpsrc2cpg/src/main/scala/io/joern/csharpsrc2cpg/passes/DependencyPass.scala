package io.joern.csharpsrc2cpg.passes

import better.files.File
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
import io.joern.semanticcpg.utils.SecureXmlParsing
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory

import scala.util.Try

class DependencyPass(cpg: Cpg, buildFiles: List[String]) extends ForkJoinParallelCpgPass[File](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[File] = buildFiles.map(x => File(x)).toArray

  override def runOnPart(builder: DiffGraphBuilder, part: File): Unit = {
    SecureXmlParsing.parseXml(part.contentAsString) match {
      case Some(xml) if xml.label == "Project" =>
        xml.child
          .filter(_.label == "ItemGroup")
          .flatMap(_.child)
          .filter(_.label == "PackageReference")
          .flatMap { packageReference =>
            Try {
              val packageName    = packageReference.attribute("Include").map(_.toString()).get
              val packageVersion = packageReference.attribute("Version").map(_.toString()).get
              NewDependency()
                .name(packageName)
                .version(packageVersion)
            }.toOption
          }
          .foreach(builder.addNode)
      case Some(_) =>
      case None    => logger.error(s"Failed to parse build file ${part.pathAsString}")
    }

//    } match {
//      case Failure(exception: Throwable) => logger.error(s"Failed to parse build file ${part.pathAsString}", exception)
//      case _                             =>
//    }
  }

}
