package io.joern.csharpsrc2cpg.passes

import better.files.File
import io.joern.semanticcpg.utils.SecureXmlParsing
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewDependency
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory

import scala.util.{Failure, Try}

class DependencyPass(cpg: Cpg, buildFiles: List[String]) extends ForkJoinParallelCpgPass[File](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[File] = buildFiles.map(x => File(x)).toArray

  override def runOnPart(builder: DiffGraphBuilder, part: File): Unit = {
    SecureXmlParsing.parseXml(part.contentAsString) match {
      case Some(xml) if xml.label == "Project" =>
        xml.child
          .collect { case x if x.label == "ItemGroup" => x.child }
          .flatten
          .collect {
            case packageReference if packageReference.label == "PackageReference" =>
              Try {
                val packageName    = packageReference.attribute("Include").map(_.toString()).get
                val packageVersion = packageReference.attribute("Version").map(_.toString()).getOrElse("")
                val dependencyNode = NewDependency()
                  .name(packageName)
                  .version(packageVersion)
                builder.addNode(dependencyNode)
              } match {
                case Failure(exception) =>
                  logger.error(s"Unable to parse $packageReference for package name and version information", exception)
                case _ =>
              }
          }
      case Some(_) =>
      case None    => logger.error(s"Failed to parse build file ${part.pathAsString}")
    }
  }

}
