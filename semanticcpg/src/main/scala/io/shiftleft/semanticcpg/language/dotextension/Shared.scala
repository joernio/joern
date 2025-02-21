package io.shiftleft.semanticcpg.language.dotextension

import better.files.File
import io.shiftleft.semanticcpg.utils.ExternalCommand

import scala.util.{Failure, Success, Try}

trait ImageViewer {
  def view(pathStr: String): Try[String]
}

object Shared {

  def plotAndDisplay(dotStrings: List[String], viewer: ImageViewer): Unit = {
    dotStrings.foreach { dotString =>
      File.usingTemporaryFile("semanticcpg") { dotFile =>
        File.usingTemporaryFile("semanticcpg") { svgFile =>
          dotFile.write(dotString)
          createSvgFile(dotFile, svgFile).toOption.foreach(_ => viewer.view(svgFile.path.toAbsolutePath.toString))
        }
      }
    }
  }

  private def createSvgFile(in: File, out: File): Try[String] = {
    Try {
      ExternalCommand
        .run(Seq("dot", "-Tsvg", in.path.toAbsolutePath.toString, "-o", out.path.toAbsolutePath.toString))
        .stdOut
        .mkString("\n")
    } match {
      case Success(v) => Success(v)
      case Failure(exc) =>
        System.err.println("Executing `dot` failed: is `graphviz` installed?")
        System.err.println(exc)
        Failure(exc)
    }
  }

}
