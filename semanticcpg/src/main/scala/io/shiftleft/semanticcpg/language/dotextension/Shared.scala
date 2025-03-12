package io.shiftleft.semanticcpg.language.dotextension

import io.shiftleft.semanticcpg.utils.{ExternalCommand, FileUtil}
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}

trait ImageViewer {
  def view(pathStr: String): Try[String]
}

object Shared {

  def plotAndDisplay(dotStrings: List[String], viewer: ImageViewer): Unit = {
    dotStrings.foreach { dotString =>
      FileUtil.usingTemporaryFile("semanticcpg") { dotFile =>
        FileUtil.usingTemporaryFile("semanticcpg") { svgFile =>
          Files.writeString(dotFile, dotString)
          createSvgFile(dotFile, svgFile).toOption.foreach(_ => viewer.view(svgFile.absolutePathAsString))
        }
      }
    }
  }

  private def createSvgFile(in: Path, out: Path): Try[String] = {
    Try {
      ExternalCommand
        .run(Seq("dot", "-Tsvg", in.absolutePathAsString, "-o", out.absolutePathAsString))
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
