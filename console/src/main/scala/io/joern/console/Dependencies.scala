package io.joern.console

import coursier.parse.DependencyParser

import java.io.File
import java.nio.file.Path
import scala.util.Try

object Dependencies {

  def resolve(coordinate: String): Either[String, Seq[File]] =
    DependencyParser.dependency(coordinate, defaultScalaVersion = "3").map { dependency =>
      coursier.Fetch().addDependencies(dependency).run()
    }

  /** try to resolve all given coordinates. report failures to stderr, but continue anyway  */
  def resolveOptimistically(coordinates: Seq[String], verbose: Boolean = false): Seq[File] =
    coordinates.flatMap { coordinate =>
      resolve(coordinate) match {
        case Left(error) =>
          System.err.println(s"cannot resolve $coordinate: $error")
          Seq.empty //that's why this method is called `optimistic`: we don't fail the entire resolution
        case Right(files) =>
          println(s"resolved $coordinate - adding ${files.size} artifact(s) to classpath")
          if (verbose)
            files.foreach(println)
          files
      }
    }

}
