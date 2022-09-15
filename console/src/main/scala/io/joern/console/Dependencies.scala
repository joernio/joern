package io.joern.console

import coursier.parse.DependencyParser
import java.io.File
import java.nio.file.Path
import scala.util.Try

object Dependencies {

  // TODO drop
  def main(args: Array[String]) = {
//    val coordinateString = "com.michaelpollmeier:versionsort:1.0.7"
    val coordinateString = "com.michaelpollmeier:gremlin-scala_2.13:3.5.1.5"
//    println(coursier.parse.ModuleParser.javaOrScalaModule(coordinateString))
//    println(coursier.parse.DependencyParser.dependency(coordinateString, defaultScalaVersion = "3"))
    println(resolve(coordinateString))
  }

  def resolve(coordinate: String): Either[String, Seq[File]] = {
    DependencyParser.dependency(coordinate, defaultScalaVersion = "3").map { dependency =>
      coursier.Fetch().addDependencies(dependency).run()
    }
  }

}
