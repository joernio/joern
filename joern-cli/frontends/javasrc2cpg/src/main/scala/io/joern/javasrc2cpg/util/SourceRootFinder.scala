package io.joern.javasrc2cpg.util

import better.files.File

case class DirectoryStructureInfo(priority: Int, structure: List[String])

/** Given a `codeDirectory`, the HeuristicSourceFinder attempts to find all the source roots.
  *
  * You might wonder, isn't a basic FSM overkill for this? Well, it probably is, but does the job (maybe even moderately
  * efficiently), so it's hopefully good enough for now.
  */
object SourceRootFinder {

  private def statePreSrc(currentDir: File): List[File] = {
    currentDir.children.filter(_.isDirectory).toList.flatMap { child =>
      child.name match {
        case "src"  => stateSrc(child)
        case "main" => stateMainTest(child)
        case "test" => stateMainTest(child)
        case "java" => child :: Nil
        case _      => statePreSrc(child)
      }
    }
  }

  private def stateSrc(currentDir: File): List[File] = {
    val mainTestChildren = currentDir.children.filter(_.isDirectory).toList.flatMap { child =>
      child.name match {
        case "main" => stateMainTest(child)
        case "test" => stateMainTest(child)
        case _      => Nil
      }
    }

    mainTestChildren match {
      case Nil => List(currentDir)
      case _   => mainTestChildren
    }
  }

  private def stateMainTest(currentDir: File): List[File] = {
    val javaChildren = currentDir.children.filter(_.isDirectory).toList.flatMap { child =>
      child.name match {
        case "java" => child :: Nil
        case _      => Nil
      }
    }

    javaChildren match {
      case Nil => currentDir :: Nil
      case _   => javaChildren
    }
  }

  private def listBottomLevelSubdirectories(currentDir: File): List[File] = {
    val srcDirs = currentDir.name match {
      case "src"  => stateSrc(currentDir)
      case "main" => stateMainTest(currentDir)
      case "test" => stateMainTest(currentDir)
      case "java" => List(currentDir)
      case _      => statePreSrc(currentDir)
    }

    srcDirs match {
      case Nil => List(currentDir)
      case _   => srcDirs
    }
  }

  def getSourceRoots(codeDir: String): List[String] = {
    listBottomLevelSubdirectories(File(codeDir)).map(_.pathAsString)
  }
}
