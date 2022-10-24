package io.joern.javasrc2cpg.util

import better.files.File

/** Given a `codeDirectory`, the SourceRootFinder attempts to find all the source roots (so that the subdirectories
  * match the package structure of the source files). The JavaParserTypeSolver is path-dependent, so without this the
  * user would need to ensure that they specify the correct source directory when running javasrc2cpg.
  *
  * This works by checking if any subdirectories of the given directory match common java directory structures. In order
  * of preference: * Maven's default structure, e.g. src/main/java/io/... and src/test/java/io/... * Top-level src and
  * test directories, e.g. src/io/... and test/io/... If neither of these match, then it defaults to using the given
  * user input as the source root.
  */
object SourceRootFinder {

  private val excludes: Set[String] = Set("test", ".mvn", ".git")

  private def statePreSrc(currentDir: File): List[File] = {
    currentDir.children.filter(_.isDirectory).toList.flatMap { child =>
      child.name match {
        case name if excludes.contains(name) => Nil
        case "src"                           => stateSrc(child)
        case "main"                          => statePostSrc(child)
        case "java"                          => child :: Nil
        case _                               => statePreSrc(child)
      }
    }
  }

  private def stateSrc(currentDir: File): List[File] = {
    val mainChildren = currentDir.children.filter(_.isDirectory).toList.flatMap { child =>
      child.name match {
        case name if excludes.contains(name) => Nil
        case "main"                          => statePostSrc(child)
        case _                               => Nil
      }
    }

    val nonMainChildren = currentDir.children.filter(_.isDirectory).toList.flatMap { child =>
      child.name match {
        case name if excludes.contains(name) => Nil
        case "main"                          => Nil
        case _                               => statePostSrc(child)
      }
    }

    val hasExcludedDir = currentDir.children.filter(_.isDirectory).toList.exists(file => excludes.contains(file.name))

    (mainChildren, nonMainChildren) match {
      case (Nil, Nil) =>
        // probably a src/test directory in a tests-only module
        if (hasExcludedDir) Nil else List(currentDir)
      case (mainC, Nil) =>
        // probably follows the common src/main/<packages> structure
        mainC
      case (Nil, _) =>
        // the non-main children are probably package roots
        List(currentDir)
      case (mainC, nonMC) =>
        // main a package root here?
        mainC ++ nonMC
    }
  }

  private def statePostSrc(currentDir: File): List[File] = {
    val javaChildren = currentDir.children.filter(_.isDirectory).toList.flatMap { child =>
      child.name match {
        case name if excludes.contains(name) => Nil
        case _                               => child :: Nil
      }
    }

    javaChildren match {
      case Nil => currentDir :: Nil
      case _   => javaChildren
    }
  }

  private def listBottomLevelSubdirectories(currentDir: File): List[File] = {
    val srcDirs = currentDir.name match {
      case name if excludes.contains(name) => Nil
      case "src"                           => stateSrc(currentDir)
      case "main"                          => statePostSrc(currentDir)
      case "java"                          => List(currentDir)
      case _                               => statePreSrc(currentDir)
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
