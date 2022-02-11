package io.joern.console
import better.files.Dsl._
import better.files.File
import better.files.File.apply

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

/** Plugin management component
  * @param installDir
  *   the Joern/Ocular installation dir
  */
class PluginManager(val installDir: File) {

  def listPlugins(): List[String] = {
    val installedPluginNames = pluginDir.toList
      .flatMap { dir =>
        File(dir).list.toList.flatMap { f =>
          "^joernext-(.*?)-.*$".r.findAllIn(f.name).matchData.map { m =>
            m.group(1)
          }
        }
      }
      .distinct
      .sorted
    installedPluginNames
  }

  def add(filename: String): Unit = {
    if (pluginDir.isEmpty) {
      println("Plugin directory does not exist")
      return
    }
    val file = File(filename)
    if (!file.exists) {
      println(s"The file $filename does not exist")
    } else {
      addExisting(file)
    }
  }

  private def addExisting(file: File): Unit = {
    val pluginName = file.name.replace(".zip", "")
    val tmpDir     = extractToTemporaryDir(file)
    tmpDir.foreach(dir => addExistingUnzipped(dir, pluginName))
  }

  private def addExistingUnzipped(file: File, pluginName: String): Unit = {
    file.listRecursively.filter(_.name.endsWith(".jar")).foreach { jar =>
      pluginDir.foreach { pDir =>
        if (!(pDir / jar.name).exists) {
          val dstFileName = s"joernext-$pluginName-${jar.name}"
          val dstFile     = pDir / dstFileName
          cp(jar, dstFile)
        }
      }
    }
  }

  private def extractToTemporaryDir(file: File) = {
    Try { file.unzip() } match {
      case Success(dir) =>
        Some(dir)
      case Failure(exc) =>
        println("Error reading zip: " + exc.getMessage)
        None
    }
  }

  def rm(name: String): List[String] = {
    if (!listPlugins().contains(name)) {
      List()
    } else {
      val filesToRemove = pluginDir.toList.flatMap { dir =>
        dir.list.filter { f =>
          f.name.startsWith(s"joernext-$name")
        }
      }
      filesToRemove.foreach(f => f.delete())
      filesToRemove.map(_.pathAsString)
    }
  }

  def pluginDir: Option[Path] = {
    val pathToPluginDir = installDir.path.resolve("lib")
    if (pathToPluginDir.toFile.exists()) {
      Some(pathToPluginDir)
    } else {
      println(s"Plugin directory at $pathToPluginDir does not exist")
      None
    }
  }

}
