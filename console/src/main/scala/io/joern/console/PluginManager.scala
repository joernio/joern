package io.joern.console

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Path, Paths, Files}
import scala.util.{Failure, Success, Try}

/** Plugin management component
  *
  * Joern allows plugins to be installed. A plugin at the very least consists of a class that inherits from
  * `LayerCreator`, bundled in a jar file, packaged in a zip file. The zip file may furthermore contain any dependency
  * jars that the plugin requires and that are not included on the joern class path by default.
  *
  * @param installDir
  *   the Joern/Ocular installation dir
  */
class PluginManager(val installDir: Path) {

  /** Generate a sorted list of all installed plugins by examining the plugin directory.
    */
  def listPlugins(): List[String] = {
    val installedPluginNames = pluginDir.toList
      .flatMap { dir =>
        (dir).listFiles().toList.flatMap { f =>
          "^joernext-(.*?)-.*$".r.findAllIn(f.fileName).matchData.map { m =>
            m.group(1)
          }
        }
      }
      .distinct
      .sorted
    installedPluginNames
  }

  /** Install the plugin stored at `filename`. The plugin is expected to be a zip file containing Java archives (.jar
    * files).
    */
  def add(filename: String): Unit = {
    if (pluginDir.isEmpty) {
      println("Plugin directory does not exist")
      return
    }
    val file = Paths.get(filename)
    if (!Files.exists(file)) {
      println(s"The file $filename does not exist")
    } else {
      addExisting(file)
    }
  }

  private def addExisting(file: Path): Unit = {
    val pluginName = file.fileName.replace(".zip", "")
    val tmpDir     = extractToTemporaryDir(file)
    tmpDir.foreach(dir => addExistingUnzipped(dir, pluginName))
  }

  private def addExistingUnzipped(file: Path, pluginName: String): Unit = {
    file.walk().filterNot(_ == file).filter(_.fileName.endsWith(".jar")).foreach { jar =>
      pluginDir.foreach { pDir =>
        if (!Files.exists((pDir / jar.fileName))) {
          val dstFileName = s"joernext-$pluginName-${jar.fileName}"
          val dstFile     = pDir / dstFileName
          FileUtil.copyFiles(jar, dstFile)
        }
      }
    }
  }

  private def extractToTemporaryDir(file: Path) = {
    Try {
      val unzipFolder = Files.createTempDirectory(file.fileName.stripSuffix(".zip"))
      file.unzipTo(unzipFolder)
    } match {
      case Success(dir) =>
        Some(dir)
      case Failure(exc) =>
        println("Error reading zip: " + exc.getMessage)
        None
    }
  }

  /** Delete plugin with given `name` from the plugin directory.
    */
  def rm(name: String): List[String] = {
    if (!listPlugins().contains(name)) {
      List()
    } else {
      val filesToRemove = pluginDir.toList.flatMap { dir =>
        dir.listFiles().filter { f =>
          f.fileName.startsWith(s"joernext-$name")
        }
      }
      filesToRemove.foreach(f => FileUtil.delete(f))
      filesToRemove.map(_.toString)
    }
  }

  /** Return the path to the plugin directory or None if the plugin directory does not exist.
    */
  def pluginDir: Option[Path] = {
    val pathToPluginDir = installDir.resolve("lib")
    if (pathToPluginDir.toFile.exists()) {
      Some(pathToPluginDir)
    } else {
      println(s"Plugin directory at $pathToPluginDir does not exist")
      None
    }
  }

}
