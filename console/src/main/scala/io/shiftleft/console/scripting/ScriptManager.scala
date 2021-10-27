package io.shiftleft.console.scripting

import better.files._
import cats.effect.IO
import io.circe.generic.auto._
import io.circe.parser._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import org.zeroturnaround.zip.{NameMapper, ZipUtil}

import java.nio.file.{Files, NoSuchFileException}
import scala.util.Try

object ScriptManager {
  final case class ScriptCollections(collection: String, scripts: ScriptDescriptions)
  final case class ScriptDescriptions(description: String, scripts: List[ScriptDescription])
  final case class ScriptDescription(name: String, description: String)

  private val SCRIPT_DESCS: String = "scripts.json"
}

/**
  * This class manages a hierarchy of scripts, and provides an interface
  * that allows users to easily discover and run scripts on their CPGs.
  *
  * Scripts should be grouped inside folders placed within the application's
  * `resources/scripts` directory, for example:
  *
  *  resources
  *    |-- scripts
  *       |-- java
  *          |-- my-java-script.sc
  *       |-- go
  *       |-- csharp
  *
  * To run `my-java-script.sc` you would run:
  * `runScript("java/my-java-script.sc", cpg)`
  *
  * @param executor An executor that is used to run the managed scripts.
  */
abstract class ScriptManager(executor: AmmoniteExecutor) {

  import ScriptManager._

  implicit class CpgScriptRunner(cpg: Cpg) {

    /**
      * Run an arbitrary script over this CPG.
      *
      * @param name The name of the script to run.
      * @return The result of running the script against this CPG.
      */
    def runScript(name: String): Any =
      runScript(name, Map.empty)

    /**
      * Run an arbitrary script over this CPG with parameters.
      *
      * @param name       The name of the script to run.
      * @param parameters The parameters to pass to the script.
      * @return The result of running the script against this CPG.
      */
    def runScript(name: String, parameters: Map[String, String]): Any =
      ScriptManager.this.runScript(name, parameters, cpg)
  }

  private val absoluteJarPathRegex = """jar:file:(.*)!/scripts""".r
  private val scriptFileRegex = """(scripts/.*)""".r
  private val scriptDir = "scripts"

  // This is to work around Ammonite failing to read resource files on the classpath.
  // We simply copy the files into a temporary directory and read from there.
  private lazy val scriptsTempDir: File = {
    val newScriptsDir = File(Files.createTempDirectory("sl_scripts"))

    val scriptsPath = this.getClass.getClassLoader.getResource(scriptDir).toURI
    if (scriptsPath.getScheme.contains("jar")) {
      // get absolute jar path from classpath URI
      scriptsPath.toString match {
        case absoluteJarPathRegex(jarPath) =>
          ZipUtil.unpack(
            new java.io.File(jarPath),
            newScriptsDir.toJava,
            new NameMapper {
              override def map(name: String): String = name match {
                case scriptFileRegex(scriptFile) => scriptFile
                case _                           => null
              }
            }
          )
      }
    } else {
      File(scriptsPath).copyToDirectory(newScriptsDir)
    }

    newScriptsDir / scriptDir
  }

  def scripts(): List[ScriptCollections] = {
    scriptsTempDir
      .collectChildren(f => f.isDirectory && f != scriptsTempDir)
      .map { dir =>
        val relativeDir = scriptsTempDir.relativize(dir)

        val scriptDescs =
          Try((dir / SCRIPT_DESCS).lines.mkString(System.lineSeparator())).toEither
            .flatMap(v => decode[ScriptDescriptions](v))
            .toOption
            .getOrElse(ScriptDescriptions("", List.empty))

        ScriptCollections(relativeDir.toString, scriptDescs)
      }
      .toList
  }

  protected def withScriptFile[T](scriptName: String)(f: File => IO[T]): IO[T] = {
    val scriptPath = scriptsTempDir / scriptName
    if (scriptPath.exists) {
      f(scriptPath)
    } else {
      IO.raiseError(new NoSuchFileException(s"Script [$scriptPath] was not found."))
    }
  }

  def runScript(scriptName: String, parameters: Map[String, String], cpgFileName: String): Any = {
    runScript(scriptName, parameters, CpgLoader.load(cpgFileName))
  }

  def runScript(scriptName: String, parameters: Map[String, String], cpg: Cpg): Any = {
    withScriptFile(scriptName) { script =>
      executor.runScript(script.path, parameters, cpg)
    }.unsafeRunSync()
  }
}
