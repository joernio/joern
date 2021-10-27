package io.shiftleft.console.scripting

import ammonite.Main
import ammonite.runtime.Storage
import ammonite.util.{Bind, Res}
import cats.effect.IO
import cats.instances.list._
import cats.syntax.traverse._
import io.shiftleft.codepropertygraph.Cpg

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
  * Provides an interface for the execution of scripts using the
  * Ammonite interpreter.
  *
  * All scripts are compiled in-memory and no caching is performed.
  */
trait AmmoniteExecutor {
  ScalaReflectWorkaround.workaroundScalaReflectBugByTriggeringReflection()

  protected def predef: String

  protected lazy val ammoniteMain: Main = ammonite.Main(predefCode = predef,
                                                        remoteLogging = false,
                                                        verboseOutput = false,
                                                        welcomeBanner = None,
                                                        storageBackend = Storage.InMemory())

  /**
    * Runs the given script, passing any defined parameters in addition to bringing the provided variable
    * bindings into scope.
    *
    * @param scriptPath A path pointing to the Ammonite script to be executed.
    * @param parameters A map of parameters to be passed to the script, useful if you have a @main method in the script.
    * @param bindings   A list of variable bindings made implicitly available to scripts.
    * @return The result of running the script.
    */
  def runScript(scriptPath: Path, parameters: Map[String, String], bindings: IndexedSeq[Bind[_]]): IO[Any] = {
    val args: Seq[String] = parameters.flatMap { case (key, value) => Seq(s"--$key", value) }.toSeq
    for {
      replInstance <- IO(ammoniteMain.instantiateRepl(bindings))
      repl <- IO.fromEither(replInstance.left.map { case (err, _) => new RuntimeException(err.msg) })
      ammoniteResult <- IO {
        repl.initializePredef()
        ammonite.main.Scripts.runScript(ammoniteMain.wd, os.Path(scriptPath), repl.interp, args)
      }
      result <- ammoniteResult match {
        case Res.Success(res)     => IO.pure(res)
        case Res.Exception(ex, _) => IO.raiseError(ex)
        case Res.Failure(msg)     => IO.raiseError(new RuntimeException(msg))
        case _                    => IO.unit
      }
    } yield result
  }

  /**
    * Runs the given script, passing any defined parameters in addition to bringing a cpg into scope.
    *
    * @param scriptPath A path pointing to the Ammonite script to be executed.
    * @param parameters A map of parameters to be passed to the script, useful if you have a @main method in the script.
    * @param cpg        A CPG that is made implicitly available in the script.
    * @return The result of running the script.
    */
  def runScript(scriptPath: Path, parameters: Map[String, String], cpg: Cpg): IO[Any] = {
    runScript(scriptPath, parameters, bindings = IndexedSeq("cpg" -> cpg))
  }

  /**
    * Runs multiple scripts in the order they are specified in `scriptPaths`.
    *
    * @param scriptPaths A list of paths pointing to Ammonite scripts to be executed.
    * @param parameters  A map from script path to a set of parameter key/values.
    *                    If no entry is found for a script, an empty set of params
    *                    will be passed to the interpreter.
    * @param bindings    A list of variable bindings made implicitly available to scripts.
    * @return A list containing the results of running each script, in order.
    */
  def runScripts(scriptPaths: List[Path],
                 parameters: Map[Path, Map[String, String]],
                 bindings: IndexedSeq[Bind[_]]): IO[List[Any]] = {
    scriptPaths.map { scriptPath =>
      val scriptParams = parameters.getOrElse(scriptPath, Map.empty)
      runScript(scriptPath, scriptParams, bindings)
    }.sequence
  }

  /**
    * Runs multiple scripts in the order they are specified in `scriptPaths`.
    *
    * @param scriptPaths A list of paths pointing to Ammonite scripts to be executed.
    * @param parameters  A map from script path to a set of parameter key/values.
    *                    If no entry is found for a script, an empty set of params
    *                    will be passed to the interpreter.
    * @param cpg         A CPG that is made implicitly available in the scripts.
    * @return A list containing the results of running each script, in order.
    */
  def runScripts(scriptPaths: List[Path], parameters: Map[Path, Map[String, String]], cpg: Cpg): IO[Any] = {
    runScripts(scriptPaths, parameters, bindings = IndexedSeq("cpg" -> cpg))
  }

  /**
    * Runs a query against the provided CPG.
    *
    * @param query The query to run against the CPG.
    * @param cpg   The CPG made implicitly available in the query
    * @return The result of running the query.
    */
  def runQuery(query: String, cpg: Cpg): IO[Any] = {
    val queryContent =
      s"""|@main def main() = {
          |$query
          |}
          |""".stripMargin

    for {
      tempFile <- IO(Files.createTempFile("sl_query", ".sc"))
      _ <- IO(Files.write(tempFile, queryContent.getBytes(StandardCharsets.UTF_8)))
      result <- runScript(tempFile, Map.empty, cpg)
    } yield result
  }
}
