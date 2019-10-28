package io.shiftleft.joern

import cats.effect.IO
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.ScriptManager
import io.shiftleft.console.query.{CpgOperationFailure, CpgOperationResult, CpgOperationSuccess}

class JoernScriptManager(executor: JoernScriptExecutor = new JoernScriptExecutor()) extends ScriptManager(executor) {

  private def scriptContent(name: String): String =
    (DEFAULT_SCRIPTS_FOLDER / name / s"$name.scala").lines.mkString(System.lineSeparator())

  private def handleQueryResult(result: IO[CpgOperationResult[AnyRef]]): AnyRef = {
    val scriptExecutionResult = for {
      queryResult <- result
      result <- queryResult match {
        case CpgOperationSuccess(result) => IO(result)
        case CpgOperationFailure(ex)     => IO.raiseError[AnyRef](ex)
      }
    } yield result
    scriptExecutionResult.handleErrorWith(IO(_)).unsafeRunSync()
  }

  def runScript(name: String, cpgFilename: String): AnyRef =
    handleQueryResult(executor.executeQuerySync(CpgLoader.load(cpgFilename), scriptContent(name)))

  def runScript(name: String, cpg: Cpg): AnyRef =
    handleQueryResult(executor.executeQuerySync(cpg, scriptContent(name)))

}
