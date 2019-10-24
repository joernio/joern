package io.shiftleft.joern

import java.util.UUID

import cats.data.OptionT
import cats.effect.{ContextShift, IO}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.query.{
  CpgOperationFailure,
  CpgOperationResult,
  CpgOperationSuccess,
  CpgQueryExecutor,
  DefaultCpgQueryExecutor
}
import javax.script.ScriptEngineManager

import scala.concurrent.ExecutionContext

class JoernScriptExecutor extends CpgQueryExecutor[AnyRef] {

  private implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  private val underlying = new DefaultCpgQueryExecutor(new ScriptEngineManager())

  override def executeQuery(cpg: Cpg, query: String): IO[UUID] =
    ??? // unused within Joern

  override def executeQuerySync(cpg: Cpg, query: String): IO[CpgOperationResult[AnyRef]] =
    underlying.executeQuerySync(cpg, query)

  override def retrieveQueryResult(queryId: UUID): OptionT[IO, CpgOperationResult[AnyRef]] =
    ??? // unused within Joern

  def runScript(content: String, cpgFilename: String): AnyRef = {
    val scriptExecutionResult = for {
      queryResult <- executeQuerySync(CpgLoader.load(cpgFilename), content)
      result <- queryResult match {
        case CpgOperationSuccess(result) => IO(result)
        case CpgOperationFailure(ex)     => IO.raiseError[AnyRef](ex)
      }
    } yield result
    scriptExecutionResult.handleErrorWith(IO(_)).unsafeRunSync()
  }
}
