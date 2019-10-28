package io.shiftleft.joern

import java.util.UUID

import cats.data.OptionT
import cats.effect.{ContextShift, IO}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.query.{CpgOperationResult, CpgQueryExecutor, DefaultCpgQueryExecutor}
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

}
