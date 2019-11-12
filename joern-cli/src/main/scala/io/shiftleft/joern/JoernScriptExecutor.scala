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

  private lazy val underlying = new DefaultCpgQueryExecutor(new ScriptEngineManager()) {
    override protected def buildQuery(query: String): String = {
      s"""
        |import gremlin.scala._
        |import io.shiftleft.console._
        |import io.shiftleft.joern.console._
        |import io.shiftleft.joern.console.Console._
        |import io.shiftleft.codepropertygraph.Cpg
        |import io.shiftleft.codepropertygraph.cpgloading._
        |import io.shiftleft.codepropertygraph.generated._
        |import io.shiftleft.codepropertygraph.generated.nodes._
        |import io.shiftleft.dataflowengine.language._
        |import io.shiftleft.semanticcpg.language._
        |import scala.collection.JavaConverters._
        |implicit val resolver: ICallResolver = NoResolve
        |val cpg = aCpg.asInstanceOf[io.shiftleft.codepropertygraph.Cpg]
        |
        |$query
        |""".stripMargin
    }
  }

  override def executeQuery(cpg: Cpg, query: String): IO[UUID] =
    ??? // unused within Joern

  override def executeQuerySync(cpg: Cpg, query: String): IO[CpgOperationResult[AnyRef]] =
    underlying.executeQuerySync(cpg, query)

  override def retrieveQueryResult(queryId: UUID): OptionT[IO, CpgOperationResult[AnyRef]] =
    ??? // unused within Joern

}
