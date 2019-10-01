package io.shiftleft.joern.server.cpg

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO}
import javax.script.ScriptEngineManager
import org.scalatest.concurrent.Eventually
import org.scalatest.{Inside, Matchers, WordSpec}

import io.shiftleft.cpgserver.model.CpgOperationSuccess
import io.shiftleft.cpgserver.query.DefaultCpgQueryExecutor
import scala.concurrent.duration._
import scala.language.postfixOps

class JoernCpgProviderSpec extends WordSpec with Matchers with Eventually with Inside {

  private implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  private def withJoernProvider[T](f: JoernCpgProvider => T): T = {
    f(new JoernCpgProvider)
  }

  "createCpg" should {
    "successfully create a CPG from a set of input files" in withJoernProvider { provider =>
      val uuid = provider.createCpg(Set("joern-cli/src/test/resources/testcode/free")).unsafeRunSync()

      eventually(timeout(5 seconds), interval(1 second)) {
        provider.retrieveCpg(uuid).value.unsafeRunSync() should matchPattern {
          case Some(CpgOperationSuccess(_)) =>
        }
      }
    }

    // TODO: This test currently fails when run via SBT (see https://github.com/scala/bug/issues/10058)
    "produce a queryable CPG from the set of input files" ignore withJoernProvider { provider =>
      val queryExecutor = new DefaultCpgQueryExecutor(new ScriptEngineManager())

      val cpgId = provider.createCpg(Set("joern-cli/src/test/resources/testcode/free")).unsafeRunSync()

      val cpg = eventually(timeout(5 seconds), interval(1 second)) {
        inside(provider.retrieveCpg(cpgId).value.unsafeRunSync()) {
          case Some(CpgOperationSuccess(cpg)) => cpg
        }
      }

      val queryId = queryExecutor.executeQuery(cpg, "cpg.method.toJsonPretty").unsafeRunSync()

      val queryResult = eventually(timeout(5 seconds), interval(1 second)) {
        inside(queryExecutor.retrieveQueryResult(queryId).value.unsafeRunSync()) {
          case Some(CpgOperationSuccess(queryResult)) => queryResult
        }
      }
      queryResult should not be empty
    }
  }
}
