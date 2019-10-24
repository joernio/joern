package io.shiftleft.joern.server.cpg

import scala.concurrent.ExecutionContext
import cats.effect.{ContextShift, IO}
import io.shiftleft.console.query.{CpgOperationSuccess, DefaultCpgQueryExecutor}
import javax.script.ScriptEngineManager
import org.scalatest.concurrent.Eventually
import org.scalatest.{Inside, Matchers, WordSpec}

import scala.concurrent.duration._
import scala.language.postfixOps

class JoernCpgProviderSpec extends WordSpec with Matchers with Eventually with Inside {

  private implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  private def withJoernProvider[T](f: JoernCpgProvider => T): T = {
    f(new JoernCpgProvider)
  }

  private val filePath = getClass.getClassLoader.getResource("testcode/free").getPath

  "createCpg" should {

    "successfully create a CPG from a set of input files" in withJoernProvider { provider =>
      val uuid = provider.createCpg(Set(filePath)).unsafeRunSync()

      eventually(timeout(5 seconds), interval(500 millis)) {
        provider.retrieveCpg(uuid).value.unsafeRunSync() should matchPattern {
          case Some(CpgOperationSuccess(_)) =>
        }
      }
    }

    "produce a queryable CPG from the set of input files" in withJoernProvider { provider =>
      val queryExecutor = new DefaultCpgQueryExecutor(new ScriptEngineManager())

      val cpgId = provider.createCpg(Set(filePath)).unsafeRunSync()

      val cpg = eventually(timeout(5 seconds), interval(500 millis)) {
        inside(provider.retrieveCpg(cpgId).value.unsafeRunSync()) {
          case Some(CpgOperationSuccess(cpg)) => cpg
        }
      }

      val queryId = queryExecutor.executeQuery(cpg, "cpg.method.toJsonPretty").unsafeRunSync()

      val queryResult = eventually(timeout(5 seconds), interval(500 millis)) {
        inside(queryExecutor.retrieveQueryResult(queryId).value.unsafeRunSync()) {
          case Some(CpgOperationSuccess(queryResult)) => queryResult
        }
      }
      queryResult should not be empty
    }

  }

}
