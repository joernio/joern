package io.shiftleft.joern.server.cpg

import javax.script.ScriptEngineManager
import org.scalatest.{Matchers, WordSpec}

import io.shiftleft.cpgserver.model.CpgOperationSuccess
import io.shiftleft.cpgserver.query.DefaultCpgQueryExecutor

class JoernCpgProviderSpec extends WordSpec with Matchers {

  private def withJoernProvider[T](f: JoernCpgProvider => T): T = {
    f(new JoernCpgProvider)
  }

  "createCpg" should {
    "successfully create a CPG from a set of input files" in withJoernProvider { provider =>
      val uuid = provider.createCpg(Set("joern-cli/src/test/resources/testcode/free")).unsafeRunSync()
      provider.retrieveCpg(uuid).value.unsafeRunSync() should matchPattern {
        case Some(CpgOperationSuccess(_)) =>
      }
    }

    "produce a queryable CPG from the set of input files" in withJoernProvider { provider =>
      val queryExecutor = new DefaultCpgQueryExecutor(new ScriptEngineManager())

      val cpgId = provider.createCpg(Set("joern-cli/src/test/resources/testcode/free")).unsafeRunSync()
      val Some(CpgOperationSuccess(cpg)) = provider.retrieveCpg(cpgId).value.unsafeRunSync()

      val queryId = queryExecutor.executeQuery(cpg, "cpg.method.toJsonPretty").unsafeRunSync()
      val Some(CpgOperationSuccess(queryResult)) = queryExecutor.retrieveQueryResult(queryId).value.unsafeRunSync()
      queryResult should not be empty
    }
  }
}
