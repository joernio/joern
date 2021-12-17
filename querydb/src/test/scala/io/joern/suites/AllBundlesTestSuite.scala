package io.joern.suites

import io.joern.console.QueryDatabase
import io.shiftleft.semanticcpg.language._
import org.scalatest.wordspec.AnyWordSpec
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import org.scalatest.matchers.should.Matchers._

class AllBundlesTestSuite extends AnyWordSpec {
  val argumentProvider = new QDBArgumentProvider(3)

  "Complete QueryDatabase" should {
    "should contain queries with unique names" in {
      val qdb = new QueryDatabase(argumentProvider)
      val nonUniqueNames =
        qdb.allQueries
          .groupBy(_.name)
          .filter { q =>
            q._2.size > 1
          }
          .keys
      nonUniqueNames shouldBe empty
    }
  }
}
