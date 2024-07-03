package io.joern.suites

import io.joern.console.QueryDatabase
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.*

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
