package io.joern.console

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

object TestBundle extends QueryBundle {
  @q def foo(n: Int = 4): Query = Query(
    name = "a-name",
    author = "an-author",
    title = "a-title",
    description = s"a-description $n",
    score = 2.0,
    traversal = { cpg =>
      cpg.method
    }
  )
}

// invalid because one of the query creators, `foo`,
// does not provide a default argument for parameter `n`
object InvalidBundle extends QueryBundle {
  @q def foo(n: Int): Query = Query(
    name = "a-name",
    author = "an-author",
    title = "a-title",
    description = s"a-description $n",
    score = 2.0,
    traversal = { cpg =>
      cpg.method
    }
  )
}

class QueryDatabaseTests extends AnyWordSpec with should.Matchers {
  "QueryDatabase" should {
    "contain Metrics bundle" in {
      new QueryDatabase(namespace = "io.joern.console").allBundles.count { bundle =>
        bundle.getName.endsWith("TestBundle$")
      } shouldBe 1
    }

    "contain `foo` query" in {
      val qdb = new QueryDatabase(namespace = "io.joern.console")
      val testBundles = qdb.allBundles.filter { bundle =>
        bundle.getName.endsWith("TestBundle$")
      }
      testBundles.size shouldBe 1
      val testBundle = testBundles.head
      val queries    = qdb.queriesInBundle(testBundle)
      queries.count(_.title == "a-title") shouldBe 1
    }

    "serialize traversal to string" in {
      val query = Query(
        name = "a-name",
        author = "an-author",
        title = "a-title",
        description = "a-description",
        score = 2.0,
        traversal = { cpg =>
          cpg.method
        }
      )
      query.title shouldBe "a-title"
    }

    "throw exception when trying to fetch queries from invalid bundle" in {
      val qdb = new QueryDatabase(namespace = "io.joern.console")
      val testBundles = qdb.allBundles.filter { bundle =>
        bundle.getName.endsWith("InvalidBundle$")
      }
      testBundles.size shouldBe 1
      val testBundle = testBundles.head
      assertThrows[RuntimeException] { qdb.queriesInBundle(testBundle) }
    }
  }
}
