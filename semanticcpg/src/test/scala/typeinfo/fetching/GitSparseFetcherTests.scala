package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Using

/** TODO: static test repo for testing */
class GitSparseFetcherTests extends AnyWordSpec with Matchers {
  "getVersions" should {
    "return all versions for a package" in {
      val expectedMetaDataBytes =
        """{
          |  VERSIONS: ["v1.0.0", "v1.11.9", "v1.11.9-rc2"],
          |  TYPE_NAME_INFO: [
          |    {
          |      VERSION: "v1.0.0",
          |      TYPE_NAMES: ["IonFloat"]
          |    },
          |    {
          |      VERSION: "v1.11.9",
          |      TYPE_NAMES: ["IonFloat"]
          |    },
          |    {
          |      VERSION: "v1.11.9-rc2",
          |      TYPE_NAMES: ["IonFloat"]
          |    }
          |  ]
          |}""".stripMargin.getBytes("UTF-8")
        
      Using.resource(GitSparseFetcher()) { fetcher =>
        val versionsBytes = fetcher.fetchMetaData(PackageIdentifier(LanguagePlatform.JVM, "ion-java"))
        versionsBytes shouldEqual expectedMetaDataBytes
      }
    }
  }
}
