package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo.*
import io.shiftleft.semanticcpg.typeinfo.version.SemVer2
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Using

/** TODO: static test repo for testing */
class GitSparseFetcherTests extends AnyWordSpec with Matchers {
  val testDefaultTimeout = 30.seconds

  "fetchMetaData" should {
    "return metadata bytes for a package" in {
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
        val versionsBytes =
          Await.result(fetcher.fetchMetaData(PackageIdentifier(LanguagePlatform.JVM, "ion-java")), testDefaultTimeout)
        versionsBytes shouldEqual expectedMetaDataBytes
      }
    }
  }

  "fetchTypeData" should {
    "return a map of type info bytes for a versioned package" in {
      val expectedBytes =
        """{
          |  FULL_NAME:"com.amazon.ion.IonFloat",
          |  NAME:"IonFloat",
          |  TYPE_PARAMETERS:[
          |  ],
          |  INHERITS:[
          |    "java.lang.Cloneable"
          |  ],
          |  METHODS:[
          |    {
          |      NAME:"bigIntegerValue",
          |      FULL_NAME:"com.amazon.ion.IonFloat.bigIntegerValue:java.math.BigInteger()",
          |      SIGNATURE:"java.math.BigInteger()"
          |    }
          |  ],
          |  MEMBERS:[
          |    {
          |      NAME:"EMPTY_ARRAY",
          |      TYPE_FULL_NAME:"com.amazon.ion.IonValue"
          |    }
          |  ],
          |  DEPENDS:[
          |    {
          |      FULL_NAME: "java.lang",
          |      VERSION: "4.1.2"
          |    }
          |  ]
          |}""".stripMargin.getBytes("UTF-8")
      val pid       = PackageIdentifier(LanguagePlatform.JVM, "ion-java")
      val version   = SemVer2("1.0.0")
      val typeNames = List("IonFloat")
      Using.resource(GitSparseFetcher()) { fetcher =>
        val typeMap = Await.result(fetcher.fetchTypeData(pid, version, typeNames), testDefaultTimeout)
        typeMap.size shouldEqual 1
        val ionFloatBytes = typeMap("IonFloat")
        ionFloatBytes shouldEqual expectedBytes
      }
    }
  }

  "fetchDirectDependencies" should {
    "for a versioned package should return the dependency bytes" in {
      val expectedBytes =
        """[
          |  {
          |    NAME: "java.lang",
          |    VERSION_CONSTRAINT: "8"
          |  },
          |]
          |""".stripMargin.getBytes("UTF-8")
      val pid     = PackageIdentifier(LanguagePlatform.JVM, "ion-java")
      val version = SemVer2("1.0.0")
      Using.resource(GitSparseFetcher()) { fetcher =>
        val directDepBytes = Await.result(fetcher.fetchDirectDependencies(pid, version), testDefaultTimeout)
        directDepBytes shouldEqual expectedBytes
      }
    }
  }
}
