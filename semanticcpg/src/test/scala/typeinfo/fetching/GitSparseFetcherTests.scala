package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GitSparseFetcherTests extends AnyWordSpec with Matchers {
  "getVersions" should {
    "return all versions for a package" in {
      val fetcher = GitSparseFetcher()
      val versions = fetcher.getVersions(PackageIdentifier(LanguageFrontend.Java, "ion-java"))
      
      versions.isSuccess shouldEqual true
      versions.get.sorted shouldEqual List("v1.0.0", "v1.11.9").sorted
    }
  }
}
