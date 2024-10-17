package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.JVM
import io.shiftleft.semanticcpg.typeinfo.PackageIdentifier
import io.shiftleft.semanticcpg.typeinfo.version.{SemVer2, Version}
import io.shiftleft.semanticcpg.typeinfo.dependencies.MavenDependencyResolver
import io.shiftleft.semanticcpg.typeinfo.fetching.GitSparseFetcher
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Using

class MavenDependencyResolverTests extends AnyWordSpec with Matchers {
  "resolution" should {
    "get all expected dependencies for test data in test repo" in {
      Using.resource(GitSparseFetcher()) { fetcher =>
        val amazonIonPid       = PackageIdentifier(JVM, "ion-java")
        val version            = SemVer2("1.11.9")
        val dependenciesFuture = MavenDependencyResolver.resolveDependencies(fetcher, amazonIonPid, version)
        val dependencies       = Await.result(dependenciesFuture, 60.seconds)
        val expectedDependencies =
          List(TransitiveDependency(amazonIonPid.name, version), TransitiveDependency("java.lang", SemVer2("8.0.0")))
        dependencies shouldEqual expectedDependencies
      }
    }
  }
}
