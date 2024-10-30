package io.shiftleft.semanticcpg.typeinfo.dependencies

import io.shiftleft.semanticcpg.typeinfo.LanguagePlatform.JVM
import io.shiftleft.semanticcpg.typeinfo.PackageIdentifier
import io.shiftleft.semanticcpg.typeinfo.version.{SemVer2, Version}
import io.shiftleft.semanticcpg.typeinfo.dependencies.MavenDependencyResolver
import io.shiftleft.semanticcpg.typeinfo.fetching.{GitSparseFetcher, TestMockFetcher}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Using

class MavenDependencyResolverTests extends AnyWordSpec with Matchers {
  private val mockFetcherTimeOut = 10.seconds
  private val gitSparseFetcherTimeOut = 60.seconds
  
  "resolution" should {
    "handle simple constraints" in {
      val mockFetcher = TestMockFetcher()
        .addFiles(
          ("jvm/package1/metadata/metadata.ion", 
            """{ VERSIONS: ["1.0.0", "1.0.0-rc2", "2.0.0"]}"""),
          ("jvm/package2/metadata/metadata.ion", 
            """{ VERSIONS: ["1.0.0"]}"""),
          ("jvm/package2/dependencies/1.0.0/direct.ion", 
            """[ { NAME: "package1", VERSION_CONSTRAINT: "<2.0.0 && >1.0.0-rc2" }]"""),
          ("jvm/package1/dependencies/2.0.0/direct.ion", 
            """[]"""),
          ("jvm/package1/dependencies/1.0.0-rc2/direct.ion",
            """[]"""),
          ("jvm/package1/dependencies/1.0.0/direct.ion",
            """[]""")
        )
      val topLevelPackage = PackageIdentifier(JVM, "package2")
      val topLevelPackageVersion = SemVer2("1.0.0")

      val resolver           = MavenDependencyResolver(mockFetcher)
      val dependenciesFuture = resolver.resolveDependencies(
        topLevelPackage, 
        topLevelPackageVersion)
      val dependencies = Await.result(dependenciesFuture, mockFetcherTimeOut)
      
      val expectedDependencies = List(
        TransitiveDependency("package2", topLevelPackageVersion),
        TransitiveDependency("package1", SemVer2("1.0.0"))
      )
      
      dependencies should contain theSameElementsAs expectedDependencies
    }
    
    "get all expected dependencies for test data in test repo" in {
      Using.resource(GitSparseFetcher()) { fetcher =>
        val amazonIonPid       = PackageIdentifier(JVM, "ion-java")
        val version            = SemVer2("1.11.9")
        val resolver           = MavenDependencyResolver(fetcher)
        val dependenciesFuture = resolver.resolveDependencies(amazonIonPid, version)
        val dependencies       = Await.result(dependenciesFuture, gitSparseFetcherTimeOut)
        val expectedDependencies =
          List(TransitiveDependency(amazonIonPid.name, version), TransitiveDependency("java.lang", SemVer2("8.0.0")))
        dependencies should contain theSameElementsAs expectedDependencies
      }
    }
  }
}
