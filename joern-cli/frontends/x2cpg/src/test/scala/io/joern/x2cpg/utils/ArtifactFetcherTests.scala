package io.joern.x2cpg.utils

import io.joern.x2cpg.utils.ArtifactFetcherTests.artifactFetcherTest
import io.joern.x2cpg.utils.RetryableAssertion.eventually
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.IOException
import java.nio.file.{Files, Path}

class ArtifactFetcherTests extends AnyWordSpec with Matchers {

  // An arbitrary small artifact
  private val testArtifact = HttpArtifact(
    url = "https://repo1.maven.org/maven2/javax/inject/javax.inject/1/javax.inject-1.jar",
    sha256 = "91c77044a50c481636c32d916fd89c9118a72195390452c81065080f957de7ff"
  )

  "ArtifactFetcher" should {

    "download and cache a public artifact" taggedAs NetworkTest in {
      artifactFetcherTest { tmpDir =>
        val fetcher = ArtifactFetcher(tmpDir)
        val result  = fetcher.fetch(testArtifact)
        result shouldBe defined
        val path = result.get
        Files.exists(path) shouldBe true
        path.getFileName.toString shouldBe "javax.inject-1.jar"
        HashUtil.sha256(path) shouldBe testArtifact.sha256
      }
    }

    "return the cached path on second fetch" taggedAs NetworkTest in {
      artifactFetcherTest { tmpDir =>
        val fetcher = ArtifactFetcher(tmpDir)
        val path1   = fetcher.fetch(testArtifact)
        val path2   = fetcher.fetch(testArtifact)
        path1 shouldBe path2
      }
    }

    "return None on incorrect hash" taggedAs NetworkTest in {
      artifactFetcherTest { tmpDir =>
        val fetcher     = ArtifactFetcher(tmpDir)
        val badArtifact = testArtifact.copy(sha256 = "0000000000000000000000000000000000000000000000000000000000000000")
        fetcher.fetch(badArtifact) shouldBe None
      }
    }
  }

  "Credentials" should {

    "parse a credentials file" in {
      FileUtil.usingTemporaryFile("test-credentials", ".properties") { tmpFile =>
        Files.writeString(
          tmpFile,
          """realm=Example Realm
            |host=repo.example.com
            |user=admin
            |password=secret123
            |""".stripMargin
        )
        val creds = Credentials.load(tmpFile)
        creds should have size 1
        creds.head.user shouldBe "admin"
        creds.head.password shouldBe "secret123"
      }
    }

    "return empty for nonexistent file" in {
      val creds = Credentials.load(Path.of("/nonexistent/path/.credentials"))
      creds shouldBe empty
    }

    "return None from env when env vars are not set" in {
      // JOERN_SBT_CREDENTIALS_HOSTNAME etc. are not expected to be set during unit tests
      Credentials.fromEnv() shouldBe None
    }
  }
}

object ArtifactFetcherTests {
  def artifactFetcherTest(test: Path => Unit): Unit = {
    eventually[Unit, IOException] {
      FileUtil.usingTemporaryDirectory("artifact-fetcher-test-")(test(_))
    }
  }
}
