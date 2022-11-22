package io.joern.scanners.android

import io.joern.suites.{JavaQueryTestSuite}

class ArbitraryFileWritesJavaTests extends JavaQueryTestSuite(ArbitraryFileWrites) {
  "the `broadcastToFileWrite` query" when {
    "should match on all multi-file positive examples" in {
      val q = queryBundle.broadcastToFileWrite()
      q.multiFileCodeExamples.positive
        .filter(_.nonEmpty)
        .filter { snippets =>
          snippets.filterNot(_.filename.endsWith(".xml")).forall(_.filename.endsWith(".java"))
        }
        .foreach { snippets =>
          val cpg = cpgForSnippets(snippets)
          findMatchingCalls(cpg, q).size shouldBe 1
        }
    }
  }

  "the `broadcastToFileWrite` query" when {
    "should not match on all multi-file negative examples" in {
      val q = queryBundle.broadcastToFileWrite()
      q.multiFileCodeExamples.negative
        .filter(_.nonEmpty)
        .filter { snippets =>
          snippets.filterNot(_.filename.endsWith(".xml")).forall(_.filename.endsWith(".java"))
        }
        .foreach { snippets =>
          val cpg = cpgForSnippets(snippets)
          findMatchingCalls(cpg, q).size shouldBe 0
        }
    }
  }
}
