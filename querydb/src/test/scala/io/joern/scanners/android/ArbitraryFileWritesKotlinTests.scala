package io.joern.scanners.android

import io.joern.suites.{KotlinQueryTestSuite}

class ArbitraryFileWritesKotlinTests extends KotlinQueryTestSuite(ArbitraryFileWrites) {
  "the `broadcastToFileWrite` query" when {
    "should match on all multi-file positive examples" in {
      val q = queryBundle.broadcastToFileWrite()
      q.multiFileCodeExamples.positive
        .filter(_.nonEmpty)
        .filter { snippets =>
          snippets.filterNot(_.filename.endsWith(".xml")).forall(_.filename.endsWith(".kt"))
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
          snippets.filterNot(_.filename.endsWith(".xml")).forall(_.filename.endsWith(".kt"))
        }
        .foreach { snippets =>
          val cpg = cpgForSnippets(snippets)
          findMatchingCalls(cpg, q).size shouldBe 0
        }
    }
  }
}
