package io.joern.scanners.android

import io.joern.suites.{JavaQueryTestSuite}

class ArbitraryFileWritesTests extends JavaQueryTestSuite(ArbitraryFileWrites) {
  "the `broadcastToFileWrite` query" when {
    "should match on all multi-file positive examples" in {
      val q = queryBundle.broadcastToFileWrite()
      q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingCalls(cpg, q).size shouldBe 1
      }
    }
  }

  "the `broadcastToFileWrite` query" when {
    "should not match on all multi-file negative examples" in {
      val q = queryBundle.broadcastToFileWrite()
      q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingCalls(cpg, q).size shouldBe 0
      }
    }
  }
}
