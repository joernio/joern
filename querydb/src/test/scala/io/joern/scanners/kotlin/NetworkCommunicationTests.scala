package io.joern.scanners.kotlin

import io.joern.suites.KotlinQueryTestSuite

class NetworkCommunicationTests extends KotlinQueryTestSuite(NetworkCommunication) {

  "should match on all multi-file positive examples" in {
    val q = queryBundle.nopTrustManagerUsed()
    q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { snippets =>
      val cpg = cpgForSnippets(snippets)
      findMatchingCalls(cpg, q).size shouldBe 1
    }
  }

  "should not match on all multi-file negative examples" in {
    val q = queryBundle.nopTrustManagerUsed()
    q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { snippets =>
      val cpg = cpgForSnippets(snippets)
      findMatchingCalls(cpg, q).size shouldBe 0
    }
  }
}
