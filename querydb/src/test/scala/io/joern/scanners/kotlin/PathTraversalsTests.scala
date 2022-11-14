package io.joern.scanners.kotlin

import io.joern.suites.KotlinQueryTestSuite

class PathTraversalsTests extends KotlinQueryTestSuite(PathTraversals) {

  "should match on all multi-file positive examples" in {
    val q = queryBundle.unzipDirectoryTraversal()
    q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { snippets =>
      val cpg = cpgForSnippets(snippets)
      findMatchingCalls(cpg, q).size shouldBe 1
    }
  }

  "should not match on all multi-file negative examples" in {
    val q = queryBundle.unzipDirectoryTraversal()
    q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { snippets =>
      val cpg = cpgForSnippets(snippets)
      findMatchingCalls(cpg, q).size shouldBe 0
    }
  }
}
