package io.joern.scanners.android

import io.joern.suites.{JavaQueryTestSuite}

class JavaScriptInterfaceTests extends JavaQueryTestSuite(JavaScriptInterface) {
  "the `insecureLoadUrlToExec` query" when {
    "should match on all multi-file positive examples" in {
      val q = queryBundle.insecureLoadUrlToExec()
      q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingCalls(cpg, q).size shouldBe 1
      }
    }
  }

  "the `insecureLoadUrlToExec` query" when {
    "should not match on all multi-file negative examples" in {
      val q = queryBundle.insecureLoadUrlToExec()
      q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingCalls(cpg, q).size shouldBe 0
      }
    }
  }
}
