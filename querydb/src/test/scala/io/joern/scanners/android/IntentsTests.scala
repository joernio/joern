package io.joern.scanners.android

import io.joern.suites.{JavaQueryTestSuite, KotlinQueryTestSuite}
import io.shiftleft.semanticcpg.language.toNodeTypeStarters

class IntentsTests extends JavaQueryTestSuite(Intents) {

  "the `intentToRuntimeExec` query" when {
    "should match on all multi-file positive examples" in {
      val q = queryBundle.intentToRuntimeExec()
      q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingCalls(cpg, q).size shouldBe 1
      }
    }

    "should not match on all multi-file negative examples" in {
      val q = queryBundle.intentToRuntimeExec()
      q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingCalls(cpg, q).size shouldBe 0
      }
    }
  }
}
