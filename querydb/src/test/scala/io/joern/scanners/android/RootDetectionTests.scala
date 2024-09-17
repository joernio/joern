package io.joern.scanners.android

import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.NoSemantics
import io.joern.suites.KotlinQueryTestSuite

class RootDetectionTests extends KotlinQueryTestSuite(RootDetection) {
  implicit val engineContext: EngineContext = EngineContext(NoSemantics)

  "the `rootDetectionViaFileChecks` query" when {
    "should match on all multi-file positive examples" in {
      val q = queryBundle.rootDetectionViaFileChecks()
      q.multiFileCodeExamples.positive.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingMethods(cpg, q).size shouldBe 1
      }
    }

    "should not match on all multi-file negative examples" in {
      val q = queryBundle.rootDetectionViaFileChecks()
      q.multiFileCodeExamples.negative.filter(_.nonEmpty).foreach { snippets =>
        val cpg = cpgForSnippets(snippets)
        findMatchingMethods(cpg, q).size shouldBe 0
      }
    }
  }
}
