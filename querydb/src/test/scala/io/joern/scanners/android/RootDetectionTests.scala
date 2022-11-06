package io.joern.scanners.android

import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.suites.{KotlinQueryTestSuite}
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._

class RootDetectionTests extends KotlinQueryTestSuite {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)

  override def queryBundle = RootDetection

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
